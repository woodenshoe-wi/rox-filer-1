/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* filer.c - code for handling filer windows */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/param.h>
#include <fnmatch.h>
#include <regex.h>

#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "filer.h"
#include "display.h"
#include "main.h"
#include "fscache.h"
#include "support.h"
#include "gui_support.h"
#include "choices.h"
#include "pixmaps.h"
#include "menu.h"
#include "dnd.h"
#include "dir.h"
#include "diritem.h"
#include "run.h"
#include "type.h"
#include "options.h"
#include "minibuffer.h"
#include "icon.h"
#include "toolbar.h"
#include "bind.h"
#include "appinfo.h"
#include "mount.h"
#include "xml.h"
#include "view_iface.h"
#include "collection.h"
#include "view_collection.h"
#include "view_details.h"
#include "action.h"
#include "bookmarks.h"
#include "xtypes.h"
#include "usericons.h"

static XMLwrapper *groups = NULL;

/* Item we are about to display a tooltip for */
static DirItem *tip_item = NULL;

/* The window which the motion event for the tooltip came from. Use this
 * to get the correct widget for finding the item under the pointer.
 */
static GdkWindow *motion_window = NULL;

/* This is rather badly named. It's actually the filer window which received
 * the last key press or Menu click event.
 */
FilerWindow 	*window_with_focus = NULL;

GList		*all_filer_windows = NULL;

gint fw_font_height;
gint fw_font_widths[0x7f];
gint fw_font_widthsb[0x7f];
gint fw_mono_width;
gint fw_mono_height;
static PangoFontDescription *current_font = NULL;

static GHashTable *window_with_id = NULL;

static FilerWindow *window_with_primary = NULL;

static GHashTable *settings_table=NULL;

typedef struct {
	gchar *path;

	guint flags;   /* Which parts are valid, see below */

	gint x, y;
	gint width, height;
	gboolean show_hidden;
	ViewType view_type;
	SortType	sort_type;
	GtkSortType	sort_order;
	gboolean        show_thumbs;

	DetailsType	details_type;
	DisplayStyle	display_style;

	FilterType      filter_type;
	char           *filter;
	gboolean        filter_directories;
} Settings;

enum settings_flags{
	SET_POSITION=1,   /* x, y */
	SET_SIZE=2,       /* width, height */
	SET_HIDDEN=4,     /* show_hidden */
	SET_STYLE=8,      /* display_style */
	SET_SORT=16,      /* sort_type, sort_order */
	SET_DETAILS=32,   /* view_type, details_type */
	SET_THUMBS=64,    /* show_thumbs */
	SET_FILTER=128,   /* filter_type, filter */
};

static GHashTable *unmount_prompt_actions = NULL;

typedef struct {
	FilerWindow *fw;
	gchar *path;
	gchar *thumb_path;
	GPtrArray *items;
	int tried;
	gboolean cancel;
} SubDirInfo;

static SubDirInfo sdinfo = {NULL, NULL, NULL, NULL, 0, TRUE};
static GMutex m_dirthumb;

/* Static prototypes */
static void attach(FilerWindow *filer_window);
static void detach(FilerWindow *filer_window);
static void filer_window_destroyed(GtkWidget    *widget,
				   FilerWindow	*filer_window);
static void update_display(Directory *dir,
		DirAction action, void *items, FilerWindow *filer_window);
static void set_scanning_display(FilerWindow *filer_window, gboolean scanning);
static gboolean may_rescan(FilerWindow *filer_window, gboolean warning);
static gboolean minibuffer_show_cb(FilerWindow *filer_window);
static void filer_add_widgets(FilerWindow *filer_window, const gchar *wm_class);
static void filer_add_signals(FilerWindow *filer_window);

static void set_selection_state(FilerWindow *filer_window, gboolean normal);
static void filer_next_thumb(GObject *window, const gchar *path);
static void start_thumb_scanning(FilerWindow *filer_window);
static void filer_options_changed(void);
static void drag_end(GtkWidget *widget, GdkDragContext *context,
		     FilerWindow *filer_window);
static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window);
static gboolean drag_motion(GtkWidget		*widget,
                            GdkDragContext	*context,
                            gint		x,
                            gint		y,
                            guint		time,
			    FilerWindow		*filer_window);

static void load_learnt_mounts(void);
static void save_learnt_mounts(void);
static void load_settings(void);
static void save_settings(void);
static gboolean check_settings(FilerWindow *filer_window, gboolean onlycheck);
static char *tip_from_desktop_file(const char *full_path);

static void free_subdir_info(void);

GdkCursor *busy_cursor = NULL;
static GdkCursor *crosshair = NULL;
static GdkCursor *hand_cursor = NULL;
static GdkCursor *blank_cursor = NULL;

/* Indicates whether the filer's display is different to the machine it
 * is actually running on.
 */
static gboolean not_local = FALSE;

static Option o_short_flag_names;
static Option o_filer_view_type;
Option o_filer_auto_resize, o_unique_filer_windows;
Option o_filer_size_limit;
Option o_filer_width_limit;
Option o_fast_font_calc;
static Option o_right_gap, o_bottom_gap, o_auto_move;
static Option o_disable_pointer_warp;
Option o_window_link;
Option o_scroll_speed;
static Option o_hide_root_msg;

#define ROX_RESPONSE_EJECT 99 /**< User clicked on Eject button */

static gpointer checklocal(gpointer p)
{
	/* Is the display on the local machine, or are we being
	 * run remotely? See filer_set_title().
	 */
	const gchar *ohost   = our_host_name(TRUE);
	      gchar *dpyhost = g_strdup(gdk_get_display());
	gchar *sep = strchr(dpyhost, ':');
	if (sep) *sep = '\0';

	if (dpyhost[0] && strcmp(ohost, dpyhost) != 0)
	{
		/* Try the cannonical name for dpyhost (see our_host_name()
		 * in support.c).
		 */
		struct hostent *ent = gethostbyname(dpyhost);
		if (!ent || strcmp(ohost, ent->h_name) != 0)
			not_local = TRUE;
	}
	g_free(dpyhost);

	return NULL;
}

void filer_init(void)
{
	option_add_int(&o_filer_size_limit, "filer_size_limit", 60);
	option_add_int(&o_filer_width_limit, "filer_width_limit", 0);
	option_add_int(&o_filer_auto_resize, "filer_auto_resize", RESIZE_ALWAYS);

	option_add_int(&o_unique_filer_windows, "filer_unique_windows", 1);
	option_add_int(&o_short_flag_names, "filer_short_flag_names", TRUE);
	option_add_int(&o_filer_view_type, "filer_view_type", VIEW_TYPE_COLLECTION);
	option_add_int(&o_window_link, "window_link", 1);
	option_add_int(&o_scroll_speed, "scroll_speed", 6);
	option_add_int(&o_hide_root_msg, "hide_root_msg", FALSE);

	option_add_int(&o_right_gap, "right_gap", 0);
	option_add_int(&o_bottom_gap, "bottom_gap", 0);
	option_add_int(&o_auto_move, "auto_move", TRUE);
	option_add_int(&o_disable_pointer_warp, "disable_pointer_warp", FALSE);
	option_add_int(&o_fast_font_calc, "fast_font_calc", TRUE);

	option_add_notify(filer_options_changed);

	busy_cursor = gdk_cursor_new(GDK_WATCH);
	crosshair = gdk_cursor_new(GDK_CROSSHAIR);
	hand_cursor = gdk_cursor_new(GDK_HAND2);
	blank_cursor = gdk_cursor_new(GDK_BLANK_CURSOR);


	window_with_id = g_hash_table_new_full(g_str_hash, g_str_equal,
					       NULL, NULL);

	g_thread_new(NULL, checklocal, NULL);

	load_settings();
	load_learnt_mounts();
}

static gboolean if_deleted(gpointer item, gpointer removed)
{
	return NULL != g_hash_table_lookup((GHashTable *) removed,
			((DirItem *)item)->leafname);
}

/* Resize the filer window to w x h pixels, plus border (not clamped).
 * If triggered by a key event, warp the pointer (for SloppyFocus users).
 */
void filer_window_set_size(FilerWindow *filer_window, int w, int h, gboolean notauto)
{
	g_return_if_fail(filer_window != NULL);

	Settings *set = (Settings *) g_hash_table_lookup(
			settings_table, filer_window->sym_path);
	GtkWidget *window = filer_window->window;
	GdkWindow *gdk_window = window->window;
	if (!gdk_window) return;
	GtkRequisition *req = &window->requisition;
	gint dx = 0, dy = 0, px = 0, py = 0;

	if (filer_window->req_width >= 0)
	{
		w = filer_window->req_width;
		h = filer_window->req_height;
		filer_window->req_width = filer_window->req_height = -1;
	}
	else if (set && set->flags & SET_SIZE)
	{
		w = set->width;
		h = set->height;
	}
	else
	{
		if (filer_window->scrollbar)
			w += filer_window->scrollbar->allocation.width;

		if (o_toolbar.int_value != TOOLBAR_NONE)
			h += filer_window->toolbar->allocation.height;
		if (gtk_widget_get_visible(filer_window->message))
			h += filer_window->message->allocation.height;
	}

	w = MAX(req->width, w);
	h = MAX(req->height, h);

	if (o_auto_move.int_value || filer_window->reqx >= 0)
	{
		gint x, y, m, currentw, currenth, bcw, bch, lx, ly, mxend, myend;
		GdkRectangle frect = {0, 0, 0, 0};

		gdk_window_get_pointer(NULL, &x, &y, NULL);
		m = gdk_screen_get_monitor_at_point(gdk_screen_get_default(), x, y);

		gdk_window_get_geometry(gdk_window, NULL, NULL, &bcw, &bch, NULL);
		gdk_window_get_frame_extents(gdk_window, &frect);
		gdk_window_get_geometry(gdk_window, NULL, NULL, &currentw, &currenth, NULL);
		/* If delay happens between bc and current then get after. */
		if (bcw != currentw || bch != currenth)
			gdk_window_get_frame_extents(gdk_window, &frect);

		mxend = monitor_geom[m].x + monitor_geom[m].width;
		myend = monitor_geom[m].y + monitor_geom[m].height;

		if (frect.x + frect.width > mxend ||
			frect.y + frect.height > myend)
		{
			lx = mxend;
			ly = myend;
		}
		else
		{
			lx = mxend - (frect.width - currentw + w + o_right_gap.int_value);
			ly = myend - (frect.height - currenth + h + o_bottom_gap.int_value);
		}

/* There are todo, so these g_print are still there */
//g_print("reqx:%i, reqy:%i\n", filer_window->reqx, filer_window->reqy);

		if (filer_window->reqx >= 0)
		{
			if (gtk_widget_get_visible(window))
			{ /* If visible there are window decorations */
				gtk_window_get_position(GTK_WINDOW(window),&x, &y);
				x = filer_window->reqx + x - frect.x;
				y = filer_window->reqy + y - frect.y;
			}
			else
			{ /* todo: There is no clue to get the size of decorations */
				lx -= 0;
				ly -= 0;
				x = filer_window->reqx;
				y = filer_window->reqy;
			}
			filer_window->reqx = filer_window->reqy = -1;
		}
		else
		{
			x = frect.x;
			y = frect.y;
		}

		x = MAX(MIN(x, lx), 0);
		y = MAX(MIN(y, ly), 0);

//g_print("x:%i, y:%i, fx:%i, fy:%i, lx:%i, ly:%i\n", x, y, frect.x, frect.y, lx, ly);
//g_print("cw:%i, ch:%i, w:%i, h:%i, fw:%i, fh:%i\n", currentw, currenth, w, h, frect.width, frect.height);

		dx = x - frect.x;
		dy = y - frect.y;

		if (dx != 0 || dy != 0)
		{
			if (gtk_window_is_active(GTK_WINDOW(window)) && !o_disable_pointer_warp.int_value)
			{
				gdk_window_get_pointer(gdk_window, &px, &py, NULL);
				XWarpPointer(gdk_x11_drawable_get_xdisplay(gdk_window),
						None,
						gdk_x11_drawable_get_xid(gdk_window),
						0, 0, 0, 0,
						px + dx, py + dy);
			}

			/* In start up, there is no GDK_window. Have to use GTK. */
			gtk_window_move(GTK_WINDOW(window), x, y);
			filer_window->configured = 0;
		}
		if (
				w != currentw ||
				h != currenth ||
				w != filer_window->last_width ||
				h != filer_window->last_height)
		{
			gtk_window_resize(GTK_WINDOW(window), w, h);
			filer_window->configured = 0;
		}
	}
	else
	{
		gtk_window_resize(GTK_WINDOW(window), w, h);
		filer_window->configured = 0;
	}

	if (notauto)
		filer_window->last_width = filer_window->last_height = -1;
	else
	{
		filer_window->last_width = w;
		filer_window->last_height = h;
	}

	if (gtk_window_is_active(GTK_WINDOW(window)) && !o_disable_pointer_warp.int_value)
	{
		GdkEvent *event = gtk_get_current_event();
		if (event)
		{
			int nx, ny;

			if (dx == 0 && dy == 0)
				gdk_window_get_pointer(gdk_window,
						&px, &py, NULL);

			if (filer_window->scrollbar)
				w -= filer_window->scrollbar->allocation.width;

			nx = CLAMP(px, 2, w - 2);
			ny = CLAMP(py, 2, h - 2);

			if (nx != px || ny != py)
			{
				XWarpPointer(
						gdk_x11_drawable_get_xdisplay(gdk_window),
						None,
						gdk_x11_drawable_get_xid(gdk_window),
						0, 0, 0, 0,
						nx + dx, ny + dy);
			}

		 	gdk_event_free(event);
		}
	}
}

static gboolean acceptfocuscb(FilerWindow *fw)
{
	if (filer_exists(fw))
		gtk_window_set_accept_focus(GTK_WINDOW(fw->window), TRUE);
	fw->accept_timeout = 0;
	return FALSE;
}
void filer_link(FilerWindow *left, FilerWindow *right)
{
	GdkRectangle frect = {0, 0, 0, 0};
	gdk_window_get_frame_extents(left->window->window, &frect);
	gtk_window_move(GTK_WINDOW(right->window),
			frect.x + frect.width,
			frect.y);

	gtk_window_set_accept_focus(GTK_WINDOW(right->window), FALSE);
	gdk_window_raise(right->window->window);
	if (right->accept_timeout)
		g_source_remove(right->accept_timeout);
	right->accept_timeout =
		g_timeout_add(200, (GSourceFunc)acceptfocuscb, right);
}

/* Look through all items we want to display, and queue a recheck on any
 * that require it.
 */
static void queue_interesting(FilerWindow *filer_window)
{
	DirItem	*item;
	ViewIter iter;

	view_get_iter(filer_window->view, &iter, 0);
	while ((item = iter.next(&iter)))
	{
		if (item->flags & ITEM_FLAG_NEED_RESCAN_QUEUE)
			dir_queue_recheck(filer_window->directory, item);
	}
}

static gboolean _set_pointer(void *vp)
{
	FilerWindow *fw = (FilerWindow *) vp;
	ViewIter iter;
	gint x, y;
	DirItem *item = NULL;

	fw->pointer_idle = 0;
	if (!g_list_find(all_filer_windows, fw)) return FALSE;//destroyed

	GdkWindow *gwin = gdk_window_get_pointer(fw->window->window, NULL, NULL, NULL);

	if (GTK_WIDGET(fw->view)->window == gwin)
	{
		gdk_window_get_pointer(gwin, &x, &y, NULL);

		view_get_iter_at_point(fw->view, &iter, gwin, x, y);
		item = iter.peek(&iter);
	}

	gdk_window_set_cursor(fw->window->window,
			item ? hand_cursor : NULL);

	return FALSE;
}
static void filer_set_pointer(FilerWindow *fw)
{
	if (!fw->pointer_idle)
		fw->pointer_idle = g_idle_add(_set_pointer, fw);
}

void filer_autosize(FilerWindow *fw) {

	if (!fw->directory) return;

	if (fw->may_resize)
		view_style_changed(fw->view, 0);
	fw->may_resize = FALSE;

	if (o_filer_auto_resize.int_value != RESIZE_ALWAYS &&
			!fw->new_win_first_scan)
		return;

	gint w,h;
	gtk_window_get_size(GTK_WINDOW(fw->window), &w, &h);
	if (!fw->configured ||
		(w == fw->last_width && h == fw->last_height))
		view_autosize(fw->view, FALSE);
}

static void set_icon(FilerWindow *fw, GdkPixbuf *src)
{
	GdkPixbuf *lined = fw->dir_colour ?
		pixmap_make_lined(src, fw->dir_colour) : NULL;

	gtk_window_set_icon(GTK_WINDOW(fw->window), lined ?: src);

	if (lined)
		g_object_unref(lined);
}
static void update_display(Directory *dir,
		DirAction action, void *items, FilerWindow *filer_window)
{
	ViewIface *view = (ViewIface *) filer_window->view;
	gboolean init = filer_window->under_init;
	FilerWindow *fw = filer_window;

	//g_print("[a%d]%s", action, action == 1 ? "\n" : "");
	//static gint64 start_time;

	switch (action)
	{
		case DIR_ADD:
			//start_time = g_get_real_time();

			view_add_items(view, items);
			filer_window->req_sort = FALSE;

			if (!filer_window->first_scan)
				filer_create_thumbs(filer_window, items);

			if (!init)
				filer_window->may_resize = TRUE;
			break;
		case DIR_REMOVE:
			view_delete_if(view, if_deleted, items);
			toolbar_update_info(filer_window);

			if (!init)
				filer_window->may_resize = TRUE;
			break;
		case DIR_START_SCAN:
			set_scanning_display(filer_window, TRUE);
			toolbar_update_info(filer_window);
			break;
		case DIR_END_SCAN:
			//g_print("end time : %li\n", g_get_real_time() - start_time);

			if (filer_window->req_sort)
			{
				filer_window->req_sort = FALSE;
				view_sort(view);
				gtk_widget_queue_draw(GTK_WIDGET(view));
			}

			GdkColor *tmpc = xlabel_get(fw->sym_path);
			if ((!tmpc) != (!fw->dir_colour) || (tmpc && fw->dir_colour
					&& !gdk_color_equal(tmpc, fw->dir_colour)))
				gtk_widget_queue_draw(GTK_WIDGET(view));
			g_free(fw->dir_colour);
			fw->dir_colour = tmpc;

			MaskedPixmap
				*fi = get_globicon(filer_window->sym_path);
			if (!fi)
				fi = get_globicon(filer_window->real_path);
			if (!fi)
				fi = g_fscache_lookup_full(pixmap_cache,
						make_path(filer_window->real_path, ".DirIcon"),
						FSCACHE_LOOKUP_ONLY_NEW, NULL);

			set_icon(filer_window, fi ? fi->src_pixbuf : NULL);

			if (fi)
				g_object_unref(fi);
			filer_window->dir_icon = fi; //just a flag

			set_scanning_display(filer_window, FALSE);
			toolbar_update_info(filer_window);

			if (filer_window->auto_select)
				display_set_autoselect(filer_window,
						filer_window->auto_select);
			null_g_free(&filer_window->auto_select);

			if (!init && filer_window->may_resize)
				filer_autosize(filer_window);

			if (filer_window->first_scan)
			{
				filer_create_thumbs(filer_window, NULL);
				filer_set_pointer(filer_window);
			}

			filer_window->may_resize = FALSE;
			filer_window->first_scan = FALSE;
			filer_window->new_win_first_scan = FALSE;
			break;
		case DIR_UPDATE_EXA:
			init = TRUE;
		case DIR_UPDATE:
			if ((filer_window->sort_type != SORT_NAME ||
						o_display_dirs_first.int_value) &&
					filer_window->view_type == VIEW_TYPE_COLLECTION)
			{
				if (!filer_window->first_scan)
					view_sort(view);
				else
					filer_window->req_sort = TRUE;
			}

			view_update_items(view, items);

			if (init) break;

			if (!filer_window->dir_icon)
			{
				filer_window->dir_icon = g_fscache_lookup_full(
						pixmap_cache,
						make_path(filer_window->real_path, ".DirIcon"),
						FSCACHE_LOOKUP_ONLY_NEW, NULL);

				if (filer_window->dir_icon) {
					set_icon(filer_window, filer_window->dir_icon->src_pixbuf);
					g_object_unref(filer_window->dir_icon);
				}
			}

			if (filer_window->view_type != VIEW_TYPE_COLLECTION)
				filer_window->may_resize = TRUE;

			if (!filer_window->first_scan)
			{
				filer_create_thumbs(filer_window, items);
				filer_set_pointer(filer_window);
			}

			break;
		case DIR_UPDATE_ICON:
			filer_window->onlyicon=TRUE;
			view_update_items(view, items);
			filer_window->onlyicon=FALSE;
			filer_set_pointer(filer_window);
			break;
		case DIR_ERROR_CHANGED:
			filer_set_title(filer_window);
			gtk_widget_queue_draw(GTK_WIDGET(view));
			break;
		case DIR_QUEUE_INTERESTING:
			queue_interesting(filer_window);
			break;
	}
}

static void attach(FilerWindow *filer_window)
{
	gdk_window_set_cursor(filer_window->window->window, busy_cursor);
	gdk_flush();

	view_clear(filer_window->view);
	filer_window->scanning = TRUE;

	if (filer_window->sort_type != SORT_NAME || o_display_dirs_first.int_value)
		filer_window->directory->notify_time = DIR_NOTIFY_TIME_FOR_SORT_DATA;
	else
		filer_window->directory->notify_time = 0;

	dir_attach(filer_window->directory, (DirCallback) update_display,
			filer_window);

	filer_set_title(filer_window);
	bookmarks_add_history(filer_window->sym_path);

	if (filer_window->directory->error)
	{
		if (spring_in_progress)
			g_printerr(_("Error scanning '%s':\n%s\n"),
				filer_window->sym_path,
				filer_window->directory->error);
	}

	if (!filer_window->scanning)
		filer_set_pointer(filer_window);
}

static void detach(FilerWindow *filer_window)
{
	g_return_if_fail(filer_window->directory != NULL);

	if (filer_window->mini_type == MINI_REG_SELECT)
		minibuffer_hide(filer_window);

	if (filer_window->view_type == VIEW_TYPE_DETAILS)
		//details selection is very heavy
		view_clear_selection(filer_window->view);

	dir_detach(filer_window->directory,
			(DirCallback) update_display, filer_window);
	g_object_unref(filer_window->directory);
	filer_window->directory = NULL;
}

/* If 'start' was mounted by ROX-Filer, return it. Otherwise, try the
 * parents up the tree.
 * NULL if we're not in a user mount point.
 * g_free() the result.
 */
static char *get_ancestor_user_mount_point(const char *start)
{
	char *path;

	path = strdup(start);

	while (1)
	{
		char *slash;

		if (mount_is_user_mounted(path))
			return path;

		if (!path[1])
		{
			g_free(path);
			return NULL;
		}

		slash = strrchr(path + 1, '/');
		if (!slash)
			slash = path + 1;
		*slash = '\0';
	}
}

static void unmount_dialog_response(GtkWidget *dialog,
        int response, char *mount)
{
	GList *list = NULL;
	UnmountPrompt prompt_val = UNMOUNT_PROMPT_ASK;

	switch (response)
	{
	case GTK_RESPONSE_OK:
		list = g_list_prepend(NULL, mount);
		action_mount(list, FALSE, FALSE, TRUE);
		prompt_val = UNMOUNT_PROMPT_UNMOUNT;
		break;

	case ROX_RESPONSE_EJECT:
		list = g_list_prepend(NULL, mount);
		action_eject(list);
		prompt_val = UNMOUNT_PROMPT_EJECT;
		break;

	default:
		prompt_val = UNMOUNT_PROMPT_NO_CHANGE;
		break;
	}
	if (list)
		g_list_free(list);

	if (gtk_toggle_button_get_active(g_object_get_data(G_OBJECT(dialog),
			"unmount_mem_btn")))
	{
	    filer_set_unmount_action(mount, prompt_val);
	}

	g_free(mount);

	gtk_widget_destroy(dialog);

	one_less_window();
}

/* 'filer_window' shows a directory under 'mount'. If no other window also
 * shows a directory under it, display a non-modal dialog offering to
 * unmount the directory.
 * 'mount' is freed by this function, either directly, or after the dialog
 * closes.
 */
static void may_offer_unmount(FilerWindow *filer_window, char *mount)
{
	GtkWidget *dialog, *button, *unmount_mem_btn;
	GList	*next;
	int len;

	len = strlen(mount);

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *other = (FilerWindow *) next->data;

		if (other == filer_window)
			continue;

		if (strncmp(filer_window->real_path, other->real_path,
			    len) != 0)
			continue;

		g_return_if_fail(
			filer_window->real_path[len] != '/' ||
			filer_window->real_path[len] != '\0');

		if (other->real_path[len] != '/' &&
		    other->real_path[len] != '\0')
			continue;

		/* Found another window. Don't offer to unmount. */
		g_free(mount);
		return;
	}

	if (unmount_prompt_actions)
	{
		GList *list = NULL;
		UnmountPrompt unmount_val = filer_get_unmount_action(mount);

		switch (unmount_val)
		{
		case UNMOUNT_PROMPT_UNMOUNT:
			list = g_list_prepend(NULL, mount);
			action_mount(list, FALSE, FALSE, TRUE);
			break;

		case UNMOUNT_PROMPT_EJECT:
			list = g_list_prepend(NULL, mount);
			action_eject(list);
			break;

		default:
			break;
		}
		if (list)
			g_list_free(list);
		if (unmount_val != UNMOUNT_PROMPT_ASK)
		{
			g_free(mount);
			return;
		}
	}

	dialog = gtk_message_dialog_new(NULL, 0, GTK_MESSAGE_QUESTION,
			GTK_BUTTONS_NONE,
			_("Do you want to unmount this device?\n\n"
			"Unmounting a device makes it safe to remove "
			"the disk."));

	unmount_mem_btn = gtk_check_button_new_with_label(
			_("Perform the same action in future for this mount point"));
	gtk_widget_show(unmount_mem_btn);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), unmount_mem_btn,
			FALSE, FALSE, 0);
	g_object_set_data(G_OBJECT(dialog), "unmount_mem_btn",
			unmount_mem_btn);

	button = button_new_mixed(ROX_STOCK_MOUNTED, _("No change"));
	gtk_widget_set_can_default(button, TRUE);
	gtk_dialog_add_action_widget(GTK_DIALOG(dialog), button,
					GTK_RESPONSE_CANCEL);
	gtk_widget_show(button);

	button = button_new_mixed(ROX_STOCK_MOUNT, _("Unmount"));
	gtk_widget_set_can_default(button, TRUE);
	gtk_dialog_add_action_widget(GTK_DIALOG(dialog), button,
					GTK_RESPONSE_OK);
	gtk_widget_show(button);

	/* We need a better icon, but I can't draw */
	button = button_new_mixed(GTK_STOCK_UNDO, _("Eject"));
	gtk_widget_set_can_default(button, TRUE);
	gtk_dialog_add_action_widget(GTK_DIALOG(dialog), button,
					ROX_RESPONSE_EJECT);
	gtk_widget_show(button);

	g_signal_connect(G_OBJECT(dialog), "response",
			G_CALLBACK(unmount_dialog_response), mount);

	gtk_dialog_set_default_response(GTK_DIALOG(dialog),
			GTK_RESPONSE_OK);

	number_of_windows++;
	gtk_widget_show(dialog);
}

/* Returns TRUE to prevent closing the window. May offer to unmount a
 * device.
 */
gboolean filer_window_delete(GtkWidget *window,
			     GdkEvent *unused, /* (may be NULL) */
			     FilerWindow *filer_window)
{
	char *mount;

	mount = get_ancestor_user_mount_point(filer_window->real_path);

	if (mount)
		may_offer_unmount(filer_window, mount);

	return FALSE;
}

void filer_cut_links(FilerWindow *fw, gint side)
{
	if (!fw) return;
	if (!fw->left_link && !fw->right_link) return;

	if (side != 1 && fw->left_link)
	{
		fw->left_link->right_link = NULL;
		filer_set_title(fw->left_link);
		fw->left_link = NULL;
	}

	if (side != -1 && fw->right_link)
	{
		gtk_widget_destroy(fw->right_link->window);
		fw->right_link = NULL;
	}

	if (side != -1 && fw->right_link_idle)
	{
		g_source_remove(fw->right_link_idle);
		fw->right_link_idle = 0;
	}

	filer_set_title(fw);
}


static void filer_window_destroyed(GtkWidget *widget, FilerWindow *filer_window)
{
	all_filer_windows = g_list_remove(all_filer_windows, filer_window);

	filer_cut_links(filer_window, 0);

	g_object_set_data(G_OBJECT(widget), "filer_window", NULL);

	if (window_with_primary == filer_window)
		window_with_primary = NULL;

	if (window_with_focus == filer_window)
	{
		menu_popdown();
		window_with_focus = NULL;
	}

	if (filer_window->directory)
		detach(filer_window);

	if (filer_window->auto_scroll != -1)
	{
		g_source_remove(filer_window->auto_scroll);
		filer_window->auto_scroll = -1;
	}

	g_mutex_lock(&m_dirthumb);
	g_mutex_unlock(&m_dirthumb);
	if (sdinfo.fw == filer_window)
		free_subdir_info();

	g_queue_free_full(filer_window->thumb_queue, g_free);

	tooltip_show(NULL);

	filer_set_id(filer_window, NULL);

	if(filer_window->filter_string)
		g_free(filer_window->filter_string);
	if(filer_window->regexp)
	{
		regfree((regex_t *) filer_window->regexp);
		g_free(filer_window->regexp);
		g_free(filer_window->temp_filter_string);
	}

	g_free(filer_window->dir_colour);
	g_free(filer_window->auto_select);
	g_free(filer_window->real_path);
	g_free(filer_window->sym_path);
	g_free(filer_window);

	one_less_window();
}

/* Returns TRUE iff the directory still exists. */
static gboolean may_rescan(FilerWindow *filer_window, gboolean warning)
{
	Directory *dir;

	g_return_val_if_fail(filer_window != NULL, FALSE);

	/* We do a fresh lookup (rather than update) because the inode may
	 * have changed.
	 */
	char *real = pathdup(filer_window->sym_path);
	dir = g_fscache_lookup(dir_cache, real);
	g_free(real);
	if (!dir)
	{
		filer_change_to(filer_window, filer_window->sym_path, NULL);
		return FALSE;
	}

	g_object_unref(dir);
	if (dir != filer_window->directory)
		filer_change_to(filer_window, filer_window->sym_path, NULL);

	return TRUE;
}

/* No items are now selected. This might be because another app claimed
 * the selection or because the user unselected all the items.
 */
void filer_lost_selection(FilerWindow *filer_window, guint time)
{
	if (window_with_primary == filer_window)
	{
		window_with_primary = NULL;
		gtk_selection_owner_set(NULL, GDK_SELECTION_PRIMARY, time);
	}
}

/* Another app has claimed the primary selection */
static void filer_lost_primary(GtkWidget *window,
		        GdkEventSelection *event,
		        gpointer user_data)
{
	FilerWindow *filer_window = (FilerWindow *) user_data;

	if (window_with_primary && window_with_primary == filer_window)
	{
		window_with_primary = NULL;
		set_selection_state(filer_window, FALSE);
	}
}

/* Someone wants us to send them the selection */
static void selection_get(GtkWidget *widget, 
		GtkSelectionData *selection_data,
		guint    info,
		guint    time,
		gpointer data)
{
	GString     *reply, *header;
	FilerWindow *filer_window = (FilerWindow *) data;
	ViewIter    iter;
	DirItem     *item;

	reply = g_string_new(NULL);
	header = g_string_new(NULL);

	view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);

	switch (info)
	{
		case TARGET_STRING:
			g_string_printf(header, " %s",
					make_path(filer_window->sym_path, ""));

			while ((item = iter.next(&iter)))
			{
				g_string_append(reply, header->str);
				g_string_append(reply, item->leafname);
			}

			break;
		case TARGET_URI_LIST:
			while ((item = iter.next(&iter)))
			{
				EscapedPath *uri = encode_path_as_uri(
						make_path(filer_window->sym_path, item->leafname));
				g_string_append_printf(reply, " %s", (char *) uri);
				g_free(uri);
			}

			break;
	}

	if (reply->len > 0)
		gtk_selection_data_set_text(selection_data,
				reply->str + 1, reply->len - 1);
	else
	{
		g_warning("Attempt to paste empty selection!");
		gtk_selection_data_set_text(selection_data, "", 0);
	}

	g_string_free(reply, TRUE);
	g_string_free(header, TRUE);
}

/* Selection has been changed -- try to grab the primary selection
 * if we don't have it. Also called when clicking on an insensitive selection
 * to regain primary.
 * Also updates toolbar info.
 */
static int copying_selection = 0;
void filer_selection_changed(FilerWindow *filer_window, gint time)
{
	if (copying_selection) return; //this is very heavy for details view

	g_return_if_fail(filer_window != NULL);

	toolbar_update_info(filer_window);
	if (window_with_primary == filer_window)
		return;		/* Already got primary */

	if (!view_count_selected(filer_window->view))
		return;		/* Nothing selected */

	if (filer_window->temp_item_selected == FALSE &&
		gtk_selection_owner_set(GTK_WIDGET(filer_window->window),
				GDK_SELECTION_PRIMARY,
				time))
	{
		window_with_primary = filer_window;
		set_selection_state(filer_window, TRUE);
	}
	else
		set_selection_state(filer_window, FALSE);
}

/* Open the item (or add it to the shell command minibuffer) */
void filer_openitem(FilerWindow *filer_window, ViewIter *iter, OpenFlags flags)
{
	gboolean	shift = (flags & OPEN_SHIFT) != 0;
	gboolean	close_mini = flags & OPEN_FROM_MINI;
	gboolean	close_window = (flags & OPEN_CLOSE_WINDOW) != 0;
	DirItem		*item;
	const guchar	*full_path;
	gboolean	wink = TRUE;
	Directory	*old_dir;

	g_return_if_fail(filer_window != NULL && iter != NULL);

	item = iter->peek(iter);

	g_return_if_fail(item != NULL);

	if (filer_window->mini_type == MINI_SHELL)
	{
		minibuffer_add(filer_window, item->leafname);
		return;
	}

	if (item->base_type == TYPE_UNKNOWN)
		dir_update_item(filer_window->directory, item->leafname);

	if (item->base_type == TYPE_DIRECTORY)
	{
		/* Never close a filer window when opening a directory
		 * (click on a dir or click on an app with shift).
		 */
		if (shift || !(item->flags & ITEM_FLAG_APPDIR))
			close_window = FALSE;
	}

	full_path = make_path(filer_window->sym_path, item->leafname);
	if (shift && (item->flags & ITEM_FLAG_SYMLINK))
		wink = FALSE;

	old_dir = filer_window->directory;
	if (run_diritem(full_path, item, filer_window, flags))
	{
		if (old_dir != filer_window->directory)
			return;

		if (close_window)
			gtk_widget_destroy(filer_window->window);
		else
		{
			if (wink)
				view_wink_item(filer_window->view, iter);
			if (close_mini)
				minibuffer_hide(filer_window);
		}
	}
}

static gint pointer_in(GtkWidget *widget,
			GdkEventCrossing *event,
			FilerWindow *filer_window)
{
	may_rescan(filer_window, TRUE);
	filer_set_pointer(filer_window);
	return FALSE;
}

static gint pointer_out(GtkWidget *widget,
			GdkEventCrossing *event,
			FilerWindow *filer_window)
{
	tooltip_show(NULL);
	return FALSE;
}
static gboolean scroll_cb(GtkWidget *widget,
			GdkEventScroll *event,
			FilerWindow *fw)
{
	filer_set_pointer(fw);
	return FALSE;
}

static gboolean link_cursor_cb(gpointer ap)
{
	ViewIter iter;
	FilerWindow *fw = (FilerWindow *) ap;
	fw->right_link_idle = 0;
	view_get_cursor(fw->view, &iter);

	if (iter.peek(&iter) && iter.peek(&iter)->base_type == TYPE_DIRECTORY)
		filer_openitem(fw, &iter,
				(iter.peek(&iter)->flags & ITEM_FLAG_APPDIR ?
				 OPEN_SHIFT : 0) | OPEN_LINK_WINDOW);

	return FALSE;
}
static void link_cursor(FilerWindow *fw)
{
	if (!fw->right_link_idle)
		fw->right_link_idle = g_idle_add(link_cursor_cb, fw);
}
void filer_dir_link_next(FilerWindow *fw, GdkScrollDirection dir, gboolean bottom)
{
	ViewIter iter;

	view_get_iter(fw->view, &iter,
			VIEW_ITER_FROM_CURSOR | VIEW_ITER_EVEN_OLD_CURSOR);
	DirItem *cursor_item = iter.next(&iter);

	view_get_iter(fw->view, &iter,
			VIEW_ITER_NO_LOOP | VIEW_ITER_EVEN_OLD_CURSOR | VIEW_ITER_DIR |
			(dir == GDK_SCROLL_UP ?  VIEW_ITER_BACKWARDS : 0));

	if (iter.next(&iter))
	{
		if (cursor_item == iter.peek(&iter))
		{
			ViewIter backup = iter;

			iter.next(&iter);

			if (!iter.peek(&iter))
			{
				if (fw->right_link) return;
				iter = backup;
			}
		}

		view_cursor_to_iter(fw->view, &iter);
		link_cursor(fw);
	}
	else
		gdk_beep();
}

/* Move the cursor to the next selected item in direction 'dir'
 * (+1 or -1).
 */
void filer_next_selected(FilerWindow *fw, int dir)
{
	ViewIter	iter, cursor;
	gboolean	have_cursor;
	ViewIface	*view = fw->view;

	g_return_if_fail(dir == 1 || dir == -1);

	view_get_cursor(view, &cursor);
	have_cursor = cursor.peek(&cursor) != NULL;

	view_get_iter(view, &iter,
			VIEW_ITER_SELECTED |
			(have_cursor ? VIEW_ITER_FROM_CURSOR : 0) |
			(dir < 0 ? VIEW_ITER_BACKWARDS : 0));

	if (have_cursor && view_get_selected(view, &cursor))
		iter.next(&iter);	/* Skip the cursor itself */

	if (iter.next(&iter))
	{
		view_cursor_to_iter(view, &iter);
		if (fw->right_link)
			link_cursor(fw);
	}
	else
		gdk_beep();

	return;
}

static void return_pressed(FilerWindow *filer_window, GdkEventKey *event)
{
	TargetFunc 		cb = filer_window->target_cb;
	gpointer		data = filer_window->target_data;
	OpenFlags		flags = 0;
	ViewIter		iter;

	filer_target_mode(filer_window, NULL, NULL, NULL);

	view_get_cursor(filer_window->view, &iter);
	if (!iter.peek(&iter))
		return;

	if (cb)
	{
		cb(filer_window, &iter, data);
		return;
	}

	if (event->state & GDK_SHIFT_MASK)
		flags |= OPEN_SHIFT;

	if (event->state & GDK_MOD1_MASK ||
			event->state & GDK_CONTROL_MASK)
	{
		if (filer_window->right_link &&
				iter.peek(&iter)->base_type == TYPE_DIRECTORY)
		{
			if (o_window_link.int_value !=
					((event->state & GDK_CONTROL_MASK) != 0))
			{
				filer_cut_links(filer_window->right_link, -1);
				return;
			}
			else
				filer_cut_links(filer_window, 1);
		}
		else if (o_window_link.int_value !=
				((event->state & GDK_CONTROL_MASK) != 0))
			flags |= OPEN_LINK_WINDOW;

		flags |= OPEN_CLOSE_WINDOW;
	}
	else
		flags |= OPEN_SAME_WINDOW;


	filer_openitem(filer_window, &iter, flags);
}

/* Makes sure that 'groups' is up-to-date, reloading from file if it has
 * changed. If no groups were loaded and there is no file then initialised
 * groups to an empty document.
 * Return the node for the 'name' group.
 */
static xmlNode *group_find(char *name)
{
	xmlNode *node;
	gchar *path;

	/* Update the groups, if possible */
	path = choices_find_xdg_path_load("Groups.xml", PROJECT, SITE);
	if (path)
	{
		XMLwrapper *wrapper;
		wrapper = xml_cache_load(path);
		if (wrapper)
		{
			if (groups)
				g_object_unref(groups);
			groups = wrapper;
		}

		g_free(path);
	}

	if (!groups)
	{
		groups = xml_new(NULL);
		groups->doc = xmlNewDoc("1.0");

		xmlDocSetRootElement(groups->doc,
			xmlNewDocNode(groups->doc, NULL, "groups", NULL));
		return NULL;
	}

	node = xmlDocGetRootElement(groups->doc);

	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		guchar	*gid;

		gid = xmlGetProp(node, "name");

		if (!gid)
			continue;

		if (strcmp(name, gid) != 0)
			continue;

		g_free(gid);

		return node;
	}

	return NULL;
}

static void group_save(FilerWindow *filer_window, char *name)
{
	xmlNode	*group;
	guchar	*save_path;
	DirItem *item;
	ViewIter iter;

	group = group_find(name);
	if (group)
	{
		xmlUnlinkNode(group);
		xmlFreeNode(group);
	}
	group = xmlNewChild(xmlDocGetRootElement(groups->doc),
			NULL, "group", NULL);
	xmlSetProp(group, "name", name);

	xmlNewTextChild(group, NULL, "directory", filer_window->sym_path);

	view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);

	while ((item = iter.next(&iter)))
		xmlNewTextChild(group, NULL, "item", item->leafname);

	save_path = choices_find_xdg_path_save("Groups.xml", PROJECT, SITE,
					       TRUE);
	if (save_path)
	{
		save_xml_file(groups->doc, save_path);
		g_free(save_path);
	}
}

static gboolean group_restore_cb(ViewIter *iter, gpointer data)
{
	GHashTable *in_group = (GHashTable *) data;

	return g_hash_table_lookup(in_group,
				   iter->peek(iter)->leafname) != NULL;
}

static void group_restore(FilerWindow *filer_window, char *name)
{
	GHashTable *in_group;
	char	*path;
	xmlNode	*group, *node;

	group = group_find(name);

	if (!group)
	{
		report_error(_("Group %s is not set. Select some files "
			     "and press Ctrl+%s to set the group. Press %s "
			     "on its own to reselect the files later.\n"
			     "Make sure NumLock is on if you use the keypad."),
			     name, name, name);
		return;
	}

	node = get_subnode(group, NULL, "directory");
	g_return_if_fail(node != NULL);
	path = xmlNodeListGetString(groups->doc, node->xmlChildrenNode, 1);
	g_return_if_fail(path != NULL);

	if (strcmp(path, filer_window->sym_path) != 0)
		filer_change_to(filer_window, path, NULL);
	g_free(path);

	in_group = g_hash_table_new(g_str_hash, g_str_equal);
	for (node = group->xmlChildrenNode; node; node = node->next)
	{
		gchar *leaf;
		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "item") != 0)
			continue;

		leaf = xmlNodeListGetString(groups->doc,
						node->xmlChildrenNode, 1);
		if (!leaf)
			g_warning("Missing leafname!\n");
		else
			g_hash_table_insert(in_group, leaf, filer_window);
	}

	view_select_if(filer_window->view, &group_restore_cb, in_group);

	g_hash_table_foreach(in_group, (GHFunc) g_free, NULL);
	g_hash_table_destroy(in_group);
}

static gboolean popup_menu(GtkWidget *widget, FilerWindow *filer_window)
{
	ViewIter iter;
	GdkEvent *event;

	view_get_cursor(filer_window->view, &iter);

	event = gtk_get_current_event();
	show_filer_menu(filer_window, event, &iter);
	if (event)
		gdk_event_free(event);

	return TRUE;
}

void filer_window_toggle_cursor_item_selected(FilerWindow *filer_window)
{
	ViewIface *view = filer_window->view;
	ViewIter iter;

	view_get_iter(view, &iter, VIEW_ITER_FROM_CURSOR);
	if (!iter.next(&iter))
		return;	/* No cursor */

	if (view_get_selected(view, &iter))
		view_set_selected(view, &iter, FALSE);
	else
		view_set_selected(view, &iter, TRUE);

	if (iter.next(&iter))
		view_cursor_to_iter(view, &iter);
}

gint filer_key_press_event(GtkWidget	*widget,
			   GdkEventKey	*event,
			   FilerWindow	*filer_window)
{
	ViewIface *view = filer_window->view;
	GtkWidget *focus = GTK_WINDOW(widget)->focus_widget;
	guint key = event->keyval;
	GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask();
	char group[2] = "1";

	window_with_focus = filer_window;

	/* Delay setting up the keys until now to speed loading... */
	if (ensure_filer_menu())
	{
		/* Gtk updates in an idle-handler, so force a recheck now */
		g_signal_emit_by_name(widget, "keys_changed");
	}

	if (focus && focus == filer_window->minibuffer)
		if (gtk_widget_event(focus, (GdkEvent *) event))
			return TRUE;	/* Handled */

	if (!focus)
		gtk_widget_grab_focus(GTK_WIDGET(view));

	if (filer_window->mini_type &&
		(key == GDK_Tab || key == GDK_ISO_Left_Tab))
	{
		gtk_widget_grab_focus(filer_window->minibuffer);
		return TRUE;
	}

	if ((filer_window->right_link || filer_window->left_link) &&
			(
			key == GDK_Left  || key == GDK_KEY_KP_Left ||
			key == GDK_Right || key == GDK_KEY_KP_Right ||
			key == GDK_Up    || key == GDK_KEY_KP_Up ||
			key == GDK_Down  || key == GDK_KEY_KP_Down)
			)
		link_cursor(filer_window);

	switch (key)
	{
		case GDK_Escape:
			if ((filer_window->right_link || filer_window->left_link))
				filer_cut_links(filer_window, 0);
			filer_target_mode(filer_window, NULL, NULL, NULL);
			view_cursor_to_iter(view, NULL);
			view_clear_selection(view);
			return FALSE;
		case GDK_Return:
		case GDK_KEY_KP_Enter:
			return_pressed(filer_window, event);
			break;
		case GDK_ISO_Left_Tab:
			filer_next_selected(filer_window, -1);
			break;
		case GDK_Tab:
			filer_next_selected(filer_window, 1);
			break;
		case GDK_BackSpace:
			change_to_parent(filer_window);
			break;
		case GDK_bar:
		case GDK_backslash:
		{
			ViewIter iter;

			tooltip_show(NULL);

			view_get_cursor(view, &iter);
			show_filer_menu(filer_window,
					(GdkEvent *) event, &iter);
			break;
		}
		case ' ':
			filer_window_toggle_cursor_item_selected(filer_window);
			break;
		default:
			if (key >= GDK_0 && key <= GDK_9)
				group[0] = key - GDK_0 + '0';
			else if (key >= GDK_KP_0 && key <= GDK_KP_9)
				group[0] = key - GDK_KP_0 + '0';
			else if (!(event->state & modifiers) &&
						key >= GDK_a && key <= GDK_z)
			{
				minibuffer_show(filer_window, MINI_REG_SELECT, key);
				return TRUE;
			}
			else if ((event->state & modifiers) == GDK_SHIFT_MASK &&
						key >= GDK_A && key <= GDK_Z)
			{
				minibuffer_show(filer_window, MINI_TEMP_FILTER, key);
				return TRUE;
			}
			else
			{
				if (focus && focus != widget &&
					gtk_widget_get_toplevel(focus) == widget)
					if (gtk_widget_event(focus,
							(GdkEvent *) event))
						return TRUE;	/* Handled */
				return FALSE;
			}

			if ((event->state & modifiers) == GDK_CONTROL_MASK)
				group_save(filer_window, group);
			else if (!(event->state & modifiers))
				group_restore(filer_window, group);
			else
				return FALSE;
	}

	return TRUE;
}

void filer_open_parent(FilerWindow *filer_window)
{
	char	*dir;
	const char *current = filer_window->sym_path;

	if (current[0] == '/' && current[1] == '\0')
		return;		/* Already in the root */

	dir = g_path_get_dirname(current);
	filer_opendir(dir, filer_window, NULL, FALSE);
	g_free(dir);
}

void change_to_parent(FilerWindow *filer_window)
{
	gchar	*dir;
	const char *current = filer_window->sym_path;
	gchar	*base;

	if (current[0] == '/' && current[1] == '\0')
		return;		/* Already in the root */

	if (mount_is_user_mounted(filer_window->real_path))
		may_offer_unmount(filer_window,
				g_strdup(filer_window->real_path));

	dir = g_path_get_dirname(current);
	base = g_path_get_basename(current);
	filer_change_to(filer_window, dir, base);
	g_free(dir);
	g_free(base);
}

/* Removes trailing /s from path (modified in place) */
static void tidy_sympath(gchar *path)
{
	int l;

	g_return_if_fail(path != NULL);

	l = strlen(path);
	while (l > 1 && path[l - 1] == '/')
	{
		l--;
		path[l] = '\0';
	}
}

/* Make filer_window display path. When finished, highlight item 'from', or
 * the first item if from is NULL. If there is currently no cursor then
 * simply wink 'from' (if not NULL).
 * If the cause was a key event and we resize, warp the pointer.
 */
void filer_change_to(FilerWindow *fw,
			const char *path, const char *from)
{
	char	*from_dup;
	char	*sym_path, *real_path;
	Directory *new_dir;
	gboolean force_resize;

	g_return_if_fail(fw != NULL);

	gboolean have_cursor = view_cursor_visible(fw->view);

	filer_cancel_thumbnails(fw);

	tooltip_show(NULL);

	sym_path = g_strdup(path);
	real_path = pathdup(path);
	new_dir = g_fscache_lookup(dir_cache, real_path) ?:
		dir_new(real_path); //dummy dir

	fw->under_init = TRUE;
	fw->first_scan = TRUE;
	fw->req_sort = FALSE;

	from_dup = from && *from ? g_strdup(from) : NULL;

	detach(fw);
	g_free(fw->real_path);
	g_free(fw->sym_path);
	fw->real_path = real_path;
	fw->sym_path = sym_path;
	tidy_sympath(fw->sym_path);
	if (fw->regexp)
	{
		regfree((regex_t *) fw->regexp);
		g_free(fw->regexp);
		fw->regexp = NULL;
		g_free(fw->temp_filter_string);
		fw->temp_filter_string = NULL;
	}
	if (fw->mini_type == MINI_TEMP_FILTER)
		minibuffer_hide(fw);

	fw->directory = new_dir;

	g_free(fw->auto_select);
	fw->auto_select = NULL;

	force_resize = check_settings(fw, FALSE);


	if (fw->window->window)
		gdk_window_set_role(fw->window->window,
				    fw->sym_path);

	attach(fw);

	filer_cut_links(fw, 0);
	if (o_unique_filer_windows.int_value && !spring_in_progress)
	{
		FilerWindow *fwd;

		fwd = find_filer_window(sym_path, fw);
		if (fwd)
			gtk_widget_destroy(fwd->window);
	}

	fw->under_init = FALSE;
	display_set_actual_size(fw, force_resize);

	if (from_dup)
	{
		if (have_cursor)
			//show cursor is have to be after what set col size
			view_show_cursor(fw->view);

		display_set_autoselect(fw, from_dup);

		g_free(from_dup);
	}
	else if (have_cursor)
	{
		GdkEvent *event = gtk_get_current_event();
		if (event && event->type == GDK_KEY_PRESS)
		{
			ViewIter start;
			view_get_iter(fw->view, &start, 0);
			if (start.next(&start))
				view_cursor_to_iter(fw->view, &start);
			view_show_cursor(fw->view);
		}
		if (event)
			gdk_event_free(event);
	}

	if (fw->mini_type == MINI_PATH)
		g_idle_add((GSourceFunc) minibuffer_show_cb, fw);
}

/* Returns a list containing the full (sym) pathname of every selected item.
 * You must g_free() each item in the list.
 */
GList *filer_selected_items(FilerWindow *filer_window)
{
	GQueue retq = G_QUEUE_INIT;
	guchar	*dir = filer_window->sym_path;
	ViewIter iter;
	DirItem *item;

	view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);
	while ((item = iter.next(&iter)))
		g_queue_push_tail(&retq,
				g_strdup(make_path(dir, item->leafname)));

	return retq.head;
}

/* Return the single selected item. Error if nothing is selected. */
DirItem *filer_selected_item(FilerWindow *filer_window)
{
	ViewIter	iter;
	DirItem		*item;

	view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);

	item = iter.next(&iter);
	g_return_val_if_fail(item != NULL, NULL);
	g_return_val_if_fail(iter.next(&iter) == NULL, NULL);

	return item;
}

/* Creates and shows a new filer window.
 * If src_win != NULL then display options can be taken from that source window.
 * Returns the new filer window, or NULL on error.
 * Note: if unique windows is in use, may return an existing window.
 */
FilerWindow *filer_opendir(const char *path, FilerWindow *src_win,
			   const gchar *wm_class, gboolean winlnk)
{
	FilerWindow	*filer_window;
	char		*real_path;
	char *from_dup = NULL;

	if (o_unique_filer_windows.int_value && !spring_in_progress)
	{
		FilerWindow	*same_dir_window;

		same_dir_window = find_filer_window(path, NULL);

		if (same_dir_window)
		{
			if (winlnk)
				gtk_widget_destroy(same_dir_window->window);
			else {
				gtk_window_present(GTK_WINDOW(same_dir_window->window));
				return same_dir_window;
			}
		}
	}

	/* Get the real pathname of the directory and copy it */
	real_path = pathdup(path);

	if (src_win) {
		char *dir = g_path_get_dirname(src_win->sym_path);
		if (strcmp(path, dir) == 0)
			from_dup = g_path_get_basename(src_win->sym_path);
		g_free(dir);
	}

	filer_window = g_new(FilerWindow, 1);
	filer_window->under_init = TRUE;
	filer_window->first_scan = TRUE;
	filer_window->new_win_first_scan = TRUE;
	filer_window->onlyicon = FALSE;
	filer_window->req_sort = FALSE;
	filer_window->may_resize = FALSE;
	filer_window->presented = FALSE;

	filer_window->message = NULL;
	filer_window->minibuffer = NULL;
	filer_window->minibuffer_label = NULL;
	filer_window->minibuffer_area = NULL;
	filer_window->temp_show_hidden = FALSE;
	filer_window->sym_path = g_strdup(path);
	filer_window->real_path = real_path;
	filer_window->scanning = FALSE;
	filer_window->auto_select = NULL;
	filer_window->toolbar_text = NULL;
	filer_window->toolbar_size_text = NULL;
	filer_window->toolbar_settings_text = NULL;
	filer_window->target_cb = NULL;
	filer_window->mini_type = MINI_NONE;
	filer_window->selection_state = GTK_STATE_ACTIVE;
	filer_window->toolbar = NULL;
	filer_window->toplevel_vbox = NULL;
	filer_window->view_hbox = NULL;
	filer_window->view = NULL;
	filer_window->scrollbar = NULL;
	filer_window->auto_scroll = -1;
	filer_window->window_id = NULL;
	filer_window->dir_colour = NULL;
	filer_window->configured = 0;
	filer_window->last_width = -1;
	filer_window->last_height = -1;
	filer_window->dir_icon = NULL;
	filer_window->right_link = NULL;
	filer_window->left_link = NULL;
	filer_window->right_link_idle = 0;
	filer_window->accept_timeout = 0;
	filer_window->pointer_idle = 0;
	filer_window->resize_drag_width = 0;

	tidy_sympath(filer_window->sym_path);

	/* Finds the entry for this directory in the dir cache, creating
	 * a new one if needed. This does not cause a scan to start,
	 * so if a new entry is created then it will be empty.
	 */
	filer_window->directory = g_fscache_lookup(dir_cache, real_path) ?:
		dir_new(real_path); //dummy dir

	filer_window->temp_item_selected = FALSE;
	filer_window->flags = (FilerFlags) 0;
	filer_window->thumb_queue = g_queue_new();
	filer_window->thumb_bar_time = 0;
	filer_window->max_thumbs = 0;
	filer_window->trying_thumbs = 0;

	filer_window->temp_filter_string = NULL;
	filer_window->regexp = NULL;

	/* Display settings target */
	filer_window->filter = FILER_SHOW_ALL;
	filer_window->filter_string = NULL;
	if (src_win && (winlnk || o_display_inherit_options.int_value))
		filer_copy_settings(src_win, filer_window);
	else
		filer_clear_settings(filer_window);

	/* Add all the user-interface elements & realise */
	filer_add_widgets(filer_window, wm_class);
	if (src_win) {
		if (winlnk)
			filer_link(src_win, filer_window);
		else
			gtk_window_set_position(GTK_WINDOW(filer_window->window),
					GTK_WIN_POS_MOUSE);
	}

	/* Override the current defaults with the per-directory
	 * user settings.
	 */
	check_settings(filer_window, FALSE);

	/* Connect to all the signal handlers */
	filer_add_signals(filer_window);

	/* The view is created empty and then attach() is called, which
	 * links the filer window to the entry in the directory cache we
	 * looked up / created above.
	 *
	 * The attach() function will immediately callback to the filer window
	 * to deliver a list of all known entries in the directory (so,
	 * the number of items will be known after attach() returns).
	 *
	 * If the directory was not in the cache (because it hadn't been
	 * opened it before) then the types and icons for the entries are
	 * not know, but the list of names is.
	 */
	attach(filer_window);

	if (from_dup)
		display_set_autoselect(filer_window, from_dup);
	g_free(from_dup);

	number_of_windows++;
	all_filer_windows = g_list_prepend(all_filer_windows, filer_window);

	filer_window->under_init = FALSE;
	display_set_actual_size(filer_window,
			o_filer_auto_resize.int_value != RESIZE_ALWAYS);

	gtk_widget_show(filer_window->window);
	gtk_window_present(GTK_WINDOW(filer_window->window));

	return filer_window;
}


static gboolean setviewcb(FilerWindow *fw)
{
	if (filer_exists(fw))
		filer_selection_changed(fw, gtk_get_current_event_time());
	return FALSE;
}

void filer_set_view_type(FilerWindow *filer_window, ViewType type)
{
	GtkWidget *view = NULL;
	Directory *dir = NULL;
	GHashTable *selected = NULL;

	g_return_if_fail(filer_window != NULL);

	tooltip_show(NULL);
	motion_window = NULL;

	if (!filer_window->under_init)
	{
		/* Save the current selection */
		ViewIter iter;
		DirItem *item;

		selected = g_hash_table_new(g_str_hash, g_str_equal);
		view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);
		while ((item = iter.next(&iter)))
			g_hash_table_insert(selected, item->leafname, "yes");

		dir = filer_window->directory;
		g_object_ref(dir);
		detach(filer_window);
	}

	if (filer_window->view)
	{
		dnd_motion_ungrab();
		/* Destroy the old view */
		gtk_widget_destroy(GTK_WIDGET(filer_window->view));
		filer_window->view = NULL;
	}

	switch (type)
	{
		case VIEW_TYPE_COLLECTION:
			view = view_collection_new(filer_window);
			break;
		case VIEW_TYPE_DETAILS:
			view = view_details_new(filer_window);
			break;
	}

	g_return_if_fail(view != NULL);

	filer_window->view = VIEW(view);
	filer_window->view_type = type;

	gtk_box_pack_start(filer_window->view_hbox, view, TRUE, TRUE, 0);
	gtk_widget_show(view);

	/* Drag and drop events */
	make_drop_target(view, 0);
	g_signal_connect(view, "drag_motion",
			G_CALLBACK(drag_motion), filer_window);
	g_signal_connect(view, "drag_leave",
			G_CALLBACK(drag_leave), filer_window);
	g_signal_connect(view, "drag_end",
			G_CALLBACK(drag_end), filer_window);
	/* Dragging from us... */
	g_signal_connect(view, "drag_data_get",
			G_CALLBACK(drag_data_get), NULL);

	g_signal_connect(view, "scroll-event",
			G_CALLBACK(scroll_cb), filer_window);

	gtk_widget_add_events(view, GDK_LEAVE_NOTIFY_MASK);
	g_signal_connect_swapped(view, "leave-notify-event",
			G_CALLBACK(filer_set_pointer), filer_window);

	if (dir)
	{
		/* Only when changing type. Otherwise, will attach later. */
		filer_window->directory = dir;
		attach(filer_window);

		if (o_filer_auto_resize.int_value != RESIZE_NEVER)
			view_autosize(filer_window->view, FALSE);
	}

	if (selected)
	{
		ViewIter iter;
		DirItem *item;

		copying_selection = g_hash_table_size(selected);

		view_get_iter(filer_window->view, &iter, 0);
		while ((item = iter.next(&iter)))
		{
			if (g_hash_table_lookup(selected, item->leafname))
				view_set_selected(filer_window->view,
						  &iter, TRUE);
		}
		g_hash_table_destroy(selected);

		if (copying_selection)
		{
			copying_selection = 0;
			//somehow details view's selection data is not available yet
			//Specifically, view_count_selected returns 0
			// and gtk_selection_owner_set returns false
			g_idle_add((GSourceFunc)setviewcb, filer_window);
		}
	}
}

static gint set_font(GtkWidget *widget)
{
	PangoContext *context = gtk_widget_get_pango_context(widget);
	PangoFontDescription *desc = pango_context_get_font_description(context);

	if (!current_font || !pango_font_description_equal(desc, current_font))
	{
		pango_font_description_free(current_font);
		current_font = pango_font_description_copy(desc);

		PangoFont *font = pango_context_load_font(context, desc);
		cairo_scaled_font_t *scaled =
			pango_cairo_font_get_scaled_font(PANGO_CAIRO_FONT(font));

		PangoFontDescription *descb = pango_font_description_copy(desc);
		pango_font_description_set_weight(descb, PANGO_WEIGHT_BOLD);
		PangoFont *fontb = pango_context_load_font(context, descb);
		pango_font_description_free(descb);
		cairo_scaled_font_t *scaledb =
			pango_cairo_font_get_scaled_font(PANGO_CAIRO_FONT(fontb));

		if (o_fast_font_calc.int_value)
		{
			gint i = 0x20;
			gchar n[] = {' ', '\0'};
			cairo_text_extents_t te;

			for (; i <= 0x7e; i++)
			{
				n[0] = (gchar) i;
				cairo_scaled_font_text_extents(scaled , n, &te);
				fw_font_widths [i] = (gint) (te.x_advance + .5);
				cairo_scaled_font_text_extents(scaledb, n, &te);
				fw_font_widthsb[i] = (gint) (te.x_advance + .5);
//g_print("caito te %s: x %f, y %f, w %f, h %f, xa %f, ya %f\n",
//n, te.x_bearing, te.y_bearing, te.width, te.height,
//te.x_advance, te.y_advance);
			}
		}

		g_object_unref(font);
		g_object_unref(fontb);

		//font height is diffrent between cairo and pango
		PangoLayout *layout = gtk_widget_create_pango_layout(widget, "|");
		pango_layout_get_pixel_size(layout, NULL, &fw_font_height);
		g_object_unref(layout);

		//monospace for details
		PangoFontDescription *monospace = pango_font_description_from_string("monospace");
		layout = gtk_widget_create_pango_layout(widget, "|");
		pango_layout_set_font_description(layout, monospace);
		pango_layout_get_pixel_size(layout, &fw_mono_width, &fw_mono_height);
		g_object_unref(layout);
		pango_font_description_free(monospace);
	}

	//make sure small_height is not font height but icons height.
	if (pango_font_description_get_size_is_absolute(desc))
		return fw_font_height;
	else
		/* if same font size then same icon size is good */
		return pango_font_description_get_size(desc) * 18 / 10 / PANGO_SCALE;
}

/* This adds all the widgets to a new filer window. It is in a separate
 * function because filer_opendir() was getting too long...
 */
static void filer_add_widgets(FilerWindow *filer_window, const gchar *wm_class)
{
	GtkWidget *hbox, *vbox;

	/* Create the top-level window widget */
	filer_window->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	filer_set_title(filer_window);
	gtk_widget_set_name(filer_window->window, "rox-filer");

	if (wm_class)
		gtk_window_set_wmclass(GTK_WINDOW(filer_window->window),
				       wm_class, PROJECT);

	if (small_height == 0)
	{
		small_height = set_font(filer_window->window);
		small_width = (SMALL_WIDTH * small_height) / SMALL_HEIGHT;
	}

	GdkScreen *screen = gtk_widget_get_screen(filer_window->window);
	GdkColormap *rgba = gdk_screen_get_rgba_colormap(screen);
	if (rgba)
		gtk_widget_set_colormap(filer_window->window, rgba);

	/* This property is cleared when the window is destroyed.
	 * You can thus ref filer_window->window and use this to see
	 * if the window no longer exists.
	 */
	g_object_set_data(G_OBJECT(filer_window->window),
			"filer_window", filer_window);

	/* Create this now to make the Adjustment before the View */
	filer_window->scrollbar = gtk_vscrollbar_new(NULL);

	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(filer_window->window), vbox);

	filer_window->toplevel_vbox = GTK_BOX(vbox);

	/* If there's a message that should be displayed in each window (eg
	 * 'Running as root'), add it here.
	 */
	filer_window->message = gtk_label_new(show_user_message);
	gtk_box_pack_start(GTK_BOX(vbox), filer_window->message, FALSE, TRUE, 0);
	if (show_user_message && !(o_hide_root_msg.int_value && euid == 0))
		gtk_widget_show(filer_window->message);

	hbox = gtk_hbox_new(FALSE, 0);
	filer_window->view_hbox = GTK_BOX(hbox);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
	/* Add the main View widget */
	filer_set_view_type(filer_window, filer_window->view_type);
	/* Put the scrollbar next to the View */
	gtk_box_pack_end(GTK_BOX(hbox),
			filer_window->scrollbar, FALSE, TRUE, 0);
	gtk_widget_show(hbox);

	/* If we want a toolbar, create it now */
	toolbar_update_toolbar(filer_window);

	/* And the minibuffer (hidden to start with) */
	create_minibuffer(filer_window);
	gtk_box_pack_end(GTK_BOX(vbox), filer_window->minibuffer_area,
				FALSE, TRUE, 0);

	/* And the thumbnail progress bar (also hidden) */
	{
		filer_window->thumb_bar = gtk_progress_bar_new();
		gtk_widget_set_size_request(filer_window->thumb_bar,
				-1, small_height * (1./3.));

		gtk_box_pack_end(GTK_BOX(vbox), filer_window->thumb_bar,
				FALSE, TRUE, 0);
	}

	gtk_widget_show(vbox);
	gtk_widget_show(filer_window->scrollbar);
	gtk_widget_realize(filer_window->window);

	gdk_window_set_role(filer_window->window->window,
			    filer_window->sym_path);
}

static gboolean configure_cb(
		GtkWidget *widget,
		GdkEvent *event,
		FilerWindow *fw)
{
	fw->configured = 1;

	if (fw->resize_drag_width)
	{
		gint cw;
		gdk_window_get_geometry(
				fw->window->window, NULL, NULL,
				&cw,
				NULL, NULL);

		if (fw->name_scale == 1.0) //just, toolbar doesn't know collection
			fw->name_scale_itemw =
				VIEW_COLLECTION(fw->view)->collection->item_width;

		gfloat last = fw->name_scale;
		fw->name_scale = cw * fw->name_scale_start / fw->resize_drag_width;
		if (fw->name_scale_start == 1.0)
		{
			if (fw->name_scale > 1.03)
				fw->name_scale -= 0.03; //margin
			else if (fw->name_scale > 1.0)
				fw->name_scale = 1.0;
			else if (fw->name_scale < 1.0)
				fw->name_scale -= 0.03; //margin
		}

		if (last != fw->name_scale)
			view_style_changed(fw->view, VIEW_UPDATE_NAME | VIEW_UPDATE_SCALE);
	}

	if (fw->right_link)
		filer_link(fw, fw->right_link);

	filer_set_pointer(fw);

	return FALSE;
}
static gboolean focus_in_cb(GtkWidget *w, GdkEvent *e, FilerWindow *fw)
{
	fw->resize_drag_width = 0;

	if (!fw->presented)
	{
		fw->presented = TRUE;
		return FALSE;
	}
	if (!fw->left_link) return FALSE;

	GdkRectangle lfrect, rfrect;
	gdk_window_get_frame_extents(
			fw->left_link->window->window, &lfrect);
	gdk_window_get_frame_extents(
			fw->window->window, &rfrect);
	if (
			rfrect.x + (fw_font_height / 2) < lfrect.x + lfrect.width ||
			rfrect.x - (fw_font_height / 2) > lfrect.x + lfrect.width)
		filer_cut_links(fw, -1);

	return FALSE;
}

static void filer_add_signals(FilerWindow *filer_window)
{
	GtkTargetEntry 	target_table[] =
	{
		{"text/uri-list", 0, TARGET_URI_LIST},
		{"UTF8_STRING", 0, TARGET_STRING},
		{"STRING", 0, TARGET_STRING},
		{"COMPOUND_TEXT", 0, TARGET_STRING},/* XXX: Treats as STRING */
	};

	/* Events on the top-level window */
	gtk_widget_add_events(filer_window->window, GDK_ENTER_NOTIFY);
	g_signal_connect(filer_window->window, "enter-notify-event",
			G_CALLBACK(pointer_in), filer_window);
	g_signal_connect(filer_window->window, "leave-notify-event",
			G_CALLBACK(pointer_out), filer_window);
	g_signal_connect(filer_window->window, "destroy",
			G_CALLBACK(filer_window_destroyed), filer_window);
	g_signal_connect(filer_window->window, "delete-event",
			G_CALLBACK(filer_window_delete), filer_window);

	g_signal_connect(filer_window->window, "selection_clear_event",
			G_CALLBACK(filer_lost_primary), filer_window);

	g_signal_connect(filer_window->window, "selection_get",
			G_CALLBACK(selection_get), filer_window);

	gtk_selection_add_targets(GTK_WIDGET(filer_window->window),
			GDK_SELECTION_PRIMARY,
			target_table,
			sizeof(target_table) / sizeof(*target_table));

	g_signal_connect(filer_window->window, "popup-menu",
			G_CALLBACK(popup_menu), filer_window);
	g_signal_connect(filer_window->window, "key_press_event",
			G_CALLBACK(filer_key_press_event), filer_window);

	g_signal_connect_swapped(filer_window->window, "style_set",
			G_CALLBACK(set_font), filer_window->window);

	g_signal_connect(filer_window->window, "configure-event",
			G_CALLBACK(configure_cb), filer_window);

	g_signal_connect(filer_window->window, "focus-in-event",
			G_CALLBACK(focus_in_cb), filer_window);

	gtk_window_add_accel_group(GTK_WINDOW(filer_window->window),
				   filer_keys);
}

static gint clear_scanning_display(FilerWindow *filer_window)
{
	if (filer_exists(filer_window))
		filer_set_title(filer_window);
	return FALSE;
}

static void set_scanning_display(FilerWindow *filer_window, gboolean scanning)
{
	if (scanning == filer_window->scanning)
		return;
	filer_window->scanning = scanning;

	if (scanning)
		filer_set_title(filer_window);
	else
		g_timeout_add(300, (GSourceFunc) clear_scanning_display,
				filer_window);
}

/* Note that filer_window may not exist after this call.
 * Returns TRUE iff the directory still exists.
 */
gboolean filer_update_dir(FilerWindow *filer_window, gboolean warning)
{
	gboolean still_exists;

	still_exists = may_rescan(filer_window, warning);

	if (still_exists)
		dir_update(filer_window->directory, filer_window->sym_path);

	return still_exists;
}

void filer_update_all(void)
{
	GList	*next = all_filer_windows;

	while (next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		/* Updating directory may remove it from list -- stop sending
		 * patches to move this line!
		 */
		next = next->next;

		/* Don't trigger a refresh if we're already scanning.
		 * Otherwise, two views of a single directory will trigger
		 * two scans.
		 */
		if (filer_window->directory &&
		    !filer_window->directory->scanning)
			filer_update_dir(filer_window, TRUE);
	}
}

static gboolean _delay_check_resize(gpointer na)
{
	GList *next;
	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *fw = (FilerWindow *) next->data;
		if (na || fw->may_resize) {
			filer_autosize(fw);
		}
	}
	return FALSE;
}
void filer_resize_all(gboolean all)
{
	if (all_filer_windows) //for first time
		g_timeout_add(DIR_NOTIFY_TIME * 2,
				_delay_check_resize, GUINT_TO_POINTER(all));
}

/* Refresh the various caches even if we don't think we need to */
void full_refresh(void)
{
	dir_stop();
	read_globicons();
	mount_update(TRUE);
	reread_mime_files();	/* Refreshes all windows */
}

/* See whether a filer window with a given path already exists
 * and is different from diff.
 */
FilerWindow *find_filer_window(const char *sym_path, FilerWindow *diff)
{
	GList	*next;

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		if (filer_window != diff &&
		    	strcmp(sym_path, filer_window->sym_path) == 0)
			return filer_window;
	}

	return NULL;
}

/* This path has been mounted/umounted/deleted some files - update all dirs */
void filer_check_mounted(const char *real_path)
{
	GList	*next = all_filer_windows;
	gchar	*parent;
	int	len;
	gboolean resize = o_filer_auto_resize.int_value == RESIZE_ALWAYS;

	/* DOS disks, etc, often don't change the mtime of the root directory
	 * on modification, so force a refresh now.
	 */
	g_fscache_update(dir_cache, real_path);

	len = strlen(real_path);

	while (next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		next = next->next;

		if (strncmp(real_path, filer_window->real_path, len) == 0)
		{
			char	s = filer_window->real_path[len];

			if (s == '/' || s == '\0')
			{
				if (filer_update_dir(filer_window, FALSE) &&
				    resize)
					view_autosize(filer_window->view, FALSE);
			}
		}
	}

	parent = g_path_get_dirname(real_path);
	refresh_dirs(parent);
	g_free(parent);

	icons_may_update(real_path);
}

/* Close all windows displaying 'path' or subdirectories of 'path' */
gboolean filer_close_recursive(char *path)
{
	GList	*next = all_filer_windows;
	gchar	*real;
	int	len;

	real = pathdup(path);
	g_free(path);
	len = strlen(real);

	while (next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		next = next->next;

		if (strncmp(real, filer_window->real_path, len) == 0)
		{
			char s = filer_window->real_path[len];

			if (len == 1 || s == '/' || s == '\0')
				gtk_widget_destroy(filer_window->window);
		}
	}
	return FALSE;
}

/* Like minibuffer_show(), except that:
 * - It returns FALSE (to be used from an idle callback)
 * - It checks that the filer window still exists.
 */
static gboolean minibuffer_show_cb(FilerWindow *filer_window)
{
	if (filer_exists(filer_window))
		minibuffer_show(filer_window, MINI_PATH, 0);
	return FALSE;
}

/* TRUE iff filer_window points to an existing FilerWindow
 * structure.
 */
gboolean filer_exists(FilerWindow *filer_window)
{
	GList	*next;

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *fw = (FilerWindow *) next->data;

		if (fw == filer_window)
			return TRUE;
	}

	return FALSE;
}

FilerWindow *filer_get_by_id(const char *id)
{
	return g_hash_table_lookup(window_with_id, id);
}

void filer_set_id(FilerWindow *filer_window, const char *id)
{
	g_return_if_fail(filer_window != NULL);

	if (filer_window->window_id)
	{
		g_hash_table_remove(window_with_id, filer_window->window_id);
		g_free(filer_window->window_id);
		filer_window->window_id = NULL;
	}

	if (id)
	{
		filer_window->window_id = g_strdup(id);
		g_hash_table_insert(window_with_id,
				filer_window->window_id,
				filer_window);
	}
}

/* Make sure the window title is up-to-date */
void filer_set_title(FilerWindow *filer_window)
{
	gchar	*title = NULL;
	guchar	*flags = "";

	if (filer_window->scanning ||
	    filer_window->filter != FILER_SHOW_ALL ||
 	    (filer_window->show_hidden != o_display_show_hidden.int_value) ||
 		(filer_window->show_thumbs != o_display_show_thumbs.int_value) ||
		filer_window->dirs_only || filer_window->files_only)
	{
		if (o_short_flag_names.int_value)
		{
 			const gchar  *hidden = "", *notflag = "";

			switch(filer_window->filter) {
			case FILER_SHOW_ALL:
 				if (filer_window->show_hidden != o_display_show_hidden.int_value) {
 					hidden = _("A");
 					notflag = !filer_window->show_hidden ? _("!") : "";
 				}
			break;
			case FILER_SHOW_GLOB:   hidden =  _("G"); break;
			default: break;
			}

			flags = g_strconcat(" +",
				filer_window->scanning ? _("S") : "",
				notflag,
				hidden,
				filer_window->show_thumbs != o_display_show_thumbs.int_value &&
 					!filer_window->show_thumbs ? _("!") : "",
 				filer_window->show_thumbs != o_display_show_thumbs.int_value ? _("T") : "",
				filer_window->dirs_only ? _("D") : "",
				filer_window->files_only ? _("F") : "",
				NULL);
		}
		else
		{
			gchar  *hidden = NULL;

			switch(filer_window->filter) {
			case FILER_SHOW_ALL:
 				if (filer_window->show_hidden != o_display_show_hidden.int_value)
 					hidden = g_strdup(filer_window->show_hidden ? _("ALL, ") : _("Not ALL, "));
 				else
 					hidden = g_strdup("");
				break;
			case FILER_SHOW_GLOB:
				hidden = g_strdup_printf(_("Glob (%s), "),
						 filer_window->filter_string);
				break;
			default:
				hidden  =g_strdup("");
				break;
			}
			flags = g_strconcat(" (",
				filer_window->scanning ? _("Scanning, ") : "",
				hidden,
 				filer_window->show_thumbs != o_display_show_thumbs.int_value ?
 					(filer_window->show_thumbs ? _("Thumbs, ") : _("Not Thumbs, ")) : "",
				filer_window->dirs_only ? _("Dirs only, ") : "",
				filer_window->files_only ? _("Files only, ") : "",
				NULL);
			flags[strlen(flags) - 2] = ')';
			g_free(hidden);
		}
	}

	if (not_local)
	        title = g_strconcat("//", our_host_name(TRUE),
			    filer_window->sym_path, flags, NULL);

	if (!title)
	{
		gchar *cpath = collapse_path(filer_window->sym_path);
		title = g_strconcat(cpath, flags, NULL);
		g_free(cpath);
	}

	ensure_utf8(&title);

	if (filer_window->directory->error)
	{
		gchar *old = title;
		title = g_strconcat("!", old, NULL);
		g_free(old);
	}

	if (filer_window->right_link) {
		gchar *old = title;
		title = g_strconcat(old, "  ☛☛", NULL);
		g_free(old);
	}
	if (filer_window->left_link) {
		gchar *old = title;
		title = g_strconcat("☚☚  ", old, NULL);
		g_free(old);
	}


	gtk_window_set_title(GTK_WINDOW(filer_window->window), title);

	g_free(title);

	if (flags[0] != '\0')
		g_free(flags);
}

/* Reconnect to the same directory (used when the Show Hidden option is
 * toggled). This has the side-effect of updating the window title.
 */
void filer_detach_rescan(FilerWindow *filer_window)
{
	Directory *dir = filer_window->directory;

	g_object_ref(dir);
	//if there were no other users this dir wills drop from cache
	detach(filer_window);
	filer_window->directory = dir;
	attach(filer_window);
}

/* Puts the filer window into target mode. When an item is chosen,
 * fn(filer_window, iter, data) is called. 'reason' will be displayed
 * on the toolbar while target mode is active.
 *
 * Use fn == NULL to cancel target mode.
 */
void filer_target_mode(FilerWindow *filer_window,
			TargetFunc fn,
			gpointer data,
			const char *reason)
{
	TargetFunc old_fn = filer_window->target_cb;

	if (fn != old_fn)
		gdk_window_set_cursor(
				GTK_WIDGET(filer_window->view)->window,
				fn ? crosshair : NULL);

	filer_window->target_cb = fn;
	filer_window->target_data = data;

	if (filer_window->toolbar_text == NULL)
		return;

	if (fn)
		gtk_label_set_text(
			GTK_LABEL(filer_window->toolbar_text), reason);
	else if (o_toolbar_info.int_value)
	{
		if (old_fn)
			toolbar_update_info(filer_window);
	}
	else
		gtk_label_set_text(GTK_LABEL(filer_window->toolbar_text), "");
}

static void set_selection_state(FilerWindow *filer_window, gboolean normal)
{
	FilerWindow *fw = filer_window;

	GtkStateType old_state = filer_window->selection_state;

	filer_window->selection_state = normal
			? GTK_STATE_SELECTED : GTK_STATE_ACTIVE;

	if (filer_window->view_type == VIEW_TYPE_DETAILS)
	{
		GtkStyle *style = fw->window->style;
		gtk_widget_modify_base(GTK_WIDGET(fw->view),
				GTK_STATE_ACTIVE, &style->base[fw->selection_state]);
		gtk_widget_modify_text(GTK_WIDGET(fw->view),
				GTK_STATE_ACTIVE, &style->text[fw->selection_state]);
	}

	if (old_state != filer_window->selection_state
	    && view_count_selected(filer_window->view))
		gtk_widget_queue_draw(GTK_WIDGET(filer_window->view));
}

static void free_subdir_info(void)
{
	if (!sdinfo.path) return;
	g_free(sdinfo.path);
	g_free(sdinfo.thumb_path);

	guint i = sdinfo.items->len;
	while (i--) g_free(sdinfo.items->pdata[i]);
	g_ptr_array_free(sdinfo.items, TRUE);

	static const SubDirInfo clear = {NULL, NULL, NULL, NULL, 0, TRUE};
	sdinfo = clear;
}

static gpointer scandir_thread(gpointer data)
{
	//alphasort is not equal to rox's sort. Even doesn't check current settings.
	sdinfo.cancel = FALSE;
	sdinfo.tried = 0;
	sdinfo.items = list_dir_all(sdinfo.path);
	g_mutex_unlock(&m_dirthumb);
	return NULL;
}

static gboolean make_dir_thumb_link()
{
	gchar *path = sdinfo.path;
	gchar *thumb_path = sdinfo.thumb_path;

	int i = sdinfo.tried;
	sdinfo.tried += 3;
	int end = MIN(sdinfo.items->len, 99);
	int currentend = MIN(sdinfo.tried, end);

	for (; i < currentend; i++)
	{
		const gchar *subpath = make_path(path,
				sdinfo.items->pdata[i]);

		struct stat	info;
		if (mc_lstat(subpath, &info) == -1 ||
			mode_to_base_type(info.st_mode) != TYPE_FILE)
			continue;

		gchar *sp = g_strdup(subpath);
		switch (pixmap_check_thumb(sp))
		{
		case 0:
			sdinfo.fw->max_thumbs++;
			g_queue_push_tail(sdinfo.fw->thumb_queue, sp/*eaten*/);
			goto done;
		case 1:
			{
			char *sub_thumb_path = pixmap_make_thumb_path(sp);
			char *rel_path = get_relative_path(thumb_path, sub_thumb_path);
			g_free(sub_thumb_path);

			if (symlink(rel_path, thumb_path) == 0)
				dir_force_update_path(path, TRUE);
			//even the symlink fails this loop wills break.

			g_free(rel_path);
			g_free(sp);
			goto done;
			}
		}
		g_free(sp);
	}

	if (currentend < end)
		return TRUE;

done:
	free_subdir_info();
	return FALSE;
}

void filer_cancel_thumbnails(FilerWindow *filer_window)
{
	filer_window->thumb_bar_time = 0;
	if (gtk_widget_get_visible(filer_window->thumb_bar))
	{
		gtk_widget_hide(filer_window->thumb_bar);
		filer_autosize(filer_window);
	}

	g_queue_free_full(filer_window->thumb_queue, g_free);
	filer_window->thumb_queue = g_queue_new();

	filer_window->max_thumbs = 0;

	if (sdinfo.fw == filer_window)
		sdinfo.cancel = TRUE;
}

/* Generate the next thumb for this window. The window object is
 * unref'd when there is nothing more to do.
 * If the window no longer has a filer window, nothing is done.
 */
static gboolean filer_next_thumb_real(GObject *window)
{
	FilerWindow *filer_window, *fw;
	gchar	*path;
	gboolean noorder = FALSE;

	fw = filer_window = g_object_get_data(window, "filer_window");

	if (!filer_window)
	{
		g_object_unref(window);
		return FALSE;
	}

	if (g_mutex_trylock(&m_dirthumb))
		g_mutex_unlock(&m_dirthumb);
	else
	{
		filer_next_thumb(window, NULL);
		return FALSE;
	}

	if (!sdinfo.cancel && sdinfo.items->len)
	{
		guint len = filer_window->thumb_queue->length;

		if (make_dir_thumb_link())
		{
			filer_next_thumb(window, NULL);
			return FALSE;
		}
		//this is first item and only one of subdir
		noorder = len != filer_window->thumb_queue->length;
	}
	else
		free_subdir_info();

	if (g_queue_is_empty(filer_window->thumb_queue))
	{
		filer_window->trying_thumbs--;
		if (filer_window->trying_thumbs == 0)
			filer_cancel_thumbnails(filer_window);
		g_object_unref(window);
		return FALSE;
	}

	path = (gchar *) g_queue_pop_tail(filer_window->thumb_queue);

	if (!g_file_test(path, G_FILE_TEST_EXISTS))
	{
		filer_next_thumb(window, NULL);
		goto out;
	}

	switch (pixmap_check_thumb(path))
	{
	case -1:
		if (o_display_show_dir_thumbs.int_value == 1)
		{
			struct stat info;
			if (mc_lstat(path, &info) != -1 &&
				mode_to_base_type(info.st_mode) == TYPE_DIRECTORY)
			{
				free_subdir_info();
				sdinfo.fw = filer_window;
				sdinfo.thumb_path = pixmap_make_thumb_path(path);
				sdinfo.path = g_strdup(path);
				g_mutex_lock(&m_dirthumb);
				GThread *scd = g_thread_new("scandir_t", scandir_thread, NULL);

				g_thread_unref(scd);
			}
		}
	case -2:
		filer_next_thumb(window, NULL);
		goto out;
	case 1:
		filer_next_thumb(window, path);
		goto out;
	case 0:
		break;
	}

	pixmap_background_thumb(path, noorder, (GFunc) filer_next_thumb, window);

	if (!fw->thumb_bar_time) {
		fw->thumb_bar_time = g_get_monotonic_time();
	} else
	if (!gtk_widget_get_visible(filer_window->thumb_bar))
	{
		if (g_get_monotonic_time() - fw->thumb_bar_time < .3 * 1000000)
			goto out;
		gtk_widget_show_all(filer_window->thumb_bar);
		filer_autosize(filer_window);
	}

	int done, total;
	total = filer_window->max_thumbs;
	done = total - g_queue_get_length(filer_window->thumb_queue);

	gtk_progress_bar_set_fraction(
			GTK_PROGRESS_BAR(filer_window->thumb_bar),
			done / (float) total);

out:
	g_free(path);
	return FALSE;
}

/* path is the thumb just loaded, if any.
 * window is unref'd (eventually).
 */
static void filer_next_thumb(GObject *window, const gchar *path)
{
	if (path)
		dir_force_update_path(path, TRUE);

	g_idle_add_full(G_PRIORITY_LOW, (GSourceFunc) filer_next_thumb_real, window, NULL);
}

static void start_thumb_scanning(FilerWindow *filer_window)
{
	if (filer_window->trying_thumbs >= g_get_num_processors())
		return;		/* Already scanning */

	filer_window->trying_thumbs++;

	g_object_ref(G_OBJECT(filer_window->window));
	filer_next_thumb(G_OBJECT(filer_window->window), NULL);
}

/* Set this image to be loaded some time in the future */
void filer_create_thumb(FilerWindow *filer_window, const gchar *path)
{
	if (g_queue_is_empty(filer_window->thumb_queue)) {
		filer_window->max_thumbs=0;
	}

	filer_window->max_thumbs++;

	g_queue_push_head(filer_window->thumb_queue, g_strdup(path));

	start_thumb_scanning(filer_window);
}

/* If thumbnail display is on, look through all the items in this directory
 * and start creating or updating the thumbnails as needed.
 */
static DirItem *diritem_next(ViewIter *iter, GPtrArray *items, int *i)
{
	if (items == NULL)
		return iter->next(iter);
	else if (*i < items->len)
	{
		*i += 1;
		return items->pdata[*i - 1];
	}

	return NULL;
}
void filer_create_thumbs(FilerWindow *filer_window, GPtrArray *items)
{
	DirItem *item;
	ViewIter iter;
	int index = 0;

	if (!filer_window->show_thumbs)
		return;

	if (items == NULL)
		view_get_iter(filer_window->view, &iter, 0);

	while ((item = diritem_next(&iter, items, &index)))
	{
		const guchar *path;

		 if (item->base_type != TYPE_FILE &&
				(o_display_show_dir_thumbs.int_value != 1 ||
				 item->base_type != TYPE_DIRECTORY))
			 continue;

		 /*if (strcmp(item->mime_type->media_type, "image") != 0)
		   continue;*/

		path = make_path(filer_window->real_path, item->leafname);
		filer_create_thumb(filer_window, path);
	}
}

static void filer_options_changed(void)
{
	if (o_short_flag_names.has_changed)
	{
		GList *next;
		for (next = all_filer_windows; next; next = next->next)
		{
			FilerWindow *filer_window = (FilerWindow *) next->data;

			filer_set_title(filer_window);
		}
	}

	if (o_fast_font_calc.has_changed &&
		o_fast_font_calc.int_value &&
		all_filer_windows)
	{
		pango_font_description_free(current_font);
		current_font = NULL;
		set_font(((FilerWindow *) all_filer_windows->data)->window);
	}


	if (all_filer_windows && o_hide_root_msg.has_changed)
		for (GList *next = all_filer_windows; next; next = next->next)
	{
		GtkWidget *msg = ((FilerWindow *)next->data)->message;
		if (show_user_message && !(o_hide_root_msg.int_value && euid == 0))
			gtk_widget_show(msg);
		else
			gtk_widget_hide(msg);
	}

	if (all_filer_windows &&
			(o_display_show_dir_thumbs.has_changed ||
			o_jpeg_thumbs.has_changed ||
			o_pixmap_thumb_file_size.has_changed
			))
	{
		GList *next;
		for (next = all_filer_windows; next; next = next->next)
		{
			FilerWindow *fw = (FilerWindow *) next->data;

			filer_cancel_thumbnails(fw);
			filer_create_thumbs(fw, NULL);
		}
	}
}

/* Append interesting information to this GString */
void filer_add_tip_details(FilerWindow *filer_window,
			   GString *tip, DirItem *item)
{
	const guchar *fullpath = NULL;

	fullpath = make_path(filer_window->real_path, item->leafname);

	if (item->flags & ITEM_FLAG_SYMLINK)
	{
		char *target;

		target = readlink_dup(fullpath);
		if (target)
		{
			ensure_utf8(&target);

			g_string_append(tip, _("Symbolic link to "));
			g_string_append(tip, target);
			g_string_append_c(tip, '\n');
			g_free(target);
		}
	}

	if (item->flags & ITEM_FLAG_APPDIR)
	{
		XMLwrapper *info;
		xmlNode *node;

		info = appinfo_get(fullpath, item);
		if (info && ((node = xml_get_section(info, NULL, "Summary"))))
		{
			guchar *str;
			str = xmlNodeListGetString(node->doc,
					node->xmlChildrenNode, 1);
			if (str)
			{
				g_string_append(tip, str);
				g_string_append_c(tip, '\n');
				g_free(str);
			}
		}
		if (info)
			g_object_unref(info);
	}
	else if (item->mime_type == application_x_desktop)
	{
		char *summary;
		summary = tip_from_desktop_file(fullpath);
		if (summary)
		{
			g_string_append(tip, summary);
			g_string_append_c(tip, '\n');
			g_free(summary);
		}
	}

	if (!g_utf8_validate(item->leafname, -1, NULL))
		g_string_append(tip,
			_("This filename is not valid UTF-8. "
			  "You should rename it.\n"));
}

/* Return the selection as a text/uri-list.
 * g_free() the result.
 */
static guchar *filer_create_uri_list(FilerWindow *filer_window)
{
	GString	*string;
	GString	*leader;
	ViewIter iter;
	DirItem	*item;
	guchar	*retval;

	g_return_val_if_fail(filer_window != NULL, NULL);

	string = g_string_new(NULL);

	leader = g_string_new(filer_window->sym_path);
	if (leader->str[leader->len - 1] != '/')
		g_string_append_c(leader, '/');

	view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);
	while ((item = iter.next(&iter)))
	{
		EscapedPath *uri;
		char *path;

		path = g_strconcat(leader->str, item->leafname, NULL);
		uri = encode_path_as_uri(path);
		g_string_append(string, (char *) uri);
		g_string_append(string, "\r\n");
		g_free(path);
		g_free(uri);
	}

	g_string_free(leader, TRUE);
	retval = string->str;
	g_string_free(string, FALSE);

	return retval;
}

void filer_perform_action(FilerWindow *fw, GdkEventButton *event)
{
	BindAction	action;
	ViewIface	*view = fw->view;
	DirItem		*item = NULL;
	gboolean	press = event->type == GDK_BUTTON_PRESS;
	ViewIter	iter;
	OpenFlags	flags = 0;

	if (event->button > 3)
	{
		if (event->button == 8)
			change_to_parent(fw);
		if (event->button == 9 && bookmarks_get_recent())
				filer_change_to(fw, bookmarks_get_recent(), NULL);
		return;
	}

	view_get_iter_at_point(view, &iter, event->window, event->x, event->y);
	item = iter.peek(&iter);

	if (item && view_cursor_visible(view))
		view_cursor_to_iter(view, &iter);

	if (item && event->button == 1 &&
		view_get_selected(view, &iter) &&
		fw->selection_state == GTK_STATE_ACTIVE)
	{
		/* Possibly a really slow DnD operation? */
		fw->temp_item_selected = FALSE;

		filer_selection_changed(fw, event->time);
	}

	if (fw->target_cb)
	{
		dnd_motion_ungrab();
		if (item && press && event->button == 1)
			fw->target_cb(fw, &iter, fw->target_data);

		filer_target_mode(fw, NULL, NULL, NULL);

		return;
	}

	if (!o_single_click.int_value)
	{
		/* Make sure both parts of a double-click fall on
		 * the same file.
		 */
		static guchar *first_click = NULL;
		static guchar *second_click = NULL;

		if (event->type == GDK_BUTTON_PRESS)
		{
			g_free(first_click);
			first_click = second_click;

			if (item)
				second_click = g_strdup(item->leafname);
			else
				second_click = NULL;
		}

		if (event->type == GDK_2BUTTON_PRESS)
		{
			if (first_click && second_click &&
			    strcmp(first_click, second_click) != 0)
				return;
			if ((first_click || second_click) &&
			    !(first_click && second_click))
				return;
		}
	}

	action = bind_lookup_bev(
			item ? BIND_DIRECTORY_ICON : BIND_DIRECTORY,
			event);

	switch (action)
	{
		case ACT_CLEAR_SELECTION:
			view_clear_selection(view);
			break;
		case ACT_TOGGLE_SELECTED:
			view_set_selected(view, &iter,
				!view_get_selected(view, &iter));
			break;
		case ACT_SELECT_EXCL:
			view_select_only(view, &iter);
			break;

		case ACT_EDIT_ITEM_CLOSE:
			flags = OPEN_CLOSE_WINDOW;
			/* (no break) */
		case ACT_EDIT_ITEM:
			flags |= OPEN_SHIFT;
		case ACT_OPEN_ITEM_CLOSE:
			if (~flags & OPEN_SHIFT)
				flags = OPEN_CLOSE_WINDOW;
		case ACT_OPEN_ITEM:
			if (~flags & OPEN_CLOSE_WINDOW)
				flags |= OPEN_SAME_WINDOW;

			if (o_new_button_1.int_value)
				flags ^= OPEN_SAME_WINDOW;
			else if (item->base_type == TYPE_DIRECTORY)
			{
				gboolean ctrl = (event->state & GDK_CONTROL_MASK) != 0;

				if ((fw->right_link || fw->left_link))
				{
					if (fw->right_link &&
						!strcmp(g_strrstr(fw->right_link->sym_path, "/") + 1,
							item->leafname))
					{
						if (~flags & OPEN_SAME_WINDOW)
						{
							if (o_window_link.int_value != ctrl)
							{
								filer_cut_links(fw->right_link, -1);
								break;
							}
							else
								filer_cut_links(fw, 1);
						}
					}
					else
					{
						if (flags & OPEN_SAME_WINDOW)
							flags |= OPEN_LINK_WINDOW;
						else if (o_window_link.int_value != ctrl)
						{ //want win link again (middle button)
							filer_cut_links(fw->right_link, -1);
							flags |= OPEN_LINK_WINDOW;
						}
					}
				}
				else
				{
					if (~flags & OPEN_SAME_WINDOW &&
						o_window_link.int_value != ctrl)
						flags |= OPEN_LINK_WINDOW;
				}
			}

			if (event->type == GDK_2BUTTON_PRESS)
				view_set_selected(view, &iter, FALSE);
			dnd_motion_ungrab();

			filer_openitem(fw, &iter, flags);
			break;
		case ACT_POPUP_MENU:
			dnd_motion_ungrab();
			tooltip_show(NULL);
			show_filer_menu(fw,
					(GdkEvent *) event, &iter);
			break;
		case ACT_PRIME_AND_SELECT:
			if (item && !view_get_selected(view, &iter))
				view_select_only(view, &iter);
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_PRIME_AND_TOGGLE:
			view_set_selected(view, &iter,
				!view_get_selected(view, &iter));
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_PRIME_FOR_DND:
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_IGNORE:
			if (press && event->button < 4)
			{
				if (item)
					view_wink_item(view, &iter);
				dnd_motion_start(MOTION_NONE);
			}
			break;
		case ACT_LASSO_CLEAR:
			view_clear_selection(view);
			/* (no break) */
		case ACT_LASSO_MODIFY:
			view_start_lasso_box(view, event);
			break;
		case ACT_RESIZE:
			if (fw->directory->error)
			{
				dnd_motion_ungrab();
				char *current = g_strdup(fw->sym_path);
				do {
					char *parent = g_path_get_dirname(current);
					DIR *d;
					struct stat info;
					if (!mc_stat(parent, &info) && (d = mc_opendir(parent)))
					{
						g_free(parent);
						mc_closedir(d);
						break;
					}
					g_free(current);
					current = parent;
				} while (1);


				int len = strlen(current);
				for (GList *next = all_filer_windows; next;)
				{
					FilerWindow *lfw = (void *)next->data;
					next = next->next; //have to get before destroy

					if (!strncmp(current, lfw->sym_path, len))
					{
						char s = lfw->sym_path[len];
						if (s == '/' || s == '\0')
							gtk_widget_destroy(lfw->window);
					}
				}
				g_free(current);
			}
			else
				view_autosize(fw->view, TRUE);
			break;
		default:
			g_warning("Unsupported action : %d\n", action);
			break;
	}

	if (fw->mini_type == MINI_REG_SELECT)
		minibuffer_hide(fw);
}

/* It's time to make the tooltip appear. If we're not over the item any
 * more, or the item doesn't need a tooltip, do nothing.
 */
static gboolean tooltip_activate(GtkWidget *window)
{
	FilerWindow *filer_window;
	ViewIface *view;
	ViewIter iter;
	gint 	x, y;
	DirItem	*item = NULL;
	GString	*tip = NULL;

	g_return_val_if_fail(tip_item != NULL, 0);

	filer_window = g_object_get_data(G_OBJECT(window), "filer_window");

	if (!motion_window || !filer_window)
		return FALSE;	/* Window has been destroyed */

	view = filer_window->view;

	tooltip_show(NULL);

	gdk_window_get_pointer(motion_window, &x, &y, NULL);
	view_get_iter_at_point(view, &iter, motion_window, x, y);

	item = iter.peek(&iter);
	if (item != tip_item)
		return FALSE;	/* Not still under the pointer */

	/* OK, the filer window still exists and the pointer is still
	 * over the same item. Do we need to show a tip?
	 */

	tip = g_string_new(NULL);

	view_extend_tip(filer_window->view, &iter, tip);

	filer_add_tip_details(filer_window, tip, tip_item);

	if (tip->len > 1)
	{
		g_string_truncate(tip, tip->len - 1);

		tooltip_show(tip->str);
	}

	g_string_free(tip, TRUE);

	return FALSE;
}

/* Motion detected on the View widget */
gint filer_motion_notify(FilerWindow *filer_window, GdkEventMotion *event)
{
	ViewIface	*view = filer_window->view;
	ViewIter	iter;
	DirItem		*item;


	FilerWindow *fw = filer_window;
	static gdouble edragx = -1;
	static gdouble edragy = -1;
	if (view_count_items(view) == 0
	&&  event->state & GDK_BUTTON1_MASK
	&&  motion_state == MOTION_DISABLED
	&&  (fw->configured || !(o_auto_move.int_value || filer_window->reqx >= 0))
	) {
		gint x, y;
		gtk_window_get_position(GTK_WINDOW(fw->window), &x, &y);
		gtk_window_move(GTK_WINDOW(fw->window),
				x + event->x_root - edragx,
				y + event->y_root - edragy);

		focus_in_cb(NULL, NULL, fw);
	}
	edragx = event->x_root;
	edragy = event->y_root;


	view_get_iter_at_point(view, &iter, event->window, event->x, event->y);
	item = iter.peek(&iter);

	if (item)
	{
		if (motion_state != MOTION_READY_FOR_DND &&
				!(event->state & (GDK_BUTTON1_MASK | GDK_BUTTON2_MASK)))
			gdk_window_set_cursor(filer_window->window->window, hand_cursor);

		if (item != tip_item)
		{
			tooltip_show(NULL);

			tip_item = item;
			motion_window = event->window;
			tooltip_prime((GSourceFunc) tooltip_activate,
					G_OBJECT(filer_window->window));
		}
	}
	else
	{
		gdk_window_set_cursor(filer_window->window->window, NULL);

		tooltip_show(NULL);
		tip_item = NULL;
	}

	if (motion_state != MOTION_READY_FOR_DND)
		return FALSE;

	if (!dnd_motion_moved(event))
		return FALSE;

	view_get_iter_at_point(view, &iter,
			event->window,
			event->x - (event->x_root - drag_start_x),
			event->y - (event->y_root - drag_start_y));
	item = iter.peek(&iter);
	if (!item)
		return FALSE;

	view_wink_item(view, NULL);

	if (!view_get_selected(view, &iter))
	{
		/* If we drag an unselected item, select it only.
		 * Unless we're also holding down Ctrl, in which case
		 * it's probably unselected only because we
		 * mis-interpreted the click as toggle-selected.
		 */
		if ((event->state & GDK_BUTTON1_MASK) &&
		    !(event->state & GDK_CONTROL_MASK))
		{
			/* Select just this one */
			filer_window->temp_item_selected = TRUE;
			view_select_only(view, &iter);
		}
		else
		{
			if (view_count_selected(view) == 0)
				filer_window->temp_item_selected = TRUE;
			view_set_selected(view, &iter, TRUE);
		}
	}

	g_return_val_if_fail(view_count_selected(view) > 0, TRUE);

	if (view_count_selected(view) == 1)
	{
		if (item->base_type == TYPE_UNKNOWN)
			item = dir_update_item(filer_window->directory,
						item->leafname);

		if (!item)
		{
			report_error(_("Item no longer exists!"));
			return FALSE;
		}

		drag_one_item(GTK_WIDGET(view), event,
			make_path(filer_window->sym_path, item->leafname),
			item, di_image(item));
#if 0
		/* XXX: Use thumbnail */
			item, view ? view->image : NULL);
#endif
	}
	else
	{
		guchar *uris;

		uris = filer_create_uri_list(filer_window);
		drag_selection(GTK_WIDGET(view), event, uris);
		g_free(uris);
	}

	return FALSE;
}

static void drag_end(GtkWidget *widget, GdkDragContext *context,
		     FilerWindow *filer_window)
{
	filer_set_autoscroll(filer_window, FALSE);

	if (filer_window->temp_item_selected)
	{
		view_clear_selection(filer_window->view);
		filer_window->temp_item_selected = FALSE;
	}
}

/* Remove highlights */
static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window)
{
	dnd_spring_abort();
}

/* Called during the drag when the mouse is in a widget registered
 * as a drop target. Returns TRUE if we can accept the drop.
 */
static gboolean drag_motion(GtkWidget		*widget,
                            GdkDragContext	*context,
                            gint		x,
                            gint		y,
                            guint		time,
			    FilerWindow		*filer_window)
{
	DirItem		*item;
	ViewIface	*view = filer_window->view;
	ViewIter	iter;
	GdkDragAction	action = context->suggested_action;
	const guchar	*new_path = NULL;
	const char	*type = NULL;
	gboolean	retval = FALSE;
	gboolean	same_window;

	if ((context->actions & GDK_ACTION_ASK) && o_dnd_left_menu.int_value)
	{
		guint state;
		gdk_window_get_pointer(NULL, NULL, NULL, &state);
		if (state & GDK_BUTTON1_MASK)
			action = GDK_ACTION_ASK;
	}

	same_window = gtk_drag_get_source_widget(context) == widget;

	filer_set_autoscroll(filer_window, TRUE);

	if (filer_window->view_type == VIEW_TYPE_DETAILS)
	{
		GdkWindow *bin;
		int bin_y;
		/* Correct for position of bin window */
		bin = gtk_tree_view_get_bin_window(GTK_TREE_VIEW(view));
		gdk_window_get_position(bin, NULL, &bin_y);
		y -= bin_y;
	}

	if (o_dnd_drag_to_icons.int_value)
	{
		view_get_iter_at_point(view, &iter, widget->window, x, y);
		item = iter.peek(&iter);
	}
	else
		item = NULL;

	if (item && same_window && view_get_selected(view, &iter))
		type = NULL;
	else
		type = dnd_motion_item(context, &item);

	if (!type)
		item = NULL;

	/* Don't allow drops to non-writeable directories. BUT, still
	 * allow drops on non-writeable SUBdirectories so that we can
	 * do the spring-open thing.
	 */
	if (item && type == drop_dest_dir &&
			!(item->flags & ITEM_FLAG_APPDIR))
	{
		dnd_spring_load(context, filer_window);
	}
	else
		dnd_spring_abort();

	if (item)
		view_cursor_to_iter(view, &iter);
	else
	{
		view_cursor_to_iter(view, NULL);

		/* Disallow background drops within a single window */
		if (type && same_window)
			type = NULL;
	}

	if (type)
	{
		if (item)
			new_path = make_path(filer_window->sym_path,
					     item->leafname);
		else
			new_path = filer_window->sym_path;
	}

	/* Don't ask about dragging to an application! */
	if (type == drop_dest_prog && action == GDK_ACTION_ASK)
		action = GDK_ACTION_COPY;

	g_dataset_set_data(context, "drop_dest_type", (gpointer) type);
	if (type)
	{
		gdk_drag_status(context, action, time);
		g_dataset_set_data_full(context, "drop_dest_path",
					g_strdup(new_path), g_free);
		retval = TRUE;
	}

	return retval;
}

static gboolean as_timeout(FilerWindow *filer_window)
{
	gboolean retval;

	retval = view_auto_scroll_callback(filer_window->view);

	if (!retval)
		filer_window->auto_scroll = -1;

	return retval;
}

/* When autoscroll is on, a timer keeps track of the pointer position.
 * While it's near the top or bottom of the window, the window scrolls.
 *
 * If the mouse buttons are released, the pointer leaves the window, or
 * a drag-and-drop operation finishes, auto_scroll is turned off.
 */
void filer_set_autoscroll(FilerWindow *filer_window, gboolean auto_scroll)
{
	g_return_if_fail(filer_window != NULL);

	if (auto_scroll)
	{
		dnd_autoscrollfw = filer_window;
		if (filer_window->auto_scroll != -1)
			return;		/* Already on! */

		filer_window->auto_scroll = g_timeout_add(50,
					(GSourceFunc) as_timeout,
					filer_window);
	}
	else
	{
		dnd_autoscrollfw = NULL;
		if (filer_window->auto_scroll == -1)
			return;		/* Already off! */

		g_source_remove(filer_window->auto_scroll);
		filer_window->auto_scroll = -1;
	}
}

#define ZERO_MNT "/uri/0install"

static void refresh_done(FilerWindow *filer_window)
{
	if (filer_exists(filer_window))
		filer_update_dir(filer_window, TRUE);
}

void filer_refresh(FilerWindow *filer_window)
{
	if (!strncmp(ZERO_MNT "/", filer_window->real_path, sizeof(ZERO_MNT)))
	{
		/* Try to run 0refresh */
		gint pid;
		gchar *argv[] = {"0refresh", NULL, NULL};
		const char *host = filer_window->real_path + sizeof(ZERO_MNT);
		const char *slash;

		slash = strchr(host, '/');
		if (slash)
			argv[1] = g_strndup(host, slash - host);
		else
			argv[1] = g_strdup(host);
		pid = rox_spawn(filer_window->real_path, (const char **) argv);
		g_free(argv[1]);
		if (pid)
			on_child_death(pid, (CallbackFn) refresh_done,
					filer_window);
	}

	full_refresh();
}

void filer_refresh_thumbs(FilerWindow *filer_window)
{
	ViewIter iter;
	DirItem *item;

	g_mutex_lock(&m_dirthumb);
	g_mutex_unlock(&m_dirthumb);

	filer_cancel_thumbnails(filer_window);

	if (!sdinfo.cancel && sdinfo.items->len)
		while (make_dir_thumb_link()) {}
	else
		free_subdir_info();

	set_scanning_display(filer_window, TRUE);

	char *thumb_path = pixmap_make_thumb_path(filer_window->real_path);
	unlink(thumb_path); ///////////////////////////
	g_free(thumb_path);

	view_get_iter(filer_window->view, &iter, 0);
	while ((item = iter.next(&iter)))
	{
		if (item->base_type != TYPE_FILE &&
				(o_display_show_dir_thumbs.int_value != 1 ||
				 item->base_type != TYPE_DIRECTORY))
			 continue;

		guchar *path = g_strdup(
				make_path(filer_window->real_path, item->leafname));

		g_fscache_remove(pixmap_cache, path);
		g_fscache_remove(thumb_cache, path);

		thumb_path = pixmap_make_thumb_path(path);
		unlink(thumb_path); ///////////////////////////

		dir_force_update_path(path, TRUE);

		if (!filer_window->show_thumbs)
			; //do nothing
		else if (item->base_type == TYPE_DIRECTORY)
		{
			free_subdir_info();
			sdinfo.fw = filer_window;
			sdinfo.thumb_path = g_strdup(thumb_path);
			sdinfo.path = g_strdup(path);

			g_mutex_lock(&m_dirthumb);
			GThread *scd = g_thread_new("scandir_t", scandir_thread, NULL);
			g_thread_join(scd);
			while (make_dir_thumb_link()) {}
		}
		else
			filer_create_thumb(filer_window, path);

		g_free(thumb_path);
		g_free(path);
	}

	set_scanning_display(filer_window, FALSE);
	if (!g_queue_is_empty(filer_window->thumb_queue))
		start_thumb_scanning(filer_window);
}

static inline gboolean is_hidden(const char *dir, DirItem *item)
{
	/* If the leaf name starts with '.' then the item is hidden */
	if(item->leafname[0]=='.')
		return TRUE;

	/*** Test disabled for now.  The flags aren't set on the first pass...
	 */
#if 0
	/* Most files will not have extended attributes, so this should
	* be quick. */
	if(!o_xattr_ignore.int_value && (item->flags & ITEM_FLAG_HAS_XATTR)) {
		gchar *path, *val;
		int len;
		gboolean hidden=FALSE;

		path=g_build_filename(dir, item->leafname, NULL);
		val=xattr_get(path, XATTR_HIDDEN, &len);
		if(val) {
			hidden=atoi(val) || (strcmp(val, "true")==0);
			g_free(val);
		}
		g_free(path);

		if(hidden)
			return TRUE;
	}
#endif

	/* Otherwise not hidden */
	return FALSE;
}

gboolean filer_match_filter(FilerWindow *filer_window, DirItem *item)
{
	g_return_val_if_fail(item != NULL, FALSE);

	if (filer_window->files_only &&
			item->base_type == TYPE_DIRECTORY)
		return FALSE;

	if (filer_window->dirs_only &&
			item->base_type != TYPE_DIRECTORY)
		return FALSE;

	if(is_hidden(filer_window->real_path, item) &&
	   (!filer_window->temp_show_hidden && !filer_window->show_hidden))
		return FALSE;

	switch(filer_window->filter) {
	case FILER_SHOW_GLOB:
		if (!(
			fnmatch(filer_window->filter_string,
				item->leafname, 0) == 0 ||
			(item->base_type == TYPE_DIRECTORY &&
				!filer_window->filter_directories)
		))
			return FALSE;

	case FILER_SHOW_ALL:
	default:
		break;
	}

	if (filer_window->regexp &&
		regexec((regex_t *) filer_window->regexp,
			item->leafname, 0, NULL, 0) != 0
	)
		return FALSE;

	return TRUE;
}

/* Provided to hide the implementation */
void filer_set_hidden(FilerWindow *filer_window, gboolean hidden)
{
	filer_window->show_hidden=hidden;
}

/* Provided to hide the implementation */
void filer_set_filter_directories(FilerWindow *filer_window,
        gboolean filter_directories)
{
	filer_window->filter_directories = filter_directories;
}

/* Set the filter type. Returns TRUE if the type has changed
 * (must call filer_detach_rescan).
 */
gboolean filer_set_filter(FilerWindow *filer_window, FilterType type,
			     const gchar *filter_string)
{
	/* Is this new filter the same as the old one? */
	if (filer_window->filter == type)
	{
		switch(filer_window->filter)
		{
		case FILER_SHOW_ALL:
			return FALSE;
		case FILER_SHOW_GLOB:
			if (strcmp(filer_window->filter_string,
				   filter_string) == 0)
				return FALSE;
			break;
		}
	}

	/* Clean up old filter */
	if (filer_window->filter_string)
	{
		g_free(filer_window->filter_string);
		filer_window->filter_string = NULL;
	}

	filer_window->filter = type;

	switch(type)
	{
	case FILER_SHOW_ALL:
		/* No extra work */
		break;

	case FILER_SHOW_GLOB:
		filer_window->filter_string = g_strdup(filter_string);
		break;

	default:
		/* oops */
		filer_window->filter = FILER_SHOW_ALL;
		g_warning("Impossible: filter type %d", type);
		break;
	}

	return TRUE;
}

/* Setting stuff */
static Settings *settings_new(const char *path)
{
	Settings *set;

	set=g_new(Settings, 1);
	memset(set, 0, sizeof(Settings));
	if(path)
		set->path=g_strdup(path);

	return set;
}

static void settings_free(Settings *set)
{
	g_free(set->path);
	if(set->filter)
		g_free(set->filter);
	g_free(set);
}

static void store_settings(Settings *set)
{
	Settings *old;

	old=g_hash_table_lookup(settings_table, set->path);
	if(old)
	{
		g_hash_table_remove(settings_table, set->path);
		settings_free(old);
	}

	g_hash_table_insert(settings_table, set->path, set);
}

/* TODO: use symbolic names in the XML file where possible */
static void load_from_node(Settings *set, xmlDocPtr doc, xmlNodePtr node)
{
	xmlChar *str=NULL;

	str=xmlNodeListGetString(doc, node->xmlChildrenNode, 1);

	if(strcmp(node->name, "X") == 0) {
		set->x=atoi(str);
		set->flags|=SET_POSITION;
	} else if(strcmp(node->name, "Y") == 0) {
		set->y=atoi(str);
		set->flags|=SET_POSITION;
	} else if(strcmp(node->name, "Width") == 0) {
		set->width=atoi(str);
		set->flags|=SET_SIZE;
	} else if(strcmp(node->name, "Height") == 0) {
		set->height=atoi(str);
		set->flags|=SET_SIZE;
	} else if(strcmp(node->name, "ShowHidden") == 0) {
		set->show_hidden=atoi(str);
		set->flags|=SET_HIDDEN;
	} else if(strcmp(node->name, "ViewType") == 0) {
		set->view_type=atoi(str);
		set->flags|=SET_DETAILS;
	} else if(strcmp(node->name, "DetailsType") == 0) {
		set->details_type=atoi(str);
		set->flags|=SET_DETAILS;
	} else if(strcmp(node->name, "SortType") == 0) {
		set->sort_type=atoi(str);
		set->flags|=SET_SORT;
	} else if(strcmp(node->name, "SortOrder") == 0) {
		set->sort_order=atoi(str);
		set->flags|=SET_SORT;
	} else if(strcmp(node->name, "DisplayStyle") == 0) {
		set->display_style=atoi(str);
		set->flags|=SET_STYLE;
	} else if(strcmp(node->name, "ShowThumbs") == 0) {
		set->show_thumbs=atoi(str);
		set->flags|=SET_THUMBS;
	} else if(strcmp(node->name, "FilterType") == 0) {
		set->filter_type=atoi(str);
		set->flags|=SET_FILTER;
	} else if(strcmp(node->name, "Filter") == 0) {
		set->filter=g_strdup(str);
		set->flags|=SET_FILTER;
	} else if(strcmp(node->name, "FilterDirectories") == 0) {
		set->filter_directories=atoi(str);
		set->flags|=SET_FILTER;
	}

	if(str)
		xmlFree(str);
}

static void load_learnt_mounts(void)
{
	gchar *path;
	gchar *buffer = NULL;
	gsize len = 0;
	gchar **entries;
	int n;

	unmount_prompt_actions = g_hash_table_new_full(g_str_hash,
			g_str_equal, g_free, NULL);

	path = choices_find_xdg_path_load("Mounts", PROJECT, SITE);
	if (!path)
		return;
	if (!g_file_get_contents(path, &buffer, &len, NULL))
	{
		g_free(path);
		return;
	}
	g_free(path);
	if (len)
	{
		buffer[len - 1] = 0;
	}
	else
	{
		g_free(buffer);
		return;
	}

	entries = g_strsplit(buffer, "\n", 0);
	g_free(buffer);
	for (n = 0; entries[n]; ++n)
	{
		gchar *p;
		buffer = entries[n];
		p = strrchr(buffer, ' ');
		if (p && p > buffer) {
			*p = 0;
			g_hash_table_insert(unmount_prompt_actions, g_strdup(buffer),
				GINT_TO_POINTER(atoi(p+1)));
		}
	}
	g_strfreev(entries);
}

static void save_mount(char *path, gpointer value, FILE **pfp)
{
	int v;

	if (!*pfp)
	{
		gchar *spath = choices_find_xdg_path_save("Mounts",
				PROJECT, SITE, TRUE);

		if (!spath)
			return;
		*pfp = fopen(spath, "w");
		g_free(spath);
		if (!*pfp)
			return;
	}

	if ((v = GPOINTER_TO_INT(value)) != UNMOUNT_PROMPT_ASK)
		fprintf(*pfp, "%s %d\n", path, v);
}

static void save_learnt_mounts(void)
{
	FILE *fp = NULL;

	/* A GHashTableIter would be easier, but it's a relatively new feature */
	if (unmount_prompt_actions)
		g_hash_table_foreach(unmount_prompt_actions, (GHFunc) save_mount, &fp);
	if (fp)
		fclose(fp);
}

static void load_settings(void)
{
	gchar *path;
	XMLwrapper *settings_doc=NULL;

	path=choices_find_xdg_path_load("Settings.xml", PROJECT, SITE);
	if(path) {
		settings_doc=xml_new(path);
		g_free(path);
	}

	if(!settings_table)
		settings_table=g_hash_table_new(g_str_hash, g_str_equal);

	if(settings_doc) {
		xmlNodePtr node, subnode;

		node = xmlDocGetRootElement(settings_doc->doc);

		for (node = node->xmlChildrenNode; node; node = node->next)
		{
			Settings *set;
			xmlChar *path;

			if (node->type != XML_ELEMENT_NODE)
				continue;
			if (strcmp(node->name, "FilerWindow") != 0)
				continue;

			path=xmlGetProp(node, "path");
			set=settings_new(path);
			xmlFree(path);

			for (subnode=node->xmlChildrenNode; subnode;
			     subnode=subnode->next) {

				if (subnode->type != XML_ELEMENT_NODE)
					continue;
				load_from_node(set, settings_doc->doc,
					       subnode);
			}

			store_settings(set);
		}
		g_object_unref(settings_doc);
	}
}

static void add_nodes(gpointer key, gpointer value, gpointer data)
{
	xmlNodePtr node=(xmlNodePtr) data;
	xmlNodePtr sub;
	Settings *set=(Settings *) value;
	char *tmp;

	sub=xmlNewChild(node, NULL, "FilerWindow", NULL);

	xmlSetProp(sub, "path", set->path);

	if(set->flags & SET_POSITION) {
		tmp=g_strdup_printf("%d", set->x);
		xmlNewChild(sub, NULL, "X", tmp);
		g_free(tmp);
		tmp=g_strdup_printf("%d", set->y);
		xmlNewChild(sub, NULL, "Y", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_SIZE) {
		tmp=g_strdup_printf("%d", set->width);
		xmlNewChild(sub, NULL, "Width", tmp);
		g_free(tmp);
		tmp=g_strdup_printf("%d", set->height);
		xmlNewChild(sub, NULL, "Height", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_HIDDEN) {
		tmp=g_strdup_printf("%d", set->show_hidden);
		xmlNewChild(sub, NULL, "ShowHidden", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_STYLE) {
		tmp=g_strdup_printf("%d", set->display_style);
		xmlNewChild(sub, NULL, "DisplayStyle", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_SORT) {
		tmp=g_strdup_printf("%d", set->sort_type);
		xmlNewChild(sub, NULL, "SortType", tmp);
		g_free(tmp);
		tmp=g_strdup_printf("%d", set->sort_order);
		xmlNewChild(sub, NULL, "SortOrder", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_DETAILS) {
		tmp=g_strdup_printf("%d", set->view_type);
		xmlNewChild(sub, NULL, "ViewType", tmp);
		g_free(tmp);
		tmp=g_strdup_printf("%d", set->details_type);
		xmlNewChild(sub, NULL, "DetailsType", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_THUMBS) {
		tmp=g_strdup_printf("%d", set->show_thumbs);
		xmlNewChild(sub, NULL, "ShowThumbs", tmp);
		g_free(tmp);
	}
	if(set->flags & SET_FILTER) {
		tmp=g_strdup_printf("%d", set->filter_type);
		xmlNewChild(sub, NULL, "FilterType", tmp);
		g_free(tmp);
		if(set->filter && set->filter[0])
			xmlNewChild(sub, NULL, "Filter", set->filter);
		tmp=g_strdup_printf("%d", set->filter_directories);
		xmlNewChild(sub, NULL, "FilterDirectories", tmp);
	}
}

static void save_settings(void)
{
	gchar *path;

	path=choices_find_xdg_path_save("Settings.xml", PROJECT, SITE, TRUE);
	if(path) {
		xmlDocPtr doc = xmlNewDoc("1.0");
		xmlDocSetRootElement(doc, xmlNewDocNode(doc, NULL,
							"Settings", NULL));

		g_hash_table_foreach(settings_table, add_nodes,
					    xmlDocGetRootElement(doc));

		save_xml_file(doc, path);

		g_free(path);
		xmlFreeDoc(doc);
	}

}

void filer_copy_settings(FilerWindow *src, FilerWindow *dest)
{
	dest->sort_type            = src->sort_type;
	dest->sort_order           = src->sort_order;
	dest->display_style_wanted = src->display_style_wanted;
	dest->details_type         = src->details_type;
	dest->show_hidden          = src->show_hidden;
	dest->show_thumbs          = src->show_thumbs;
	dest->view_type            = src->view_type;
	dest->icon_scale           = src->icon_scale;
	dest->name_scale           = 1.0;

	dest->filter_directories   = src->filter_directories;
	filer_set_filter(dest, src->filter, src->filter_string);
}
void filer_clear_settings(FilerWindow *fw)
{
	fw->sort_type            = o_display_sort_by.int_value;
	fw->sort_order           = GTK_SORT_ASCENDING;
	fw->display_style_wanted = o_display_size.int_value;
	fw->details_type         = o_display_details.int_value;
	fw->show_hidden          = o_display_show_hidden.int_value;
	fw->show_thumbs          = o_display_show_thumbs.int_value;
	fw->view_type            = o_filer_view_type.int_value;
	fw->icon_scale           = 1.0;
	fw->name_scale           = 1.0;

	fw->filter_directories   = FALSE;
	filer_set_filter(fw, FILER_SHOW_ALL, NULL);
}
static gboolean check_settings(FilerWindow *filer_window, gboolean onlycheck)
{
	Settings *set;
	gboolean force_resize = FALSE;
	DisplayStyle dstyle = filer_window->display_style_wanted;
	ViewType    vtype = filer_window->view_type;
	DetailsType dtype = filer_window->details_type;
	gchar *current, *moved = NULL, *status;

	set = (Settings *) g_hash_table_lookup(settings_table,
			filer_window->sym_path);

	status = set ? _("▼") : _("▽");

	current = filer_window->sym_path;
	while (!set && !(current[0] == '/' && current[1] == '\0'))
	{
		current = g_path_get_dirname(current);

		set = (Settings *) g_hash_table_lookup(settings_table,
				make_path(current , "*"));

		if (set)
			status = moved ? _("▷") : _("▶");

		g_free(moved);
		moved = current;
	}
	g_free(moved);

	if (filer_window->toolbar_settings_text)
	{
		gtk_label_set_text(filer_window->toolbar_settings_text, status);
	}

	if (onlycheck)
		return FALSE;


	/* shared init (open and change to) */
	filer_window->dirs_only = FALSE;
	filer_window->files_only = FALSE;

	filer_window->reqx = filer_window->reqy = -1;
	filer_window->req_width = filer_window->req_height = -1;

	if (!set)
		goto out;

	if (set->flags & SET_HIDDEN)
		filer_window->show_hidden = set->show_hidden;

	if (set->flags & SET_STYLE)
		filer_window->display_style_wanted = set->display_style;

	if (set->flags & SET_DETAILS)
	{
		filer_window->details_type = set->details_type;

		if (vtype != set->view_type)
			filer_set_view_type(filer_window, set->view_type);
	}

	if (set->flags & SET_SORT)
	{
		filer_window->sort_type = set->sort_type;
		filer_window->sort_order = set->sort_order;
	}

	if (set->flags & SET_THUMBS)
		filer_window->show_thumbs = set->show_thumbs;

	if (set->flags & SET_FILTER)
	{
		filer_set_filter(filer_window,
				   set->filter_type,
				   set->filter);
		filer_set_filter_directories(filer_window,
					 set->filter_directories);
	}


	if (o_filer_auto_resize.int_value == RESIZE_STYLE &&
		(filer_window->display_style_wanted != dstyle ||
		 filer_window->view_type    != vtype ||
		 filer_window->details_type != dtype))
		force_resize = TRUE;

	if (set->flags & SET_POSITION)
	{
		filer_window->reqx = set->x;
		filer_window->reqy = set->y;

		if (gtk_widget_get_visible(filer_window->window) &&
			o_filer_auto_resize.int_value != RESIZE_ALWAYS &&
			!(set->flags & SET_SIZE) &&
			!force_resize)
		{ /* This case, there is no resize(move) request. */
			/* Keep current size */
			gtk_window_get_size(GTK_WINDOW(filer_window->window),
					&(filer_window->req_width),
					&(filer_window->req_height));

			force_resize = TRUE;
		}
	}

	if (set->flags & SET_SIZE)
	{
		force_resize = TRUE;
	}

out:
	filer_window->display_style =
		filer_window->display_style_wanted == AUTO_SIZE_ICONS ?
			SMALL_ICONS : filer_window->display_style_wanted;

	return force_resize;
}

typedef struct settings_window {
	GtkWidget *window;
	gboolean parent;
	GtkWidget *pos, *size, *hidden, *style, *sort, *details,
			  *thumbs, *filter;

	Settings *set;
} SettingsWindow;


static void settings_response(GtkWidget *window, gint response,
			      SettingsWindow *set_win)
{
	if(response==GTK_RESPONSE_OK) {
		gint flags=0;

		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->pos)))
			flags|=SET_POSITION;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->size)))
			flags|=SET_SIZE;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->hidden)))
			flags|=SET_HIDDEN;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->style)))
			flags|=SET_STYLE;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->sort)))
			flags|=SET_SORT;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->details)))
			flags|=SET_DETAILS;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->thumbs)))
			flags|=SET_THUMBS;
		if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(set_win->filter)))
			flags|=SET_FILTER;

		set_win->set->flags=flags;
		store_settings(set_win->set);
		save_settings();
	} else if(response==GTK_RESPONSE_NO) {
		Settings *old=g_hash_table_lookup(settings_table, set_win->set->path);
		if(old) {
			g_hash_table_remove(settings_table, set_win->set->path);
			settings_free(old);
		}
		save_settings();
	}

	if (response==GTK_RESPONSE_OK || response==GTK_RESPONSE_NO)
	{
		GList *next;
		gchar *parent = NULL;
		int len = 0;

		if (set_win->parent) {
			parent = g_path_get_dirname(set_win->set->path);
			len = strlen(parent);
		}

		for (next = all_filer_windows; next; next = next->next) {
			FilerWindow *fw = (FilerWindow *) next->data;

			if ((set_win->parent && strncmp(parent, fw->sym_path, len) == 0) ||
				(strcmp(set_win->set->path, fw->sym_path) == 0))
					check_settings(fw, TRUE);
		}

		if (set_win->parent)
			g_free(parent);
	}


	gtk_widget_destroy(window);
}

void filer_save_settings(FilerWindow *fwin, gboolean parent)
{
	SettingsWindow *set_win;
	GtkWidget *vbox, *frame;
	GtkWidget *path, *lbl;
	gint x, y;
	gchar *target = fwin->sym_path;

	if (parent)
	{
		gchar *tmp = g_path_get_dirname(target);
		target = g_strdup(make_path(tmp , "*"));
		g_free(tmp);
	}

	Settings *set=settings_new(target);
	Settings *bs = (Settings *) g_hash_table_lookup(
					settings_table,
					target);

	gtk_window_get_position(GTK_WINDOW(fwin->window),&x, &y);
	set->x=x;
	set->y=y;

	gtk_window_get_size(GTK_WINDOW(fwin->window),&x, &y);
	set->width=x;
	set->height=y;

	set->show_hidden=fwin->show_hidden;

	set->display_style=fwin->display_style_wanted;

	set->sort_type=fwin->sort_type;
	set->sort_order=fwin->sort_order;

	set->view_type=fwin->view_type;
	set->details_type=fwin->details_type;

	set->show_thumbs=fwin->show_thumbs;

	set->filter_type=fwin->filter;
	if(fwin->filter_string)
		set->filter=g_strdup(fwin->filter_string);
	set->filter_directories=fwin->filter_directories;

	if (bs)
		set->flags=bs->flags;

	/* Store other parameters
	*/
	set_win=g_new(SettingsWindow, 1);

	set_win->parent = parent;

	set_win->window=gtk_dialog_new();
	number_of_windows++;

	gtk_window_set_position(GTK_WINDOW(set_win->window),
			GTK_WIN_POS_MOUSE);

	gtk_dialog_add_button(GTK_DIALOG(set_win->window),
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL);

	if (bs)
		gtk_dialog_add_button(GTK_DIALOG(set_win->window),
				GTK_STOCK_DELETE, GTK_RESPONSE_NO);

	gtk_dialog_add_button(GTK_DIALOG(set_win->window),
			GTK_STOCK_OK, GTK_RESPONSE_OK);

	g_signal_connect(set_win->window, "destroy",
			 G_CALLBACK(one_less_window), NULL);
	g_signal_connect_swapped(set_win->window, "destroy",
			 G_CALLBACK(g_free), set_win);

	gtk_window_set_title(GTK_WINDOW(set_win->window),
			     _("Select display properties to save"));

	vbox=GTK_DIALOG(set_win->window)->vbox;

	lbl=gtk_label_new(parent ?
			_("<b>Save display settings for parent directory/*</b>") :
			_("<b>Save display settings for directory</b>"));
	gtk_label_set_use_markup(GTK_LABEL(lbl), TRUE);
	gtk_box_pack_start(GTK_BOX(vbox), lbl, FALSE, FALSE, 2);

	path=gtk_label_new(set->path);
	gtk_box_pack_start(GTK_BOX(vbox), path, FALSE, FALSE, 2);

	frame=gtk_frame_new(_("Select settings to save"));
	gtk_box_pack_start(GTK_BOX(vbox), frame, TRUE, TRUE, 2);

	/*Make new vbox to go in the frame */
	vbox=gtk_vbox_new(FALSE, 2);
	gtk_container_add(GTK_CONTAINER(frame), vbox);

	set_win->pos=gtk_check_button_new_with_label(_("Position"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->pos, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->pos),
				     set->flags & SET_POSITION);

	set_win->size=gtk_check_button_new_with_label(_("Window size"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->size, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->size),
				     set->flags & SET_SIZE);

	set_win->hidden=gtk_check_button_new_with_label(_("Show hidden"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->hidden,
			   FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->hidden),
				     set->flags & SET_HIDDEN);

	set_win->style=gtk_check_button_new_with_label(_("Display style (Icon size)"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->style,
			   FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->style),
				     set->flags & SET_STYLE);

	set_win->sort=gtk_check_button_new_with_label(_("Sort type and order"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->sort, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->sort),
				     set->flags & SET_SORT);

	set_win->details=gtk_check_button_new_with_label(_("Details"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->details, FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->details),
				     set->flags & SET_DETAILS);

	set_win->thumbs=gtk_check_button_new_with_label(_("Thumbnails"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->thumbs,
			   FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->thumbs),
				     set->flags & SET_THUMBS);

	set_win->filter=gtk_check_button_new_with_label(_("Filter"));
	gtk_box_pack_start(GTK_BOX(vbox), set_win->filter,
			   FALSE, FALSE, 2);
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(set_win->filter),
				     set->flags & SET_FILTER);

	set_win->set=set;
	g_signal_connect(set_win->window, "response",
			 G_CALLBACK(settings_response), set_win);

	gtk_widget_show_all(set_win->window);

	if (parent)
		g_free(target);
}

static char *tip_from_desktop_file(const char *full_path)
{
	GError *error = NULL;
	char *comment = NULL;

	comment = get_value_from_desktop_file(full_path,
			"Desktop Entry", "Comment", &error);
	if (error)
	{
		delayed_error("Failed to parse .desktop file '%s':\n%s",
				full_path, error->message);
		goto err;
	}

err:
	if (error != NULL)
		g_error_free(error);

	return comment;
}

UnmountPrompt filer_get_unmount_action(const char *path)
{
	return GPOINTER_TO_INT(g_hash_table_lookup(unmount_prompt_actions, path));
}

void filer_set_unmount_action(const char *path, UnmountPrompt action)
{
    g_hash_table_insert(unmount_prompt_actions, g_strdup(path),
            GINT_TO_POINTER(action));
    save_learnt_mounts();
}

void filer_send_event_to_view(FilerWindow *fw, GdkEvent * event)
{
	((GdkEventAny *) event)->send_event = TRUE;
	if (fw->view_type == VIEW_TYPE_COLLECTION)
		gtk_widget_event(GTK_WIDGET(
			VIEW_COLLECTION(fw->view)->collection), event);
	else
		gtk_widget_event(GTK_WIDGET(fw->view), event);
}
