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

/* toolbar.c - for the button bars that go along the tops of windows */

#include "config.h"

#include <string.h>

#include "global.h"

#include "toolbar.h"
#include "options.h"
#include "support.h"
#include "main.h"
#include "menu.h"
#include "dnd.h"
#include "filer.h"
#include "display.h"
#include "pixmaps.h"
#include "bind.h"
#include "type.h"
#include "dir.h"
#include "diritem.h"
#include "view_iface.h"
#include "bookmarks.h"
#include "gui_support.h"

typedef struct _Tool Tool;

typedef enum {DROP_NONE, DROP_TO_PARENT, DROP_TO_HOME, DROP_BOOKMARK} DropDest;

struct _Tool {
	const gchar	*label;
	const gchar	*name;
	const gchar	*tip;		/* Tooltip */
	void		(*clicked)(GtkWidget *w, FilerWindow *filer_window);
	DropDest	drop_action;
	gboolean	enabled;
	gboolean	menu;		/* Activate on button-press */
};

Option o_toolbar, o_toolbar_info, o_toolbar_disable;
Option o_toolbar_min_width;
int toolbar_min_width = 100;

/* TRUE if the button presses (or released) should open a new window,
 * rather than reusing the existing one.
 */
#define NEW_WIN_BUTTON(button_i) \
  (o_new_button_1.int_value \
	? button_i == 1 \
	: button_i != 1)

/* Static prototypes */
static void toolbar_close_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_up_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_home_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_bookmarks_clicked(GtkWidget *widget,
				      FilerWindow *filer_window);
static void toolbar_help_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_settings_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_refresh_clicked(GtkWidget *widget,
				    FilerWindow *filer_window);
static void toolbar_size_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_autosize_clicked(GtkWidget *widget, FilerWindow *filer_window);
static void toolbar_details_clicked(GtkWidget *widget,
				    FilerWindow *filer_window);
static void toolbar_hidden_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_dirs_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_select_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_new_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static void toolbar_sort_clicked(GtkWidget *widget,
				   FilerWindow *filer_window);
static GtkWidget *add_button(GtkWidget *bar, Tool *tool,
				FilerWindow *filer_window);
static void create_toolbar(GtkWidget *bar, FilerWindow *filer_window);
static gboolean drag_motion(GtkWidget		*widget,
                            GdkDragContext	*context,
                            gint		x,
                            gint		y,
                            guint		time,
			    FilerWindow		*filer_window);
static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window);
static void handle_drops(FilerWindow *filer_window,
			 GtkWidget *button,
			 DropDest dest);
static void toggle_selected(GtkToggleButton *widget, gpointer data);
static void option_notify(void);
static GList *build_tool_options(Option *option, xmlNode *node, guchar *label);

static Tool all_tools[] = {
	{N_("Close"), GTK_STOCK_CLOSE, N_("Close filer window"),
	 toolbar_close_clicked, DROP_NONE, FALSE,
	 FALSE},

	{N_("Up"), GTK_STOCK_GO_UP, N_("Change to parent directory\n"
						"  Right: Open parent directory\n"
						"  Middle: Change to parent in real path"),
	 toolbar_up_clicked, DROP_TO_PARENT, TRUE,
	 FALSE},

	{N_("Home"), GTK_STOCK_HOME, N_("Change to home directory\n"
						"  Right: Open home directory\n"
						"  Middle: Change to first bookmark"),
	 toolbar_home_clicked, DROP_TO_HOME, TRUE,
	 FALSE},

	{N_("Jump"), ROX_STOCK_BOOKMARKS, N_("Bookmarks menu\n"
						"  Right: Edit Bookmarks\n"
						"  Middle: Jump to last visited directory"),
	 toolbar_bookmarks_clicked, DROP_BOOKMARK, FALSE,
	 TRUE},

	{N_("Scan"), GTK_STOCK_REFRESH, N_("Rescan directory contents\n"
						"  Middle: Delete/re-create thumbnail cache"),
	 toolbar_refresh_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("Size┼"), GTK_STOCK_ZOOM_IN, N_("Change icon size\n"
						"  Right: Change to smaller\n"
						"  Middle: Change to Auto Size\n"
						"  Scroll: Temporary huge zoom\n"
						"Current size:\n"
						"  ┘ : Huge\n"
						"  ┤ : Large\n"
						"  ┐ : Small\n"
						"  ┌, ├ : Auto"),
	 toolbar_size_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("Auto"), GTK_STOCK_ZOOM_FIT, N_("Automatic size mode"),
	 toolbar_autosize_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("List"), ROX_STOCK_SHOW_DETAILS, N_("Show extra details\n"
						"  Right: Rotate Icons with details\n"
						"  Middle: Return to normal Icons View"),
	 toolbar_details_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("Sort"), GTK_STOCK_SORT_ASCENDING, N_("Change sort criteria"),
	 toolbar_sort_clicked, DROP_NONE, FALSE,
	 FALSE},

	{N_("Hide"), ROX_STOCK_SHOW_HIDDEN, N_("Left: Show/hide hidden files\n"
						 "Right: Show/hide thumbnails"),
	 toolbar_hidden_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("Dirs"), GTK_STOCK_DIRECTORY, N_("Left: Show only directories\n"
						 "Right: Show only files"),
	 toolbar_dirs_clicked, DROP_NONE, FALSE,
	 FALSE},

	{N_("Select"), GTK_STOCK_SELECT_ALL, N_("Left: Select all\n"
						"Right: Invert selection"),
	 toolbar_select_clicked, DROP_NONE, FALSE,
	 FALSE},

	{N_("New"), GTK_STOCK_ADD, N_("Left: New Directory\n"
								  "Middle: New Blank file\n"
								  "Right: Menu"),
	 toolbar_new_clicked, DROP_NONE, FALSE,
	 FALSE},

	{N_("○"), GTK_STOCK_SAVE, N_("Save Current Display Settings...\n"
						"  Right: for parent/* \n"
						"  Middle: Clear to default settings\n"
						"Under:\n"
						"  ▽: No settings\n"
						"  ▼: Own settings\n"
						"  ▶: Parent settings\n"
						"  ▷: Far parent settings"
						),
	 toolbar_settings_clicked, DROP_NONE, TRUE,
	 FALSE},

	{N_("Help"), GTK_STOCK_HELP, N_("Show ROX-Filer help"),
	 toolbar_help_clicked, DROP_NONE, TRUE,
	 FALSE},
};


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void toolbar_init(void)
{
	option_add_int(&o_toolbar, "toolbar_type", TOOLBAR_TEXT);
	option_add_int(&o_toolbar_info, "toolbar_show_info", 1);
	option_add_string(&o_toolbar_disable, "toolbar_disable",
					GTK_STOCK_CLOSE ","
					GTK_STOCK_ZOOM_FIT ","
					GTK_STOCK_SORT_ASCENDING ","
					ROX_STOCK_SHOW_HIDDEN ","
					GTK_STOCK_DIRECTORY ","
					GTK_STOCK_SELECT_ALL ","
					GTK_STOCK_HELP);
	option_add_int(&o_toolbar_min_width, "toolbar_min_width", 1);
	option_add_notify(option_notify);

	option_register_widget("tool-options", build_tool_options);

	option_notify();
}

void toolbar_update_info(FilerWindow *filer_window)
{
	gchar		*label;
	ViewIface	*view;
	int		n_selected;

	g_return_if_fail(filer_window != NULL);

	if (o_toolbar.int_value == TOOLBAR_NONE || !o_toolbar_info.int_value)
		return;		/* Not showing info */

	if (filer_window->target_cb)
		return;

	view = filer_window->view;

	n_selected = view_count_selected(view);

	if (n_selected == 0)
	{
		if (filer_window->scanning)
		{
			gtk_label_set_text(
				GTK_LABEL(filer_window->toolbar_text), "");
			return;
		}

		gchar *s = NULL;
		int n_items = view_count_items(view);

		if (!(filer_window->show_hidden ||
		      filer_window->temp_show_hidden) ||
		    filer_window->filter!=FILER_SHOW_ALL)
		{
			int tally = g_hash_table_size(filer_window->directory->known_items)
				- n_items;

			if (tally)
				s = g_strdup_printf(_(" (%u hidden)"), tally);
		}

		if (n_items)
			label = g_strdup_printf("%d %s%s",
					n_items,
					n_items != 1 ? _("items") : _("item"),
					s ? s : "");
		else /* (French plurals work differently for zero) */
			label = g_strdup_printf(_("No items%s"),
					s ? s : "");
		g_free(s);
	}
	else
	{
		double	size = 0;
		ViewIter iter;
		DirItem *item;

		view_get_iter(filer_window->view, &iter, VIEW_ITER_SELECTED);

		while ((item = iter.next(&iter)))
		{
			if (item->base_type != TYPE_DIRECTORY &&
			    item->base_type != TYPE_UNKNOWN)
				size += (double) item->size;
		}

		label = g_strdup_printf(_("%u selected (%s)"),
				n_selected, format_double_size(size));
	}

	gtk_label_set_text(GTK_LABEL(filer_window->toolbar_text), label);

	g_free(label);
}

/* Create, destroy or recreate toolbar for this window so that it
 * matches the option setting.
 */
void toolbar_update_toolbar(FilerWindow *filer_window)
{
	g_return_if_fail(filer_window != NULL);

	if (filer_window->toolbar)
	{
		gtk_widget_destroy(filer_window->toolbar);
		filer_window->toolbar = NULL;
		filer_window->toolbar_text = NULL;
		filer_window->toolbar_size_text = NULL;
		filer_window->toolbar_settings_text = NULL;
	}

	if (o_toolbar.int_value != TOOLBAR_NONE)
	{
		filer_window->toolbar = gtk_toolbar_new();

		gtk_box_pack_start(filer_window->toplevel_vbox,
				filer_window->toolbar, FALSE, TRUE, 0);
		gtk_box_reorder_child(filer_window->toplevel_vbox,
				filer_window->toolbar, 0);

		create_toolbar(filer_window->toolbar, filer_window);

		gtk_widget_show_all(filer_window->toolbar);
	}

	filer_target_mode(filer_window, NULL, NULL, NULL);
	toolbar_update_info(filer_window);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Wrapper for gtk_get_current_event() which creates a fake release event
 * if there is no current event. This is for ATK.
 */
static GdkEvent *get_current_event(int default_type)
{
	GdkEvent *event;

	event = gtk_get_current_event();

	if (event)
		return event;

	event = gdk_event_new(default_type);
	if (default_type == GDK_BUTTON_PRESS || default_type == GDK_BUTTON_RELEASE)
	{
		GdkEventButton *bev;
		bev = (GdkEventButton *) event;
		bev->button = 1;
	}
	return event;
}

static gint get_release()
{
	gint ret = 0;
	GdkEvent *event = get_current_event(GDK_BUTTON_RELEASE);

	if (event->type == GDK_BUTTON_RELEASE)
	{
		ret = ((GdkEventButton *) event)->button;
		if (ret == 1 && ((GdkEventButton *) event)->state & GDK_MOD1_MASK)
			ret = 2;
	}

	gdk_event_free(event);

	return ret;
}

static void toolbar_help_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	if (get_release() != 1)
		menu_rox_help(NULL, HELP_MANUAL, NULL);
	else
		filer_opendir(make_path(app_dir, "Help"), NULL, NULL, FALSE);
}

static void toolbar_settings_clicked(GtkWidget *widget, FilerWindow *fw)
{
	gint eb = get_release();

	if (eb == 1)
		filer_save_settings(fw, FALSE);
	else if (eb == 2)
	{
		filer_clear_settings(fw);
		display_update_hidden(fw);
	}
	else
		filer_save_settings(fw, TRUE);
}

static void toolbar_refresh_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb == 2)
		filer_refresh_thumbs(filer_window);
	else if (eb != 1)
	{
		filer_opendir(filer_window->sym_path, filer_window, NULL, FALSE);
	}
	else
		filer_refresh(filer_window);
}

static void toolbar_home_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb == 2)
	{
		gchar *staticret = bookmarks_get_top();
		if (staticret)
			filer_change_to(filer_window, staticret, NULL);
	}
	else if (NEW_WIN_BUTTON(eb))
	{
		filer_opendir(home_dir, filer_window, NULL, FALSE);
	}
	else
		filer_change_to(filer_window, home_dir, NULL);
}

static void toolbar_bookmarks_clicked(GtkWidget *widget,
				      FilerWindow *filer_window)
{
	GdkEvent	*event;

	g_return_if_fail(filer_window != NULL);

	event = get_current_event(GDK_BUTTON_PRESS);
	if (event->type == GDK_BUTTON_PRESS &&
			((GdkEventButton *) event)->button == 1)
	{
		bookmarks_show_menu(filer_window, widget);
	}
	else if (event->type == GDK_BUTTON_RELEASE)
	{
		gint eb = get_release();
		if (eb == 2)
		{
			if (bookmarks_get_recent())
				filer_change_to(filer_window, bookmarks_get_recent(), NULL);
		}
		else if (eb != 1)
			bookmarks_edit();
	}
	gdk_event_free(event);
}

static void toolbar_close_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	g_return_if_fail(filer_window != NULL);

	gint eb = get_release();
	if (eb != 1)
	{
		filer_opendir(filer_window->sym_path, filer_window, NULL, FALSE);
	}
	else if (!filer_window_delete(filer_window->window, NULL, filer_window))
		gtk_widget_destroy(filer_window->window);
}

static void toolbar_up_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	gint eb = get_release();

	if (NEW_WIN_BUTTON(eb) && eb != 2)
		filer_open_parent(filer_window);
	else
	{
		view_cursor_to_iter(filer_window->view, NULL);

		if (eb == 2 &&
			strcmp(filer_window->real_path, filer_window->sym_path) != 0)
		{ /* to realpath parent */
			gchar *dir = g_path_get_dirname(filer_window->real_path);
			gchar *base = g_path_get_basename(filer_window->real_path);
			filer_change_to(filer_window, dir, base);
			g_free(dir);
			g_free(base);
		}
		else
			change_to_parent(filer_window);
	}
}

static void toolbar_autosize_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	display_set_layout(filer_window, AUTO_SIZE_ICONS, filer_window->details_type,
			TRUE);
}

static void toolbar_size_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb == 2)
		display_set_layout(filer_window,
			AUTO_SIZE_ICONS, filer_window->details_type, TRUE);
	else
		display_change_size(filer_window, eb == 1);
}

static void toolbar_sort_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	gint eb = get_release();
	int i, current, next, next_wrapped;
	gboolean adjust;
	GtkSortType dir;
	gchar *tip;

	static const SortType sorts[]={
		SORT_NAME, SORT_TYPE, SORT_DATEC, SORT_SIZE,
		SORT_PERM, SORT_OWNER, SORT_GROUP,
	};
	static const char *sort_names[] = {
		N_("Sort by name"), N_("Sort by type"), N_("Sort by date"), N_("Sort by size"),
		N_("Sort by permissions"), N_("Sort by owner"), N_("Sort by group"),
	};

	adjust = (eb != 1) && eb != 0;

	current = -1;
	dir = filer_window->sort_order;
	for (i=0; i < G_N_ELEMENTS(sort_names); i++)
	{
		if (filer_window->sort_type == sorts[i])
		{
			current = i;
			break;
		}
	}

	if (current == -1)
		next = 0;
	else if (adjust)
		next = current - 1;
	else
		next = current + 1;

	next_wrapped = next % G_N_ELEMENTS(sorts);

	if (next_wrapped != next)
		dir = (dir == GTK_SORT_ASCENDING)
			? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;

	display_set_sort_type(filer_window, sorts[next_wrapped], dir);
 	tip = g_strconcat(_(sort_names[next_wrapped]), ", ",
			dir == GTK_SORT_ASCENDING
				? _("ascending") : _("descending"),
			NULL);
	tooltip_show(tip);
	g_free(tip);
}

static void toolbar_details_clicked(GtkWidget *widget,
				    FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb == 1)
	{
		if (filer_window->view_type == VIEW_TYPE_DETAILS)
			filer_set_view_type(filer_window, VIEW_TYPE_COLLECTION);
		else
			filer_set_view_type(filer_window, VIEW_TYPE_DETAILS);
	}
	else
	{
		DetailsType action;

		if (eb == 2)
			action = DETAILS_NONE;
		else if (filer_window->view_type != VIEW_TYPE_COLLECTION)
			action = filer_window->details_type;
		else
			switch (filer_window->details_type)
			{
				case DETAILS_NONE:
					action = DETAILS_SIZE;
					break;
				case DETAILS_SIZE:
					action = DETAILS_TIMES;
					break;
				case DETAILS_TIMES:
					action = DETAILS_PERMISSIONS;
					break;
				case DETAILS_PERMISSIONS:
					action = DETAILS_TYPE;
					break;
				case DETAILS_TYPE:
					action = DETAILS_NONE;
					break;
				default:
					action = DETAILS_NONE;
					break;
			}

		if (filer_window->view_type != VIEW_TYPE_COLLECTION)
			filer_set_view_type(filer_window, VIEW_TYPE_COLLECTION);

		if (action != filer_window->details_type)
			display_set_layout(filer_window,
					filer_window->display_style_wanted,
					action,
					FALSE);
	}
}

static void toolbar_hidden_clicked(GtkWidget *widget,
				   FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb != 0)
	{
		if (eb == 1)
			display_set_hidden(filer_window, !filer_window->show_hidden);
		else if (eb == 2)
		{
			display_set_thumbs(filer_window, o_display_show_thumbs.int_value);
			display_set_hidden(filer_window, o_display_show_hidden.int_value);
		}
		else
			display_set_thumbs(filer_window, !filer_window->show_thumbs);
	}
}

static void toolbar_dirs_clicked(GtkWidget *widget,
				   FilerWindow *filer_window)
{
	switch (get_release())
	{
	case 1:
		filer_window->dirs_only = !filer_window->dirs_only;
		filer_window->files_only = FALSE;
		break;
	case 2:
		filer_window->dirs_only = FALSE;
		filer_window->files_only = FALSE;
		break;
	default:
		filer_window->dirs_only = FALSE;
		filer_window->files_only = !filer_window->files_only;
	}
	display_update_hidden(filer_window);
}

static gboolean invert_cb(ViewIter *iter, gpointer data)
{
	return !view_get_selected((ViewIface *) data, iter);
}

static void toolbar_select_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	gint eb = get_release();

	if (eb != 0)
	{
		if (eb == 1)
			view_select_all(filer_window->view);
		else
			view_select_if(filer_window->view, invert_cb,
				       filer_window->view);
	}
	filer_window->temp_item_selected = FALSE;
}

static int pressx;
static int pressy;
static int pressbtn;
static GtkWidget *in_drag_move;
static int bar_enter(FilerWindow *fw)
{ //end of gtk_window_begin_move_drag
	if (in_drag_move)
		gtk_button_released(GTK_BUTTON(in_drag_move));

	in_drag_move = NULL;
	return FALSE;
}
static int bar_motion(GtkWidget *widget, GdkEventMotion *event, FilerWindow *fw)
{
	if (!pressx && !pressy) return FALSE;

	int threshold;
	g_object_get(gtk_settings_get_default(),
		"gtk-dnd-drag-threshold", &threshold, NULL);

	int dx = event->x_root - pressx;
	int dy = event->y_root - pressy;

	if (ABS(dx) > threshold || ABS(dy) > threshold)
	{
		GtkWindow *win = GTK_WINDOW(gtk_widget_get_toplevel(widget));
		if (pressbtn == 1)
		{
			gtk_window_begin_move_drag(win,
					1, pressx, pressy, event->time); //use pressx/y for cursor pos
			if (GTK_IS_BUTTON(widget))
				in_drag_move = widget;
		}
		else if (pressbtn == 3)
		{
			if (fw->view_type == VIEW_TYPE_COLLECTION)
			{
				gdk_window_get_geometry(
						fw->window->window, NULL, NULL,
						&fw->resize_drag_width,
						NULL, NULL);

				fw->name_scale_start = fw->name_scale;
			}

			gtk_window_begin_resize_drag(win, GDK_WINDOW_EDGE_NORTH_EAST,
					3, event->x_root, event->y_root, event->time);
		}

		pressbtn = pressx = pressy = 0;
		return TRUE;
	}

	return FALSE;
}
static gint btn_released(GtkWidget *widget,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	pressbtn = pressx = pressy = 0;
	return FALSE;
}
static gint bar_released(GtkWidget *widget,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	if (pressbtn == event->button)
		switch (event->button)
	{
	case 1:
		{
			ViewIter iter;
			if ((view_get_cursor(filer_window->view, &iter), iter.peek(&iter))
				&& iter.peek(&iter)->base_type == TYPE_DIRECTORY)
			{
				view_cursor_to_iter(filer_window->view, NULL);
				filer_change_to(filer_window,
						make_path(filer_window->sym_path, iter.peek(&iter)->leafname),
						NULL);
			}
		}
		break;
	case 3:
		{
			const char *current = filer_window->sym_path;
			if (current[0] == '/' && current[1] == '\0')
				break;

			view_cursor_to_iter(filer_window->view, NULL);
			change_to_parent(filer_window);
		}
		break;
	}

	pressbtn = pressx = pressy = 0;
	return FALSE;
}
static gint bar_pressed(GtkWidget *widget,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	if (event->type != GDK_BUTTON_PRESS) return FALSE;

	switch (event->button)
	{
	case 2:
		filer_cut_links(filer_window, 0);
		if (filer_window->name_scale != 1.0)
		{
			filer_window->name_scale = 1.0;
			view_style_changed(filer_window->view, VIEW_UPDATE_NAME);
		}
		view_autosize(filer_window->view, FALSE);
		view_cursor_to_iter(filer_window->view, NULL);
		break;
	case 1:
	case 3:
		pressbtn = event->button;
		pressx = event->x_root;
		pressy = event->y_root;
	default:
		return FALSE;
	}

	return TRUE;
}
static gint bar_scrolled(
		GtkButton *button,
		GdkEventScroll *event,
		FilerWindow *fw)
{
//	if (fw->right_link && event->state & GDK_MOD1_MASK)
//		filer_dir_link_next(fw->right_link, event->direction, FALSE);
//
	if (fw->right_link &&
				event->state & (GDK_SHIFT_MASK | GDK_BUTTON1_MASK))
		filer_send_event_to_view(fw->right_link, (GdkEvent *) event);
	else if (event->state & (GDK_CONTROL_MASK | GDK_BUTTON3_MASK))
		filer_send_event_to_view(fw, (GdkEvent *) event);
	else
		filer_dir_link_next(fw, event->direction, FALSE);

	pressbtn = pressx = pressy = 0;

	return TRUE;
}

static void toolbar_new_clicked(GtkWidget *widget, FilerWindow *filer_window)
{
	GdkEvent	*event;

	event = get_current_event(GDK_BUTTON_RELEASE);
	if (event->type == GDK_BUTTON_RELEASE)
	{
		if (((GdkEventButton *) event)->button == 1)
			show_new_directory(filer_window);
		else if (((GdkEventButton *) event)->button == 2)
			show_new_file(filer_window);
		else
			show_menu_new(filer_window);
	}
	gdk_event_free(event);
}

/* If filer_window is NULL, the toolbar is for the options window */
static void create_toolbar(GtkWidget *bar, FilerWindow *filer_window)
{
	GtkWidget	*b;
	int i;

	if (o_toolbar.int_value == TOOLBAR_NORMAL || !filer_window)
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_ICONS);
	else if (o_toolbar.int_value == TOOLBAR_TEXT)
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_TEXT);
	else if (o_toolbar.int_value == TOOLBAR_HORIZONTAL)
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_BOTH_HORIZ);
	else
		gtk_toolbar_set_style(GTK_TOOLBAR(bar), GTK_TOOLBAR_BOTH);

	for (i = 0; i < sizeof(all_tools) / sizeof(*all_tools); i++)
	{
		Tool	*tool = &all_tools[i];

		if (filer_window && !tool->enabled)
			continue;

		b = add_button(bar, tool, filer_window);

		if (filer_window && tool->drop_action != DROP_NONE)
			handle_drops(filer_window, b, tool->drop_action);
	}

	if (filer_window)
	{
		/* Make the toolbar wide enough for all icons to be
		   seen, plus a little for the (start of the) text
		   label. Though the return of size_request is incorrect.
		   Probably it can't get current icon size.
		   */
		GtkRequisition req;
		gtk_widget_size_request(bar, &req);
		toolbar_min_width = req.width;

		filer_window->toolbar_text = gtk_label_new(
				o_toolbar_info.int_value ? _("_N_Items_") : "");
		gtk_misc_set_alignment(GTK_MISC(filer_window->toolbar_text),
					0, 0.5);
		gtk_toolbar_append_widget(GTK_TOOLBAR(bar),
				filer_window->toolbar_text, NULL, NULL);

		//somehow label width is not included
		gtk_widget_size_request(filer_window->toolbar_text, &req);
		toolbar_min_width += req.width;

		gtk_widget_set_size_request(bar, 100, -1);

		gtk_widget_add_events(bar, GDK_BUTTON_RELEASE | GDK_MOTION_NOTIFY);
		g_signal_connect(bar, "motion-notify-event",
			G_CALLBACK(bar_motion), filer_window);
		g_signal_connect(bar, "button-release-event",
			G_CALLBACK(bar_released), filer_window);

		g_signal_connect(bar, "button_press_event",
			G_CALLBACK(bar_pressed), filer_window);

		g_signal_connect(bar, "scroll_event",
			G_CALLBACK(bar_scrolled), filer_window);
	}
}

/* This is used to simulate a click when button 3 is used (GtkButton
 * normally ignores this).
 */
static gint toolbar_other_button = 0;
static gint toolbar_button_pressed(GtkButton *button,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	gint	b = event->button;
	Tool	*tool;

	tool = g_object_get_data(G_OBJECT(button), "rox-tool");
	g_return_val_if_fail(tool != NULL, TRUE);

	if (tool->menu && b == 1 &&
			!(((GdkEventButton *) event)->state & GDK_MOD1_MASK))
	{
		tool->clicked((GtkWidget *) button, filer_window);
		return TRUE;
	}

	if ((b == 2 || b == 3) && toolbar_other_button == 0)
	{
		toolbar_other_button = event->button;
		gtk_grab_add(GTK_WIDGET(button));
		gtk_button_pressed(button);

		return TRUE;
	}

	return FALSE;
}

static gint toolbar_size_enter(GtkButton *button,
				GdkEventScroll *event,
				FilerWindow *fw)
{
	gtk_widget_set_has_tooltip((GtkWidget *) button, TRUE);
	return FALSE;
}
static gint toolbar_button_scroll(GtkButton *button,
				GdkEventScroll *event,
				FilerWindow *fw)
{
	if (!fw) return TRUE; //kill event for bar event area

	DisplayStyle ds = fw->display_style;
	DisplayStyle *dsw = &(fw->display_style_wanted);
	gfloat *sc = &(fw->icon_scale);
	gfloat
		step_pix = MAX((huge_size - ICON_HEIGHT) / 4.0, ICON_HEIGHT - SMALL_WIDTH),
		start = (ICON_HEIGHT + step_pix - 1) / huge_size,
		step  = step_pix / huge_size,
		end   = HUGE_LIMIT_F / huge_size;

	gtk_widget_set_has_tooltip((GtkWidget *) button, FALSE);

	if (event->direction == GDK_SCROLL_UP)
	{
		if (ds == SMALL_ICONS)
			*dsw = LARGE_ICONS;
		else if (ds == LARGE_ICONS)
		{
			*dsw = HUGE_ICONS;
			*sc = start;
		}
		else if (*sc < end)
			*sc += step;
		else
			return TRUE;
	}
	else if(event->direction == GDK_SCROLL_DOWN)
	{
		if (ds == HUGE_ICONS)
		{
			*sc -= step;
			if (*sc < start) {
				*sc += step;
				*dsw = LARGE_ICONS;
			}
		}
		else if (ds == LARGE_ICONS)
			*dsw = SMALL_ICONS;
		else
			return TRUE;
	}
	else
		return FALSE;

	display_set_actual_size(fw, FALSE);
	return TRUE;
}

static gint toolbar_button_released(GtkButton *button,
				GdkEventButton *event,
				FilerWindow *filer_window)
{
	if (event->button == toolbar_other_button)
	{
		toolbar_other_button = 0;
		gtk_grab_remove(GTK_WIDGET(button));
		gtk_button_released(button);

		return TRUE;
	}

	return FALSE;
}

/* If filer_window is NULL, the toolbar is for the options window */
static GtkWidget *add_button(GtkWidget *bar, Tool *tool,
				FilerWindow *filer_window)
{
	GtkWidget *button, *icon_widget, *label = NULL, *hbox;

	icon_widget = gtk_image_new_from_stock(tool->name,
						GTK_ICON_SIZE_LARGE_TOOLBAR);

	button = gtk_toolbar_insert_element(GTK_TOOLBAR(bar),
			   filer_window ? GTK_TOOLBAR_CHILD_BUTTON
					: GTK_TOOLBAR_CHILD_TOGGLEBUTTON,
			   NULL,
			   _(tool->label),
			   _(tool->tip), NULL,
			   icon_widget,
			   NULL, NULL,	/* CB, userdata */
			   GTK_TOOLBAR(bar)->num_children);
	gtk_widget_set_can_focus(button, FALSE);

	g_object_set_data(G_OBJECT(button), "rox-tool", tool);

	if (filer_window)
	{
		g_signal_connect(button, "clicked",
			G_CALLBACK(tool->clicked), filer_window);

		g_signal_connect(button, "button_press_event",
			G_CALLBACK(toolbar_button_pressed), filer_window);
		g_signal_connect(button, "button_release_event",
			G_CALLBACK(toolbar_button_released), filer_window);

		gtk_widget_add_events(button, GDK_BUTTON_RELEASE | GDK_MOTION_NOTIFY);
		g_signal_connect(button, "motion-notify-event",
			G_CALLBACK(bar_motion), filer_window);
		g_signal_connect(button, "button-release-event",
			G_CALLBACK(btn_released), filer_window);
		g_signal_connect(button, "button_press_event",
			G_CALLBACK(bar_pressed), filer_window);
		g_signal_connect_swapped(button, "enter-notify-event",
			G_CALLBACK(bar_enter), filer_window);

		if (o_toolbar.int_value != TOOLBAR_NORMAL)
		{
			GList	  *kids;
			hbox = GTK_BIN(button)->child;
			kids = gtk_container_get_children(GTK_CONTAINER(hbox));
			label = g_list_nth_data(kids, 1);

			g_list_free(kids);
		}

		if (o_toolbar.int_value == TOOLBAR_HORIZONTAL ||
			o_toolbar.int_value == TOOLBAR_TEXT)
			gtk_misc_set_alignment(GTK_MISC(label), 0.2, 0.5);

		if (tool->clicked == toolbar_size_clicked)
		{
			filer_window->toolbar_size_text = GTK_LABEL(label);
			g_signal_connect(button, "scroll_event",
				G_CALLBACK(toolbar_button_scroll), filer_window);
			g_signal_connect(button, "enter_notify_event",
				G_CALLBACK(toolbar_size_enter), filer_window);
		}
		else
			g_signal_connect(button, "scroll_event",
				G_CALLBACK(toolbar_button_scroll), NULL);

		if (tool->clicked == toolbar_settings_clicked)
			filer_window->toolbar_settings_text = GTK_LABEL(label);
	}
	else
	{
		g_signal_connect(button, "clicked",
			G_CALLBACK(toggle_selected), NULL);
		g_object_set_data(G_OBJECT(button), "tool_name",
				  (gpointer) tool->name);
	}

	return button;
}

static void toggle_selected(GtkToggleButton *widget, gpointer data)
{
	option_check_widget(&o_toolbar_disable);
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
	GdkDragAction	action = context->suggested_action;
	DropDest	dest;
	gpointer	type = (gpointer) drop_dest_dir;

	dest = (DropDest) g_object_get_data(G_OBJECT(widget), "toolbar_dest");

	if ((context->actions & GDK_ACTION_ASK) && o_dnd_left_menu.int_value &&
		dest != DROP_BOOKMARK)
	{
		guint state;
		gdk_window_get_pointer(NULL, NULL, NULL, &state);
		if (state & GDK_BUTTON1_MASK)
			action = GDK_ACTION_ASK;
	}

	if (dest == DROP_TO_HOME)
		g_dataset_set_data(context, "drop_dest_path",
				   (gchar *) home_dir);
	else if (dest == DROP_BOOKMARK)
		type = (gpointer) drop_dest_bookmark;
	else
		g_dataset_set_data_full(context, "drop_dest_path",
				g_path_get_dirname(filer_window->sym_path),
				g_free);

	g_dataset_set_data(context, "drop_dest_type", type);
	gdk_drag_status(context, action, time);

	dnd_spring_load(context, filer_window);
	gtk_button_set_relief(GTK_BUTTON(widget), GTK_RELIEF_NORMAL);

	return TRUE;
}

static void drag_leave(GtkWidget	*widget,
                       GdkDragContext	*context,
		       guint32		time,
		       FilerWindow	*filer_window)
{
	gtk_button_set_relief(GTK_BUTTON(widget), GTK_RELIEF_NONE);
	dnd_spring_abort();
}

static void handle_drops(FilerWindow *filer_window,
			 GtkWidget *button,
			 DropDest dest)
{
	make_drop_target(button, 0);
	g_signal_connect(button, "drag_motion",
			G_CALLBACK(drag_motion), filer_window);
	g_signal_connect(button, "drag_leave",
			G_CALLBACK(drag_leave), filer_window);
	g_object_set_data(G_OBJECT(button), "toolbar_dest", (gpointer) dest);
}

static void option_notify(void)
{
	int		i;
	gboolean	changed = FALSE;
	guchar		*list = o_toolbar_disable.value;

	for (i = 0; i < sizeof(all_tools) / sizeof(*all_tools); i++)
	{
		Tool	*tool = &all_tools[i];
		gboolean old = tool->enabled;

		tool->enabled = !in_list(tool->name, list);

		if (old != tool->enabled)
			changed = TRUE;
	}

	if (changed || o_toolbar.has_changed || o_toolbar_info.has_changed)
	{
		GList	*next;

		for (next = all_filer_windows; next; next = next->next)
		{
			FilerWindow *filer_window = (FilerWindow *) next->data;

			toolbar_update_toolbar(filer_window);
		}

		filer_resize_all(TRUE);
	} else if (o_toolbar_min_width.has_changed)
		filer_resize_all(TRUE);
}

static void update_tools(Option *option)
{
	GList	*next, *kids;

	kids = gtk_container_get_children(GTK_CONTAINER(option->widget));

	for (next = kids; next; next = next->next)
	{
		GtkToggleButton	*kid = (GtkToggleButton *) next->data;
		guchar		*name;

		name = g_object_get_data(G_OBJECT(kid), "tool_name");

		g_return_if_fail(name != NULL);

		gtk_toggle_button_set_active(kid,
					 !in_list(name, option->value));
	}

	g_list_free(kids);
}

static guchar *read_tools(Option *option)
{
	GList	*next, *kids;
	GString	*list;
	guchar	*retval;

	list = g_string_new(NULL);

	kids = gtk_container_get_children(GTK_CONTAINER(option->widget));

	for (next = kids; next; next = next->next)
	{
		GtkToggleButton	*kid = (GtkToggleButton *) next->data;
		guchar		*name;

		if (!gtk_toggle_button_get_active(kid))
		{
			name = g_object_get_data(G_OBJECT(kid), "tool_name");
			g_return_val_if_fail(name != NULL, list->str);

			if (list->len)
				g_string_append(list, ", ");
			g_string_append(list, name);
		}
	}

	g_list_free(kids);
	retval = list->str;
	g_string_free(list, FALSE);

	return retval;
}

static GList *build_tool_options(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget	*bar;

	g_return_val_if_fail(option != NULL, NULL);

	option->widget = gtk_toolbar_new();
	create_toolbar(option->widget, NULL);

	option->update_widget = update_tools;
	option->read_widget = read_tools;

	return g_list_append(NULL, option->widget);
}
