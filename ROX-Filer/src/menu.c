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

/* menu.c - code for handling the popup menus */

#ifndef GTK_STOCK_INFO
# define GTK_STOCK_INFO "gtk-info"
#endif

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <dirent.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "main.h"
#include "menu.h"
#include "run.h"
#include "action.h"
#include "filer.h"
#include "pixmaps.h"
#include "type.h"
#include "support.h"
#include "gui_support.h"
#include "options.h"
#include "choices.h"
#include "gtksavebox.h"
#include "mount.h"
#include "minibuffer.h"
#include "i18n.h"
#include "pinboard.h"
#include "dir.h"
#include "diritem.h"
#include "appmenu.h"
#include "usericons.h"
#include "infobox.h"
#include "view_iface.h"
#include "display.h"
#include "bookmarks.h"
#include "panel.h"
#include "bulk_rename.h"
#include "xtypes.h"
#include "log.h"
#include "dnd.h"

typedef enum {
	FILE_DUPLICATE_ITEM,
	FILE_RENAME_ITEM,
	FILE_LINK_ITEM,
	FILE_OPEN_FILE,
	FILE_PROPERTIES,
	FILE_RUN_ACTION,
	FILE_SET_ICON,
	FILE_SEND_TO,
	FILE_DELETE,
	FILE_USAGE,
	FILE_CHMOD_ITEMS,
	FILE_FIND,
	FILE_SET_TYPE,
	FILE_COPY_TO_CLIPBOARD,
	FILE_CUT_TO_CLIPBOARD,
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
	FILE_XATTRS,
#endif
} FileOp;

typedef void (*ActionFn)(GList *paths,
			 const char *dest_dir, const char *leaf, int quiet);

typedef gboolean (*SaveCb)(GObject *savebox,
			   const gchar *current, const gchar *new);

GtkAccelGroup	*filer_keys = NULL;

static GtkWidget *popup_menu = NULL;	/* Currently open menu */

static gint updating_menu = 0;		/* Non-zero => ignore activations */
static GList *send_to_paths = NULL;

static Option o_menu_iconsize, o_menu_xterm, o_menu_quick;

/* clipboard targets */
static const GtkTargetEntry clipboard_targets[] = {
	{"text/uri-list", 0, TARGET_URI_LIST},
	{"x-special/gnome-copied-files", 0, TARGET_GNOME_COPIED_FILES},
};
static GtkClipboard *clipboard;
static const char *clipboard_action = NULL;
static GList *selected_paths = NULL;

/* Static prototypes */

static void save_menus(void);
static void menu_closed(GtkWidget *widget);
static void shade_file_menu_items(gboolean shaded);
static void savebox_show(const gchar *action, const gchar *path,
			 MaskedPixmap *image, SaveCb callback,
			 GdkDragAction dnd_action);
static gint save_to_file(GObject *savebox,
			 const gchar *pathname, gpointer data);
static gboolean action_with_leaf(ActionFn action,
				 const gchar *current, const gchar *new);
static gboolean link_cb(GObject *savebox,
			const gchar *initial, const gchar *path);
static gboolean rename_cb(GObject *savebox,
			const gchar *initial, const gchar *path);
static void select_nth_item(GtkMenuShell *shell, int n);
static void new_file_type(gchar *templ);
static void do_send_to(gchar *templ);
static void show_send_to_menu(FilerWindow *fw, GdkEvent *event);
static void show_dir_send_to_menu(GdkEvent *event);
static GList *set_keys_button(Option *option, xmlNode *node, guchar *label);

/* Note that for most of these callbacks none of the arguments are used. */

static void view_type(gpointer data, guint action, GtkWidget *widget);

/* (action used in these three - DetailsType) */
static void change_size(gpointer data, guint action, GtkWidget *widget);
static void change_size_auto(gpointer data, guint action, GtkWidget *widget);
static void set_with(gpointer data, guint action, GtkWidget *widget);

static void set_sort(gpointer data, guint action, GtkWidget *widget);
static void reverse_sort(gpointer data, guint action, GtkWidget *widget);

static void filter_directories(gpointer data, guint action, GtkWidget *widget);
static void hidden(gpointer data, guint action, GtkWidget *widget);
static void only_dirs(gpointer data, guint action, GtkWidget *widget);
static void show_thumbs(gpointer data, guint action, GtkWidget *widget);
static void refresh(gpointer data, guint action, GtkWidget *widget);
static void refresh_thumbs(gpointer data, guint action, GtkWidget *widget);
static void save_settings(gpointer data, guint action, GtkWidget *widget);
static void save_settings_parent(gpointer data, guint action, GtkWidget *widget);

static void file_op(gpointer data, FileOp action, GtkWidget *widget);

static void select_all(gpointer data, guint action, GtkWidget *widget);
static void clear_selection(gpointer data, guint action, GtkWidget *widget);
static void invert_selection(gpointer data, guint action, GtkWidget *widget);
static void new_directory(gpointer data, guint action, GtkWidget *widget);
static void new_file(gpointer data, guint action, GtkWidget *widget);
static void customise_new(gpointer data);
static GList *add_sendto_shared(GtkWidget *menu,
		const gchar *type, const gchar *subtype, CallbackFn swapped_func);
static void customise_directory_menu(gpointer data);
static void xterm_here(gpointer data, guint action, GtkWidget *widget);

static void open_parent_same(gpointer data, guint action, GtkWidget *widget);
static void open_parent(gpointer data, guint action, GtkWidget *widget);
static void home_directory(gpointer data, guint action, GtkWidget *widget);
static void show_bookmarks(gpointer data, guint action, GtkWidget *widget);
static void show_log(gpointer data, guint action, GtkWidget *widget);
static void new_window(gpointer data, guint action, GtkWidget *widget);
/* static void new_user(gpointer data, guint action, GtkWidget *widget); */
static void close_window(gpointer data, guint action, GtkWidget *widget);
static void follow_symlinks(gpointer data, guint action, GtkWidget *widget);

/* (action used in this - MiniType) */
static void mini_buffer(gpointer data, guint action, GtkWidget *widget);
static void resize(gpointer data, guint action, GtkWidget *widget);

/* clipboard */
static void clipboard_get(GtkClipboard *clipboard, GtkSelectionData *selection_data, guint info, gpointer user_data);
static void clipboard_clear(GtkClipboard *clipboard, gpointer user_data);
static void paste_from_clipboard(gpointer data, guint action, GtkWidget *widget);

#define MENUS_NAME "menus2"

static GtkWidget	*filer_menu = NULL;		/* The popup filer menu */
static GtkWidget	*filer_file_item;	/* The File '' label */
static GtkWidget	*filer_file_menu;	/* The File '' menu */
static GtkWidget	*file_shift_item;	/* Shift Open label */
static GtkWidget	*filer_auto_size_menu;	/* The Automatic item */
static GtkWidget	*filer_hidden_menu;	/* The Show Hidden item */
static GtkWidget	*filer_files_only_menu;
static GtkWidget	*filer_dirs_only_menu;
static GtkWidget	*filer_filter_dirs_menu;/* The Filter Dirs item */

/* The Sort items */
static GtkWidget	*filer_sort_name_menu;
static GtkWidget	*filer_sort_type_menu;
static GtkWidget	*filer_sort_date_a_menu;
static GtkWidget	*filer_sort_date_c_menu;
static GtkWidget	*filer_sort_date_m_menu;
static GtkWidget	*filer_sort_size_menu;
static GtkWidget	*filer_sort_perm_menu;
static GtkWidget	*filer_sort_owner_menu;
static GtkWidget	*filer_sort_group_menu;

static GtkWidget	*filer_reverse_menu;	/* The Reversed item */
static GtkWidget	*filer_thumb_menu;	/* The Show Thumbs item */
static GtkWidget	*filer_new_window;	/* The New Window item */
static GtkWidget    *filer_new_menu;        /* The New submenu */
static GtkWidget    *filer_follow_sym;      /* Follow symbolic links item */
static GtkWidget    *filer_set_type;        /* Set type item */
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
static GtkWidget	*filer_xattrs;	/* Extended attributes item */
#endif

//working buffers
static GtkWidget *current;
static GtkWidget *cmenu;

static void item_common(const gchar *label, MenuCB cb, guint action)
{
	if (cb)
		g_signal_connect(current, "activate",
				G_CALLBACK(cb),
				GUINT_TO_POINTER(action));

	gtk_menu_item_set_accel_path(GTK_MENU_ITEM(current),
			g_strconcat(gtk_menu_get_accel_path(GTK_MENU(cmenu)), label, NULL));

	gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), current);
}

#define adi menu_add_item
GtkWidget *menu_add_item(gchar *label, MenuCB cb, guint action)
{
	current = gtk_menu_item_new_with_label(_(label));
	item_common(label, cb, action);
	return GTK_BIN(current)->child;
}
#define ads menu_add_stock
GtkWidget *menu_add_stock(
		gchar *label, MenuCB cb, guint action, const gchar *stock_id)
{
	current = gtk_image_menu_item_new_from_stock(stock_id, NULL);
	gtk_menu_item_set_label(GTK_MENU_ITEM(current), _(label));
	item_common(label, cb, action);
	return GTK_BIN(current)->child;
}
static void adt(gchar *label, MenuCB cb, guint action, GtkWidget **w)
{
	current = *w = gtk_check_menu_item_new_with_label(_(label));
	item_common(label, cb, action);
}
static GSList *adr(
		const gchar *label, MenuCB cb, guint action, GSList *group, GtkWidget **w)
{
	current = *w = gtk_radio_menu_item_new_with_label(group, _(label));
	item_common(label, cb, action);
	return gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(current));
}

#define add_separator menu_add_separator
void menu_add_separator(void)
{ gtk_menu_shell_append(GTK_MENU_SHELL(cmenu), gtk_separator_menu_item_new()); }

static void sta(guint accel_key, GdkModifierType accel_mods)
{
	gtk_accel_map_add_entry(
			gtk_menu_item_get_accel_path(GTK_MENU_ITEM(current)),
			accel_key,
			accel_mods);
}

#define start_menu menu_start
GtkWidget *menu_start(gchar *label, GtkWidget *parent)
{
	const gchar *path = "";
	if (parent)
	{
		cmenu = parent;
		path = gtk_menu_get_accel_path(GTK_MENU(parent));
		if (label)
			adi(label, NULL, 0);
	}

	if (label)
	{
		cmenu = gtk_menu_new();
		if (parent)
			gtk_menu_set_accel_group(GTK_MENU(cmenu),
					gtk_menu_get_accel_group(GTK_MENU(parent)));

		gtk_menu_set_accel_path(GTK_MENU(cmenu),
				g_strconcat(path, label, "/" , NULL));

		if (parent)
			gtk_menu_item_set_submenu(GTK_MENU_ITEM(current), cmenu);
	}

	return cmenu;
}

void menu_init(void)
{
	char *menurc = choices_find_xdg_path_load(MENUS_NAME, PROJECT, SITE);
	if (menurc)
	{
		gtk_accel_map_load(menurc);
		g_free(menurc);
	}

	option_add_string(&o_menu_xterm, "menu_xterm", "xterm");
	option_add_int(&o_menu_iconsize, "menu_iconsize", MIS_SMALL);
	option_add_int(&o_menu_quick, "menu_quick", FALSE);
	option_add_saver(save_menus);

	option_register_widget("menu-set-keys", set_keys_button);

	filer_keys = gtk_accel_group_new();

	clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
}

MenuIconStyle get_menu_icon_style(void)
{
	MenuIconStyle mis;
	int display;

	mis = o_menu_iconsize.int_value;

	switch (mis)
	{
		case MIS_NONE: case MIS_SMALL: case MIS_LARGE:
			return mis;
		default:
			break;
	}

	if (mis == MIS_CURRENT && window_with_focus)
	{
		switch (window_with_focus->display_style)
		{
			case HUGE_ICONS:
			case LARGE_ICONS:
				return MIS_LARGE;
			case SMALL_ICONS:
				return MIS_SMALL;
			default:
				break;
		}
	}

	display = o_display_size.int_value;
	switch (display)
	{
		case HUGE_ICONS:
		case LARGE_ICONS:
			return MIS_LARGE;
		case SMALL_ICONS:
			return MIS_SMALL;
		default:
			break;
	}

	return MIS_SMALL;
}

/* Shade items that only work on single files */
static void shade_file_menu_items(gboolean shaded)
{
	menu_set_items_shaded(filer_file_menu, shaded, 2, 1); /* Duplicate... */
	menu_set_items_shaded(filer_file_menu, shaded, 4, 1); /* Link... */
	menu_set_items_shaded(filer_file_menu, shaded, 7, 1); /* Shift Open */
	menu_set_items_shaded(filer_file_menu, shaded, 10, 2); /* Set Run Action... + Set Icon... */
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
	menu_set_items_shaded(filer_file_menu, shaded, 12, 1); /* Extended Attributes... */
#endif
}

/* 'data' is an array of three ints:
 * [ pointer_x, pointer_y, item_under_pointer ]
 */
void position_menu(GtkMenu *menu, gint *x, gint *y,
		   gboolean  *push_in, gpointer data)
{
	int		*pos = (int *) data;
	GtkRequisition 	requisition;
	GList		*items, *next;
	int		y_shift = 0;
	int		item = pos[2];
	int h = 2;

	next = items = gtk_container_get_children(GTK_CONTAINER(menu));

	while (item >= 0 && next)
	{
		h = ((GtkWidget *) next->data)->requisition.height;

		if (item > 0)
			y_shift += h;
		else
			y_shift += h / 2;

		next = next->next;
		item--;
	}

	g_list_free(items);

	gtk_widget_size_request(GTK_WIDGET(menu), &requisition);

	*x = pos[0] - (requisition.width * 7 / 8);
	*y = pos[1] - y_shift;

	*x = CLAMP(*x, 0, screen_width - requisition.width);
	if (filer_file_menu == (GtkWidget *)menu)
		*y = CLAMP(*y, 0, screen_height - requisition.height);

	if (*y > screen_height - requisition.height)
		*y -= h / 2 - 1;

	*push_in = TRUE;
}

GtkWidget *menu_make_image(DirItem *ditem, MenuIconStyle style)
{
	if (style != MIS_NONE && di_image(ditem))
	{
		GdkPixbuf *pixbuf;
		MaskedPixmap *image;

		image = di_image(ditem);

		switch (style)
		{
			case MIS_LARGE:
				pixbuf = image->pixbuf;
				break;
			default:
				if (!image->sm_pixbuf)
					pixmap_make_small(image);
				pixbuf = image->sm_pixbuf;
				break;
		}

		GdkPixbuf *new = ditem->label ?
			(pixbuf = pixmap_make_lined(pixbuf, ditem->label)) : NULL;

		GtkWidget *ret = gtk_image_new_from_pixbuf(pixbuf);

		if (new)
			g_object_unref(new);

		return ret;
	}

	return NULL;
}
static GtkWidget *make_send_to_item(DirItem *ditem, const char *label,
				MenuIconStyle style)
{
	GtkWidget *item;
	GtkWidget *img;

	if ((img = menu_make_image(ditem, style)))
	{
		item = gtk_image_menu_item_new_with_mnemonic(label);
		/* TODO: Find a way to allow short-cuts */

		gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(item),
				img);
		gtk_widget_show_all(item);
	}
	else
		item = gtk_menu_item_new_with_label(label);

	return item;
}

static GList *menu_from_dir(GtkWidget *menu, const gchar *dir_name,
			    MenuIconStyle style, CallbackFn func,
			    gboolean separator, gboolean strip_ext,
			    gboolean recurse)
{
	GList *widgets = NULL;
	DirItem *ditem;
	int i;
	GtkWidget *item;
	char *dname = NULL;
	GPtrArray *names;

	dname = pathdup(dir_name);

	names = list_dir(dname);
	if (!names)
		goto out;

	for (i = 0; i < names->len; i++)
	{
		char	*leaf = names->pdata[i];
		gchar	*fname;

		if (separator)
		{
			item = gtk_menu_item_new();
			widgets = g_list_append(widgets, item);
			if (menu)
				gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
			separator = FALSE;
		}

		fname = g_strconcat(dname, "/", leaf, NULL);

		/* Strip off extension, if any */
		if (strip_ext)
		{
			char	*dot;
			dot = strchr(leaf, '.');
			if (dot)
				*dot = '\0';
		}

		ditem = diritem_new("");
		diritem_restat(fname, ditem, NULL, TRUE);

		item = make_send_to_item(ditem, leaf, style);

		/* If it is a directory (but NOT an AppDir) and we are
		 * recursing then set up a sub menu.
		 */
		if (recurse && ditem->base_type == TYPE_DIRECTORY &&
			   !(ditem->flags & ITEM_FLAG_APPDIR))
		{
			GtkWidget *sub = gtk_menu_new();
			const gchar *apath = menu ?
				gtk_menu_get_accel_path(GTK_MENU(menu)) : NULL;
			if (apath)
			{
				gtk_menu_set_accel_group(GTK_MENU(sub),
						gtk_menu_get_accel_group(GTK_MENU(menu)));
				gtk_menu_set_accel_path(GTK_MENU(sub),
						g_strconcat(apath, leaf, "/", NULL));
			}

			g_list_free(
				menu_from_dir(sub, fname, style, func,
						separator, strip_ext, TRUE)
			);

			gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), sub);
		}
		else
			g_signal_connect_swapped(item, "activate",
					G_CALLBACK(func), fname);

		g_free(leaf);
		diritem_free(ditem);

		if (menu)
			gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		g_signal_connect_swapped(item, "destroy",
				G_CALLBACK(g_free), fname);

		widgets = g_list_append(widgets, item);
	}

	g_ptr_array_free(names, TRUE);
out:
	g_free(dname);

	return widgets;
}

/* Scan the templates dir and create entries for the New menu */
static void update_new_files_menu()
{
	static GList *widgets = NULL;

	gchar	*templ_dname = NULL;

	if (widgets)
	{
		GList	*next;

		for (next = widgets; next; next = next->next)
			gtk_widget_destroy((GtkWidget *) next->data);

		g_list_free(widgets);
		widgets = NULL;
	}

	templ_dname = choices_find_xdg_path_load("Templates", "", SITE);
	if (templ_dname)
	{
		widgets = menu_from_dir(filer_new_menu, templ_dname,
					get_menu_icon_style(),
					(CallbackFn) new_file_type, TRUE, TRUE,
					FALSE);
		g_free(templ_dname);
	}
	gtk_widget_show_all(filer_new_menu);
}

static void directory_cb(const gchar *app)
{
	GList *file_list = g_list_prepend(NULL, window_with_focus->sym_path);
	run_with_files(app, file_list, FALSE);
	g_list_free(file_list);
}
static void update_directory_menu()
{
	static GList *widgets = NULL;

	for (; widgets; widgets = g_list_delete_link(widgets, widgets))
		gtk_widget_destroy((GtkWidget *) widgets->data);

	widgets = add_sendto_shared(filer_menu,
			inode_directory->media_type, NULL, (CallbackFn) directory_cb);
}

gboolean ensure_filer_menu(void)
{
	if (filer_menu) return FALSE;

	filer_menu = start_menu("<filer>", NULL);
	gtk_menu_set_accel_group(GTK_MENU(filer_menu), filer_keys);

	start_menu(N_("Display"), filer_menu);

	adi(N_("Icons View"            ), view_type, VIEW_TYPE_COLLECTION);
	adi(N_("Icons With Sizes"      ), set_with , DETAILS_SIZE);
	adi(N_("Icons With Times"      ), set_with , DETAILS_TIMES);
	adi(N_("Icons With Permissions"), set_with , DETAILS_PERMISSIONS);
	adi(N_("Icons With Types"      ), set_with , DETAILS_TYPE);

	ads(N_("List View"), view_type, VIEW_TYPE_DETAILS, ROX_STOCK_SHOW_DETAILS);

	add_separator();

	ads(N_("Bigger Icons"), change_size, 1, GTK_STOCK_ZOOM_IN);
		sta(GDK_KEY_plus, 0);
	ads(N_("Smaller Icons"), change_size, -1, GTK_STOCK_ZOOM_OUT);
		sta(GDK_KEY_minus, 0);
	adt(N_("Automatic"), change_size_auto, 0, &filer_auto_size_menu);
		sta(GDK_KEY_equal, 0);

	add_separator();

	GSList *sg = NULL;
	sg = adr(N_("Sort by Name"        ), set_sort, SORT_NAME , sg, &filer_sort_name_menu  );
	sg = adr(N_("Sort by Type"        ), set_sort, SORT_TYPE , sg, &filer_sort_type_menu  );
	sg = adr(N_("Sort by Date (atime)"), set_sort, SORT_DATEA, sg, &filer_sort_date_a_menu);
	sg = adr(N_("Sort by Date (ctime)"), set_sort, SORT_DATEC, sg, &filer_sort_date_c_menu);
	sg = adr(N_("Sort by Date (mtime)"), set_sort, SORT_DATEM, sg, &filer_sort_date_m_menu);
	sg = adr(N_("Sort by Size"        ), set_sort, SORT_SIZE , sg, &filer_sort_size_menu  );
	sg = adr(N_("Sort by permissions" ), set_sort, SORT_PERM , sg, &filer_sort_perm_menu  );
	sg = adr(N_("Sort by Owner"       ), set_sort, SORT_OWNER, sg, &filer_sort_owner_menu );
	     adr(N_("Sort by Group"       ), set_sort, SORT_GROUP, sg, &filer_sort_group_menu );

	adt(N_("Reversed"), reverse_sort, 0, &filer_reverse_menu);

	add_separator();

	adt(N_("Show Hidden"), hidden, 0, &filer_hidden_menu);
		sta(GDK_KEY_h, GDK_CONTROL_MASK);
	adt(N_("Show Only Files"              ), only_dirs, 0, &filer_files_only_menu);
	adt(N_("Show Only Directories"        ), only_dirs, 1, &filer_dirs_only_menu);
	adi(N_("Filter Files..."              ), mini_buffer, MINI_FILTER);
	adi(N_("Temp Filter..."               ), mini_buffer, MINI_TEMP_FILTER);
	adt(N_("Filter Directories With Files"), filter_directories, 0, &filer_filter_dirs_menu);
	adt(N_("Show Thumbnails"              ), show_thumbs   , 0, &filer_thumb_menu);
	ads(N_("Refresh"                      ), refresh       , 0, GTK_STOCK_REFRESH);
	ads(N_("Refresh Thumbs"               ), refresh_thumbs, 0, GTK_STOCK_REFRESH);

	adi(N_("Save Display Settings..."           ), save_settings, 0);
	adi(N_("Save Display Settings to parent ..."), save_settings_parent, 0);


	filer_file_menu = start_menu("File", filer_menu);
	filer_file_item = GTK_BIN(current)->child;

	ads(N_("Copy"), file_op, FILE_COPY_TO_CLIPBOARD, GTK_STOCK_COPY);
		sta(GDK_KEY_c, GDK_CONTROL_MASK);
	ads(N_("Cut" ), file_op, FILE_CUT_TO_CLIPBOARD, GTK_STOCK_CUT);
		sta(GDK_KEY_x, GDK_CONTROL_MASK);
	ads(N_("Duplicate..."), file_op, FILE_DUPLICATE_ITEM, GTK_STOCK_COPY);
		sta(GDK_KEY_d, GDK_CONTROL_MASK);
	adi(N_("Rename..."), file_op, FILE_RENAME_ITEM);
	adi(N_("Link..."  ), file_op, FILE_LINK_ITEM);
	ads(N_("Delete"   ), file_op, FILE_DELETE, GTK_STOCK_DELETE);
		sta(GDK_KEY_Delete, 0);

	add_separator();

	file_shift_item =
	adi(N_("Shift Open"), file_op, FILE_OPEN_FILE);
	adi(N_("Send To..."), file_op, FILE_SEND_TO);

	add_separator();

	ads(N_("Set Run Action..."), file_op, FILE_RUN_ACTION, GTK_STOCK_EXECUTE);
		sta(GDK_KEY_asterisk, 0);
	adi(N_("Set Icon..."      ), file_op, FILE_SET_ICON);
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
	filer_xattrs =
	ads(N_("Extended attributes..."), file_op, FILE_XATTRS, ROX_STOCK_XATTR);
#endif
	ads(N_("Properties" ), file_op, FILE_PROPERTIES, GTK_STOCK_PROPERTIES);
		sta(GDK_KEY_p, GDK_CONTROL_MASK);
	adi(N_("Count"      ), file_op, FILE_USAGE);
	filer_set_type =
	adi(N_("Set Type..."), file_op, FILE_SET_TYPE);
	adi(N_("Permissions"), file_op, FILE_CHMOD_ITEMS);

	add_separator();

	ads(N_("Find"), file_op, FILE_FIND, GTK_STOCK_FIND);
		sta(GDK_KEY_f, GDK_CONTROL_MASK);

	start_menu(N_("Select"), filer_menu);

	adi(N_("Select All"       ), select_all, 0);
		sta(GDK_KEY_a, GDK_CONTROL_MASK);
	adi(N_("Clear Selection"  ), clear_selection, 0);
	adi(N_("Invert Selection" ), invert_selection, 0);
	adi(N_("Select by Name..."), mini_buffer, MINI_SELECT_BY_NAME);
		sta(GDK_KEY_period, 0);
	adi(N_("Reg Select..."    ), mini_buffer, MINI_REG_SELECT);
		sta(GDK_KEY_asciicircum, 0);
	adi(N_("Select If..."     ), mini_buffer, MINI_SELECT_IF);
		sta(GDK_KEY_question, GDK_SHIFT_MASK);

	start_menu(NULL, filer_menu);
	ads(N_("Options..."), menu_show_options, 0, GTK_STOCK_PREFERENCES);
	ads(N_("Paste"), paste_from_clipboard, 0, GTK_STOCK_PASTE);
		sta(GDK_KEY_v, GDK_CONTROL_MASK);

	filer_new_menu = start_menu(N_("New"), filer_menu);

	ads(N_("Directory"        ), new_directory, 0, GTK_STOCK_DIRECTORY);
	ads(N_("Blank file"       ), new_file     , 0, GTK_STOCK_NEW);
	adi(N_("Customise Menu..."), customise_new, 0);


	start_menu(N_("Window"), filer_menu);

	ads(N_("Parent, New Window"   ), open_parent     , 0, GTK_STOCK_GO_UP);
	adi(N_("Parent, Same Window"  ), open_parent_same, 0);
	filer_new_window =
	adi(N_("New Window"           ), new_window      , 0);
	ads(N_("Home Directory"       ), home_directory  , 0, GTK_STOCK_HOME);
		sta(GDK_KEY_Home, GDK_CONTROL_MASK);
	ads(N_("Show Bookmarks"       ), show_bookmarks  , 0, ROX_STOCK_BOOKMARKS);
		sta(GDK_KEY_b, GDK_CONTROL_MASK);
	ads(N_("Show Log"             ), show_log        , 0, GTK_STOCK_INFO);
	filer_follow_sym =
	adi(N_("Follow Symbolic Links"), follow_symlinks , 0);
	adi(N_("Resize Window"        ), resize          , 0);
		sta(GDK_KEY_e, GDK_CONTROL_MASK);
	ads(N_("Close Window"         ), close_window    , 0, GTK_STOCK_CLOSE);
		sta(GDK_KEY_q, GDK_CONTROL_MASK);

	add_separator();

	adi(N_("Enter Path..."     ), mini_buffer, MINI_PATH);
		sta(GDK_KEY_slash, 0);
	adi(N_("Shell Command..."  ), mini_buffer, MINI_SHELL);
		sta(GDK_KEY_exclam, GDK_SHIFT_MASK);
	adi(N_("Terminal Here"     ), xterm_here , FALSE);
		sta(GDK_KEY_grave, 0);
	adi(N_("Switch to Terminal"), xterm_here , TRUE);


	start_menu(N_("Help"), filer_menu);

	adi(N_("About ROX-Filer..."), menu_rox_help,  HELP_ABOUT);
	ads(N_("Show Help Files"   ), menu_rox_help,  HELP_DIR, GTK_STOCK_HELP);
		sta(GDK_KEY_F1, 0);
	adi(N_("About ROX-Filer..."), menu_rox_help,  HELP_MANUAL);

	start_menu(NULL, filer_menu);
	adi(N_("Customise Dir Menu..."), customise_directory_menu, 0);


	g_signal_connect(filer_menu, "selection-done",
			G_CALLBACK(menu_closed), NULL);
	g_signal_connect(filer_file_menu, "selection-done",
			G_CALLBACK(menu_closed), NULL);

	g_signal_connect(filer_keys, "accel_changed",
			G_CALLBACK(save_menus), NULL);

	//for accel
	update_new_files_menu();
	update_directory_menu();
	gtk_widget_show_all(filer_menu);

	return TRUE;
}

/* 'item' is the number of the item to appear under the pointer. */
void show_popup_menu(GtkWidget *menu, GdkEvent *event, int item)
{
	int		pos[3];
	int		button = 0;
	guint32		time = 0;

	if (event && (event->type == GDK_BUTTON_PRESS ||
			event->type == GDK_BUTTON_RELEASE))
	{
		GdkEventButton *bev = (GdkEventButton *) event;

		pos[0] = bev->x_root;
		pos[1] = bev->y_root;
		button = bev->button;
		time = bev->time;
	}
	else if (event && event->type == GDK_KEY_PRESS)
	{
		GdkEventKey *kev = (GdkEventKey *) event;

		get_pointer_xy(pos, pos + 1);
		time = kev->time;
	}
	else
		get_pointer_xy(pos, pos + 1);

	pos[2] = item;

	gtk_widget_show_all(menu);
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL,
			position_menu, (gpointer) pos, button, time);
	select_nth_item(GTK_MENU_SHELL(menu), item);
}

/* Hide the popup menu, if any */
void menu_popdown(void)
{
	if (popup_menu)
		gtk_menu_popdown(GTK_MENU(popup_menu));
}

static void clipboardcb(
		GtkClipboard *clipboard,
		GtkSelectionData *data,
		gpointer p)
{
	if (data->length > 0)
		menu_set_items_shaded(filer_menu, false, 4, 1);
	else if (GPOINTER_TO_INT(p))
		gtk_clipboard_request_contents(
				clipboard, text_uri_list, clipboardcb, GUINT_TO_POINTER(0));
}

/* iter->peek() is the clicked item, or NULL if none */
void show_filer_menu(FilerWindow *filer_window, GdkEvent *event, ViewIter *iter)
{
	DirItem		*file_item = NULL;
	GdkModifierType	state = 0;
	int		n_selected;
	int             n_added = 0;

	g_return_if_fail(event != NULL);

	n_selected = view_count_selected(filer_window->view);

	ensure_filer_menu();

	updating_menu++;

	/* Remove previous AppMenu, if any */
	appmenu_remove();

	window_with_focus = filer_window;

	if (event->type == GDK_BUTTON_PRESS)
		state = ((GdkEventButton *) event)->state;
	else if (event->type == GDK_KEY_PRESS)
		state = ((GdkEventKey *) event)->state;

	if (n_selected == 0 && iter && iter->peek(iter) != NULL)
	{
		filer_window->temp_item_selected = TRUE;
		view_set_selected(filer_window->view, iter, TRUE);
		n_selected = view_count_selected(filer_window->view);
	}
	else
	{
		filer_window->temp_item_selected = FALSE;
	}

	/* Determine whether to shade "Paste" option */
	menu_set_items_shaded(filer_menu, true, 4, 1);
	gtk_clipboard_request_contents(
			clipboard, gnome_copied_files, clipboardcb, GUINT_TO_POINTER(1));

	/* Short-cut to the Send To menu */
	if (state & GDK_SHIFT_MASK)
	{
		updating_menu--;

		if (n_selected == 0)
		{
			show_dir_send_to_menu(event);
			return;
		}

		/* (paths eaten) */
		show_send_to_menu(filer_window, event);
		return;
	}

	{
		GtkWidget	*file_label;
		GString		*buffer;
		DirItem		*item;

		file_label = filer_file_item;
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_thumb_menu),
				filer_window->show_thumbs);
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_hidden_menu),
				filer_window->show_hidden);
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_files_only_menu),
				filer_window->files_only);
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_dirs_only_menu),
				filer_window->dirs_only);
		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_filter_dirs_menu),
				filer_window->filter_directories);

		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_name_menu  ),
				filer_window->sort_type == SORT_NAME );
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_type_menu  ),
				filer_window->sort_type == SORT_TYPE );
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_date_a_menu),
				filer_window->sort_type == SORT_DATEA);
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_date_c_menu),
				filer_window->sort_type == SORT_DATEC);
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_date_m_menu),
				filer_window->sort_type == SORT_DATEM);
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_size_menu  ),
				filer_window->sort_type == SORT_SIZE );
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_perm_menu  ),
				filer_window->sort_type == SORT_PERM );
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_owner_menu ),
				filer_window->sort_type == SORT_OWNER);
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(filer_sort_group_menu ),
				filer_window->sort_type == SORT_GROUP);

		gtk_check_menu_item_set_active(
				GTK_CHECK_MENU_ITEM(filer_reverse_menu),
				filer_window->sort_order != GTK_SORT_ASCENDING);
		gtk_check_menu_item_set_active(
			GTK_CHECK_MENU_ITEM(filer_auto_size_menu),
			filer_window->display_style_wanted == AUTO_SIZE_ICONS);
		buffer = g_string_new(NULL);

		switch (n_selected)
		{
			case 0:
				g_string_assign(buffer, _("Next Click"));
				shade_file_menu_items(FALSE);
				break;
			case 1:
				item = filer_selected_item(filer_window);
				if (item->base_type == TYPE_UNKNOWN)
					dir_update_item(filer_window->directory,
							item->leafname);
				shade_file_menu_items(FALSE);
				file_item = filer_selected_item(filer_window);
				g_string_printf(buffer, _("%s '%s'"),
					basetype_name(file_item),
					g_utf8_validate(file_item->leafname,
							-1, NULL)
						? file_item->leafname
						: _("(bad utf-8)"));
				if (!can_set_run_action(file_item))
					menu_set_items_shaded(filer_file_menu,
							TRUE, 10, 1);
				break;
			default:
				shade_file_menu_items(TRUE);
				g_string_printf(buffer, _("%d items"),
						 n_selected);
				break;
		}
		gtk_label_set_text(GTK_LABEL(file_label), buffer->str);
		g_string_free(buffer, TRUE);

		menu_show_shift_action(file_shift_item, file_item,
					n_selected == 0);

		if (n_selected)
			n_added = file_item ?
				appmenu_add(NULL, make_path(filer_window->sym_path, file_item->leafname),
							file_item, filer_file_menu)
				:
				appmenu_add(filer_window, NULL, NULL, filer_file_menu);
	}

	update_new_files_menu();
	update_directory_menu();

	gtk_widget_set_sensitive(filer_new_window,
			!o_unique_filer_windows.int_value);
	gtk_widget_set_sensitive(filer_follow_sym,
		strcmp(filer_window->sym_path, filer_window->real_path) != 0);
	gtk_widget_set_sensitive(filer_set_type,
				 xattr_supported(filer_window->real_path));
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
	gtk_widget_set_sensitive(filer_xattrs,
				 xattr_supported(filer_window->real_path) && n_selected <= 1);
#endif

	if (n_selected && o_menu_quick.int_value)
		popup_menu = (state & GDK_CONTROL_MASK)
					? filer_menu
					: filer_file_menu;
	else
		popup_menu = (state & GDK_CONTROL_MASK)
					? filer_file_menu
					: filer_menu;

	updating_menu--;

	show_popup_menu(popup_menu, event,
			popup_menu == filer_file_menu ? n_added : 1);
}

static void menu_closed(GtkWidget *widget)
{
	if (window_with_focus == NULL || widget != popup_menu)
		return;			/* Close panel item chosen? */

	popup_menu = NULL;

	if (window_with_focus->temp_item_selected)
	{
		view_clear_selection(window_with_focus->view);
		window_with_focus->temp_item_selected = FALSE;
	}

	appmenu_remove();
}

static void target_callback(FilerWindow *filer_window,
			ViewIter *iter,
			gpointer action)
{
	g_return_if_fail(filer_window != NULL);

	window_with_focus = filer_window;

	/* Don't grab the primary selection */
	filer_window->temp_item_selected = TRUE;

	view_wink_item(filer_window->view, iter);
	view_select_only(filer_window->view, iter);
	file_op(NULL, GPOINTER_TO_INT(action), NULL);

	view_clear_selection(filer_window->view);
	filer_window->temp_item_selected = FALSE;
}

/* Set the text of the 'Shift Open...' menu item.
 * If icon is NULL, reset the text and also shade it, unless 'next'.
 */
void menu_show_shift_action(GtkWidget *menu_item, DirItem *item, gboolean next)
{
	guchar		*shift_action = NULL;

	if (item)
	{
		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			if (item->flags & ITEM_FLAG_MOUNTED)
				shift_action = N_("Unmount");
			else
				shift_action = N_("Open unmounted");
		}
		else if (item->flags & ITEM_FLAG_SYMLINK)
			shift_action = N_("Show Target");
		else if (item->base_type == TYPE_DIRECTORY)
			shift_action = N_("Look Inside");
		else if (item->base_type == TYPE_FILE)
			shift_action = N_("Open As Text");
	}
	gtk_label_set_text(GTK_LABEL(menu_item),
			shift_action ? _(shift_action)
				     : _("Shift Open"));
	gtk_widget_set_sensitive(menu_item, shift_action != NULL || next);
}

/* Actions */

static void view_type(gpointer data, guint action, GtkWidget *widget)
{
	ViewType view_type = (ViewType) action;

	g_return_if_fail(window_with_focus != NULL);

	if (view_type == VIEW_TYPE_COLLECTION)
		display_set_layout(window_with_focus,
				window_with_focus->display_style_wanted,
				DETAILS_NONE, FALSE);

	filer_set_view_type(window_with_focus, (ViewType) action);
}

static void change_size(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	display_change_size(window_with_focus, action == 1);
}

static void change_size_auto(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	if (updating_menu)
		return;

	if (window_with_focus->display_style_wanted == AUTO_SIZE_ICONS)
		display_set_layout(window_with_focus,
				   window_with_focus->display_style,
				   window_with_focus->details_type, FALSE);
	else
		display_set_layout(window_with_focus, AUTO_SIZE_ICONS,
				   window_with_focus->details_type, FALSE);
}

static void set_with(gpointer data, guint action, GtkWidget *widget)
{
	DisplayStyle size;

	g_return_if_fail(window_with_focus != NULL);

	size = window_with_focus->display_style_wanted;

	filer_set_view_type(window_with_focus, VIEW_TYPE_COLLECTION);
	display_set_layout(window_with_focus, size, action, FALSE);
}
static void wink_if(FilerWindow *fw)
{
	if (!fw->temp_item_selected &&
			view_count_selected(fw->view) == 1)
	{
		ViewIter iter;
		view_get_iter(fw->view, &iter, VIEW_ITER_SELECTED);
		if (iter.next(&iter))
			view_wink_item(fw->view, &iter);
	} else {
		view_scroll_to_top(fw->view);
	}
}
static void set_sort(gpointer data, guint action, GtkWidget *widget)
{
	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);

	display_set_sort_type(window_with_focus, action, GTK_SORT_ASCENDING);
	wink_if(window_with_focus);
}

static void reverse_sort(gpointer data, guint action, GtkWidget *widget)
{
	GtkSortType order;

	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);

	order = window_with_focus->sort_order;
	if (order == GTK_SORT_ASCENDING)
		order = GTK_SORT_DESCENDING;
	else
		order = GTK_SORT_ASCENDING;

	display_set_sort_type(window_with_focus, window_with_focus->sort_type,
			      order);
	wink_if(window_with_focus);
}

static void hidden(gpointer data, guint action, GtkWidget *widget)
{
	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);

	display_set_hidden(window_with_focus,
			   !window_with_focus->show_hidden);
}
static void only_dirs(gpointer data, guint action, GtkWidget *widget)
{
	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);
	FilerWindow *fw = window_with_focus;

	if (action) //dir
	{
		fw->dirs_only = !fw->dirs_only;
		fw->files_only = FALSE;
	}
	else //faile
	{
		fw->dirs_only = FALSE;
		fw->files_only = !fw->files_only;
	}
	display_update_hidden(fw);
}

static void filter_directories(gpointer data, guint action, GtkWidget *widget)
{
	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);

	display_set_filter_directories(window_with_focus,
			   !window_with_focus->filter_directories);
}

static void show_thumbs(gpointer data, guint action, GtkWidget *widget)
{
	if (updating_menu)
		return;

	g_return_if_fail(window_with_focus != NULL);

	display_set_thumbs(window_with_focus, !window_with_focus->show_thumbs);
}

static void refresh(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_refresh(window_with_focus);
}

static void refresh_thumbs(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_refresh_thumbs(window_with_focus);
}

static void save_settings(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_save_settings(window_with_focus, FALSE);
}

static void save_settings_parent(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_save_settings(window_with_focus, TRUE);
}

static void delete(FilerWindow *filer_window)
{
	GList *paths;
	paths = filer_selected_items(filer_window);
	action_delete(paths);
	destroy_glist(&paths);
}

static void usage(FilerWindow *filer_window)
{
	GList *paths;
	paths = filer_selected_items(filer_window);
	action_usage(paths);
	destroy_glist(&paths);
}

static void chmod_items(FilerWindow *filer_window)
{
	GList *paths;
	paths = filer_selected_items(filer_window);
	action_chmod(paths, FALSE, NULL);
	destroy_glist(&paths);
}

static void set_type_items(FilerWindow *filer_window)
{
	GList *paths, *p;
	int npass=0, nfail=0;

	paths = filer_selected_items(filer_window);
	for(p=paths; p; p=g_list_next(p)) {
		if(xattr_supported((const char *) p->data))
			npass++;
		else
			nfail++;
	}
	if(npass==0)
		report_error(_("Extended attributes, used to store types, are not supported for this "
				"file or files.\n"
				"This may be due to lack of support from the filesystem or the C library, "
				"or it may simply be that the filesystem needs to be mounted with "
				"the right mount option ('user_xattr' on Linux)."));
	else if(nfail>0)
		report_error(_("Setting type not supported for some of these files"));
	if(npass>0)
		action_settype(paths, FALSE, NULL);
	destroy_glist(&paths);
}

static void find(FilerWindow *filer_window)
{
	GList *paths;
	paths = filer_selected_items(filer_window);
	action_find(paths);
	destroy_glist(&paths);
}

static gboolean last_symlink_check_relative = TRUE;
static gboolean last_symlink_check_sympath = FALSE;

/* This creates a new savebox widget, and allows the user to pick a new path
 * for the file.
 * Once the new path has been picked, the callback will be called with
 * both the current and new paths.
 * NOTE: This function unrefs 'image'!
 */
static void savebox_show(const gchar *action, const gchar *path,
			 MaskedPixmap *image, SaveCb callback,
			 GdkDragAction dnd_action)
{
	GtkWidget *savebox = NULL;
	GtkWidget *check_relative = NULL, *check_sympath = NULL;

	g_return_if_fail(image != NULL);

	savebox = gtk_savebox_new(action);
	gtk_savebox_set_action(GTK_SAVEBOX(savebox), dnd_action);

	if (callback == link_cb)
	{
		check_relative = gtk_check_button_new_with_mnemonic(
							_("_Relative link"));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_relative),
					     last_symlink_check_relative);

		gtk_widget_set_can_focus(check_relative, FALSE);
		gtk_widget_set_tooltip_text(check_relative,
			_("If on, the symlink will store the path from the "
			"symlink to the target file. Use this if the symlink "
			"and the target will be moved together.\n"
			"If off, the path from the root directory is stored - "
			"use this if the symlink may move but the target will "
			"stay put."));
		gtk_box_pack_start(GTK_BOX(GTK_DIALOG(savebox)->vbox),
				check_relative, FALSE, TRUE, 0);
		gtk_widget_show(check_relative);


		check_sympath = gtk_check_button_new_with_mnemonic(
							_("_Sym path"));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check_sympath),
					     last_symlink_check_sympath);

		gtk_widget_set_can_focus(check_sympath, FALSE);
		gtk_widget_set_tooltip_text(check_sympath,
			_("If on, the symlink will use target path as Sym path."));
		gtk_box_pack_start(GTK_BOX(GTK_DIALOG(savebox)->vbox),
				check_sympath, FALSE, TRUE, 0);
		gtk_widget_show(check_sympath);
	}
	else if (callback == rename_cb)
	{
		gtk_window_set_default_size(GTK_WINDOW(savebox), 400, -1);
	}

	g_signal_connect(savebox, "save_to_file",
				G_CALLBACK(save_to_file), NULL);

	g_object_set_data_full(G_OBJECT(savebox), "current_path",
				g_strdup(path), g_free);
	g_object_set_data(G_OBJECT(savebox), "action_callback", callback);
	g_object_set_data(G_OBJECT(savebox), "check_relative", check_relative);
	g_object_set_data(G_OBJECT(savebox), "check_sympath", check_sympath);

	gtk_window_set_title(GTK_WINDOW(savebox), action);

	if (g_utf8_validate(path, -1, NULL))
		gtk_savebox_set_pathname(GTK_SAVEBOX(savebox), path);
	else
	{
		gchar *u8, *dir, *base;
		dir = g_path_get_dirname(path);
		base = g_path_get_basename(path);
		u8 = to_utf8(base);
		gtk_savebox_set_pathname(GTK_SAVEBOX(savebox),
				make_path(dir, u8));
		g_free(u8);
		g_free(dir);
		g_free(base);
	}
	gtk_savebox_set_icon(GTK_SAVEBOX(savebox), image->pixbuf);
	g_object_unref(image);

	gtk_widget_show(savebox);
}

static gint save_to_file(GObject *savebox,
			 const gchar *pathname, gpointer data)
{
	SaveCb		callback;
	const gchar	*current_path;

	callback = g_object_get_data(savebox, "action_callback");
	current_path = g_object_get_data(savebox, "current_path");

	g_return_val_if_fail(callback != NULL, GTK_XDS_SAVE_ERROR);
	g_return_val_if_fail(current_path != NULL, GTK_XDS_SAVE_ERROR);

	return callback(savebox, current_path, pathname)
			? GTK_XDS_SAVED : GTK_XDS_SAVE_ERROR;
}

static gboolean copy_cb(GObject *savebox,
			const gchar *current, const gchar *new)
{
	return action_with_leaf(action_copy, current, new);
}

static gboolean action_with_leaf(ActionFn action,
				 const gchar *current, const gchar *new)
{
	const char	*leaf;
	char		*new_dir;
	GList		*local_paths;

	if (new[0] != '/')
	{
		report_error(_("New pathname is not absolute"));
		return FALSE;
	}

	if (new[strlen(new) - 1] == '/')
	{
		new_dir = g_strdup(new);
		leaf = NULL;
	}
	else
	{
		const gchar *slash;

		slash = strrchr(new, '/');
		new_dir = g_strndup(new, slash - new);
		leaf = slash + 1;
	}

	local_paths = g_list_append(NULL, (gchar *) current);
	action(local_paths, new_dir, leaf, -1);
	g_list_free(local_paths);

	g_free(new_dir);

	return TRUE;
}

/* Open a savebox to act on the selected file.
 * Call 'callback' later to perform the operation.
 */
static void src_dest_action_item(const gchar *path, MaskedPixmap *image,
			 const gchar *action, SaveCb callback,
			 GdkDragAction dnd_action)
{
	g_object_ref(image);
	savebox_show(action, path, image, callback, dnd_action);
}

static gboolean rename_cb(GObject *savebox,
			  const gchar *current, const gchar *new)
{
	return action_with_leaf(action_move, current, new);
}

static gboolean link_cb(GObject *savebox,
			const gchar *initial, const gchar *path)
{
	GtkToggleButton *check_relative, *check_sympath;
	struct stat info;
	int	err;
	gchar	*link_path;

	check_relative = g_object_get_data(savebox, "check_relative");
	check_sympath = g_object_get_data(savebox, "check_sympath");

	last_symlink_check_relative = gtk_toggle_button_get_active(check_relative);
	last_symlink_check_sympath = gtk_toggle_button_get_active(check_sympath);

	if (last_symlink_check_relative)
	{
		if (last_symlink_check_sympath)
			link_path = get_relative_path(path, initial);
		else
		{
			gchar *real = pathdup(initial);
			link_path = get_relative_path(path, real);
			g_free(real);
		}
	}
	else
		link_path = last_symlink_check_sympath ?
			g_strdup(initial) : pathdup(initial);

	if (mc_lstat(path, &info) == 0 && S_ISLNK(info.st_mode))
	{
		GtkWidget *box, *button;
		gint ans;

		box = gtk_message_dialog_new(NULL, 0, GTK_MESSAGE_QUESTION,
				GTK_BUTTONS_CANCEL,
				_("Symlink from '%s' already exists. "
				"Replace it with a link to '%s'?"),
				path, link_path);

		gtk_window_set_position(GTK_WINDOW(box), GTK_WIN_POS_MOUSE);

		button = button_new_mixed(GTK_STOCK_YES, _("_Replace"));
		gtk_widget_show(button);
		gtk_widget_set_can_default(button, TRUE);
		gtk_dialog_add_action_widget(GTK_DIALOG(box),
					     button, GTK_RESPONSE_OK);
		gtk_dialog_set_default_response(GTK_DIALOG(box),
						GTK_RESPONSE_OK);

		ans = gtk_dialog_run(GTK_DIALOG(box));
		gtk_widget_destroy(box);

		if (ans != GTK_RESPONSE_OK)
		{
			g_free(link_path);
			return FALSE;
		}

		unlink(path);
	}

	err = symlink(link_path, path);
	g_free(link_path);

	if (err)
	{
		report_error("symlink: %s", g_strerror(errno));
		return FALSE;
	}

	dir_check_this(path);

	return TRUE;
}

static void run_action(DirItem *item)
{
	if (can_set_run_action(item))
		type_set_handler_dialog(item->mime_type);
	else
		report_error(
			_("You can only set the run action for a "
			"regular file"));
}

void open_home(gpointer data, guint action, GtkWidget *widget)
{
	filer_opendir(home_dir, NULL, NULL, FALSE);
}

static void select_all(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	window_with_focus->temp_item_selected = FALSE;
	view_select_all(window_with_focus->view);
}

static void clear_selection(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	window_with_focus->temp_item_selected = FALSE;
	view_clear_selection(window_with_focus->view);
}

static gboolean invert_cb(ViewIter *iter, gpointer data)
{
	return !view_get_selected((ViewIface *) data, iter);
}

static void invert_selection(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	window_with_focus->temp_item_selected = FALSE;

	view_select_if(window_with_focus->view, invert_cb,
		       window_with_focus->view);
}

void menu_show_options(gpointer data, guint action, GtkWidget *widget)
{
	GtkWidget *win;

	win = options_show();

	if (win)
	{
		number_of_windows++;
		g_signal_connect(win, "destroy",
				G_CALLBACK(one_less_window), NULL);
	}
}

static gchar *add_seqnum(const gchar *base) {
	gchar *ret = NULL;
	gboolean pass;
	int i;

	/* check exists names */
	for (i = 1; i <= 9999; i++) {
		pass = TRUE;

		g_free(ret);
		if (i == 1)
			ret = g_strdup_printf("%s", base);
		else
			ret = g_strdup_printf("%s%d", base, i);

		DirItem  *item;
		ViewIter iter;
		view_get_iter(window_with_focus->view, &iter, 0);
		while ((item = iter.next(&iter)))
			if (strcmp(item->leafname, ret) == 0)
			{
				pass = FALSE;
				break;
			}

		if (pass)
			break;

	}
	return ret;
}

static gboolean new_directory_cb(GObject *savebox,
				 const gchar *initial, const gchar *path)
{
	if (mkdir(path, S_IRWXU | S_IRWXG | S_IRWXO))
	{
		report_error("mkdir: %s", g_strerror(errno));
		return FALSE;
	}

	dir_check_this(path);

	if (filer_exists(window_with_focus))
	{
		guchar	*leaf;
		leaf = strrchr(path, '/');
		if (leaf)
			display_set_autoselect(window_with_focus, leaf + 1);
	}

	return TRUE;
}

void show_new_directory(FilerWindow *filer_window)
{
	window_with_focus = filer_window;
	new_directory(NULL, 0, NULL);
}

static void new_directory(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	gchar *leaf = add_seqnum(_("NewDir"));

	savebox_show(_("Create"),
		make_path(window_with_focus->sym_path, leaf),
		type_to_icon(inode_directory), new_directory_cb,
		GDK_ACTION_COPY);

	g_free(leaf);
}

static gboolean new_file_cb(GObject *savebox,
			    const gchar *initial, const gchar *path)
{
	int fd;

	fd = open(path, O_CREAT | O_EXCL, 0666);

	if (fd == -1)
	{
		report_error(_("Error creating '%s': %s"),
				path, g_strerror(errno));
		return FALSE;
	}

	if (close(fd))
		report_error(_("Error creating '%s': %s"),
				path, g_strerror(errno));

	dir_check_this(path);

	if (filer_exists(window_with_focus))
	{
		guchar	*leaf;
		leaf = strrchr(path, '/');
		if (leaf)
			display_set_autoselect(window_with_focus, leaf + 1);
	}

	return TRUE;
}

void show_new_file(FilerWindow *filer_window)
{
	window_with_focus = filer_window;
	new_file(NULL, 0, NULL);
}

static void new_file(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	gchar *leaf = add_seqnum(_("NewFile"));

	savebox_show(_("Create"),
		make_path(window_with_focus->sym_path, leaf),
		type_to_icon(text_plain),
		new_file_cb, GDK_ACTION_COPY);

	g_free(leaf);
}

static gboolean new_file_type_cb(GObject *savebox,
			         const gchar *initial, const gchar *path)
{
	const gchar *oleaf, *leaf;
	gchar *templ, *rtempl, *templ_dname, *dest, *base;
	GList *paths;

	/* We can work out the template path from the initial name */
	base = g_path_get_basename(initial);
	oleaf = base;
	templ_dname = choices_find_xdg_path_load("Templates", "", SITE);
	if (!templ_dname)
	{
		report_error(
		_("Error creating file: could not find the template for %s"),
				oleaf);
		return FALSE;
	}

	templ = g_strconcat(templ_dname, "/", oleaf, NULL);
	g_free(templ_dname);
	rtempl = pathdup(templ);
	g_free(templ);
	g_free(base);

	base = g_path_get_basename(path);
	dest = g_path_get_dirname(path);
	leaf = base;
	paths = g_list_append(NULL, rtempl);

	action_copy(paths, dest, leaf, TRUE);

	g_list_free(paths);
	g_free(dest);
	g_free(rtempl);
	g_free(base);

	if (filer_exists(window_with_focus))
		display_set_autoselect(window_with_focus, leaf);

	return TRUE;
}

static void do_send_to(gchar *templ)
{
	g_return_if_fail(send_to_paths != NULL);

	run_with_files(templ, send_to_paths, FALSE);
}

static void new_file_type(gchar *templ)
{
	const gchar *leaf;
	MIME_type *type;
	gchar *base;

	g_return_if_fail(window_with_focus != NULL);

	base = g_path_get_basename(templ);
	leaf = base;
	type = type_get_type(templ);

	savebox_show(_("Create"),
		make_path(window_with_focus->sym_path, leaf),
		type_to_icon(type),
		new_file_type_cb, GDK_ACTION_COPY);
	g_free(base);
}

static void customise_directory_menu(gpointer data)
{
	char *path;
	char *leaf = g_strconcat(".", inode_directory->media_type, NULL);

	path = choices_find_xdg_path_save(leaf, "SendTo", SITE, TRUE);
	g_free(leaf);

	mkdir(path, 0755);
	filer_opendir(path, NULL, NULL, FALSE);
	g_free(path);

	info_message(
			_("Symlink any programs you want into this directory. \n\n"
			"Tip: Directories and `Set Icon' may make it more usefull."));
}

void show_menu_new(FilerWindow *filer_window)
{
	window_with_focus = filer_window;
	ensure_filer_menu();
	update_new_files_menu(get_menu_icon_style());
	show_popup_menu(filer_new_menu, NULL, 1);
}

static void customise_send_to(gpointer data)
{
	GPtrArray	*path;
	guchar		*save;
	GString		*dirs;
	int		i;

	dirs = g_string_new(NULL);

	path = choices_list_xdg_dirs("", SITE);
	for (i = 0; i < path->len; i++)
	{
		guchar *old = (guchar *) path->pdata[i];

		g_string_append(dirs, old);
		g_string_append(dirs, "/SendTo\n");
	}
	choices_free_list(path);

	save = choices_find_xdg_path_save("", "SendTo", SITE, TRUE);
	if (save)
		mkdir(save, 0777);

	info_message(
		_("The `Send To' menu provides a quick way to send some files "
		"to an application. The applications listed are those in "
		"the following directories:\n\n%s\n%s\n"
		"The `Send To' menu may be opened by Shift+Menu clicking "
		"over a file.\n\n"
		"Advanced use:\n"
		"You can also create subdirectories called "
		"`.text_html', `.text', etc which will only be "
		"shown for files of that type and shared with the file menu. "
		"In addition, `.group' is shown only when multiple files are selected. "
		"`.all' is all."),
		dirs->str,
		save ? _("I'll show you your SendTo directory now; you should "
			"symlink (Ctrl+Shift drag) any applications you want "
			"into it.")
		     : _("Your CHOICESPATH variable setting prevents "
			 "customisations - sorry."));

	g_string_free(dirs, TRUE);

	if (save)
		filer_opendir(save, NULL, NULL, FALSE);
}

static void customise_new(gpointer data)
{
	GPtrArray	*path;
	guchar		*save;
	GString		*dirs;
	int		i;

	dirs = g_string_new(NULL);

	path = choices_list_xdg_dirs("", SITE);
	for (i = 0; i < path->len; i++)
	{
		guchar *old = (guchar *) path->pdata[i];

		g_string_append(dirs, old);
		g_string_append(dirs, "/Templates\n");
	}
	choices_free_list(path);

	save = choices_find_xdg_path_save("", "Templates", SITE, TRUE);
	if (save)
		mkdir(save, 0777);

	info_message(
		_("Any files placed in your Templates directories will "
		"appear on the `New' menu. Choosing one of them will make "
		"a copy of it as the new file.\n\n"
		"The following directories contain templates:\n\n%s\n%s\n"),
		dirs->str,
		save ? _("I'll show you your Templates directory now; you "
			 "should place any template files you want inside it.")
		     : _("Your CHOICESPATH variable setting prevents "
			 "customisations - sorry."));

	g_string_free(dirs, TRUE);

	if (save)
		filer_opendir(save, NULL, NULL, FALSE);
}

/* Add everything in the directory <Choices>/SendTo/[.type[_subtype]]
 * to the menu.
 */
static GList *add_sendto_shared(GtkWidget *menu,
		const gchar *type, const gchar *subtype, CallbackFn swapped_func)
{
	gchar *searchdir;
	GPtrArray *paths;
	GList *widgets = NULL;
	int i;

	if (subtype)
		searchdir = g_strdup_printf("SendTo/.%s_%s", type, subtype);
	else if (type)
		searchdir = g_strdup_printf("SendTo/.%s", type);
	else
		searchdir = g_strdup("SendTo");

	paths = choices_list_xdg_dirs(searchdir, SITE);
	g_free(searchdir);

	for (i = 0; i < paths->len; i++)
	{
		guchar	*dir = (guchar *) paths->pdata[i];

		widgets = g_list_concat(widgets,
				menu_from_dir(menu, dir, get_menu_icon_style(),
					swapped_func, FALSE, FALSE, TRUE)
			);
	}

	choices_free_list(paths);
	return widgets;
}
static void add_sendto(GList **list, const gchar *type, const gchar *subtype)
{
	GList *new = add_sendto_shared(NULL, type, subtype, (CallbackFn) do_send_to);
	if (!new) return;
	if (*list)
		new = g_list_prepend(new, gtk_menu_item_new());
	*list = g_list_concat(*list, new);
}


MIME_type *menu_selection_type(FilerWindow *fw)
{
	MIME_type *type=NULL;
	gboolean same=TRUE, same_media=TRUE;

	ViewIter iter;
	DirItem *item;
	view_get_iter(fw->view, &iter, VIEW_ITER_SELECTED);
	while ((item = iter.next(&iter)))
	{
		if (item->mime_type == NULL)
		{
			same = same_media = FALSE;
			break;
		}

		if(!type)
			type=item->mime_type;
		else
		{
			if(type!=item->mime_type)
			{
				same=FALSE;
				if(strcmp(type->media_type,
					  item->mime_type->media_type)!=0)
				{
					same_media=FALSE;
					break;
				}
			}
		}
	}

	static MIME_type ret;
	ret.media_type = ret.subtype = NULL;
	if(type)
	{
		if(same)
			ret.subtype    = type->subtype;
		if(same_media)
			ret.media_type = type->media_type;
	}
	return &ret;
}

//don't free paths
GList *menu_sendto_for_type(GList *paths, MIME_type *type)
{
	GList *ret = NULL;
	if (!paths)
		;
	if (!paths->next)
	{
		add_sendto(&ret, type->media_type, type->subtype);
		add_sendto(&ret, type->media_type, NULL);
	}
	else
	{
		if(type->subtype)
			add_sendto(&ret, type->media_type, type->subtype);
		if(type->media_type)
			add_sendto(&ret, type->media_type, NULL);

		add_sendto(&ret, "group", NULL);
	}

	add_sendto(&ret, "all", NULL);

	if (send_to_paths) destroy_glist(&send_to_paths);
	send_to_paths = paths;
	return ret;
}

/* Scan the SendTo dir and create and show the Send To menu.
 * The 'paths' list and every path in it is claimed, and will be
 * freed later -- don't free it yourself!
 */
static void show_send_to_menu(FilerWindow *fw, GdkEvent *event)
{
	GtkWidget	*menu, *item;

	menu = gtk_menu_new();

	GList *list = menu_sendto_for_type(
			filer_selected_items(fw), menu_selection_type(fw));
	add_sendto(&list, NULL, NULL);

	for (; list; list = g_list_delete_link(list, list))
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), list->data);

	item = gtk_menu_item_new_with_label(_("Customise..."));
	g_signal_connect_swapped(item, "activate",
				G_CALLBACK(customise_send_to), NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);

	g_signal_connect(menu, "selection-done", G_CALLBACK(menu_closed), NULL);

	popup_menu = menu;
	show_popup_menu(menu, event, 0);
}

static void show_dir_send_to_menu(GdkEvent *event)
{
	GtkWidget	*menu, *item;

	menu = gtk_menu_new();

	GList *widgets = add_sendto_shared(menu,
			inode_directory->media_type, NULL, (CallbackFn) directory_cb);
	if (widgets)
	{
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), gtk_menu_item_new());
		g_list_free(widgets);
	}

	item = gtk_menu_item_new_with_label(_("Customise Dir menu..."));
	g_signal_connect_swapped(item, "activate",
				G_CALLBACK(customise_directory_menu), NULL);
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);


	g_signal_connect(menu, "selection-done", G_CALLBACK(menu_closed), NULL);

	popup_menu = menu;
	show_popup_menu(menu, event, 0);
}

static void send_to(FilerWindow *filer_window)
{
	GdkEvent *event = gtk_get_current_event();
	/* Eats paths */
	show_send_to_menu(filer_window, event);
	if (event)
		gdk_event_free(event);
}

static void xterm_here(gpointer data, guint action, GtkWidget *widget)
{
	const char *argv[] = {"sh", "-c", NULL, NULL};
	gboolean close = action;

	argv[2] = o_menu_xterm.value;

	g_return_if_fail(window_with_focus != NULL);

	if (rox_spawn(window_with_focus->sym_path, argv) && close)
		gtk_widget_destroy(window_with_focus->window);
}

static void home_directory(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_change_to(window_with_focus, home_dir, NULL);
}

static void show_bookmarks(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	bookmarks_show_menu(window_with_focus, NULL);
}

static void show_log(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	log_show_window();
}

static void follow_symlinks(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	if (strcmp(window_with_focus->real_path, window_with_focus->sym_path))
		filer_change_to(window_with_focus,
				window_with_focus->real_path, NULL);
	else
		delayed_error(_("This is already the canonical name "
				"for this directory."));
}

static void open_parent(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	filer_open_parent(window_with_focus);
}

static void open_parent_same(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	change_to_parent(window_with_focus);
}

static void resize(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	view_autosize(window_with_focus->view, TRUE);
}

static void new_window(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	if (o_unique_filer_windows.int_value)
	{
		report_error(_("You can't open a second view onto "
			"this directory because the `Unique Windows' option "
			"is turned on in the Options window."));
	}
	else
		filer_opendir(window_with_focus->sym_path, window_with_focus, NULL, FALSE);
}

static void close_window(gpointer data, guint action, GtkWidget *widget)
{
	g_return_if_fail(window_with_focus != NULL);

	if (!filer_window_delete(window_with_focus->window, NULL,
				 window_with_focus))
		gtk_widget_destroy(window_with_focus->window);
}

static void mini_buffer(gpointer data, guint action, GtkWidget *widget)
{
	MiniType type = (MiniType) action;

	g_return_if_fail(window_with_focus != NULL);

	/* Item needs to remain selected... */
	if (type == MINI_SHELL)
		window_with_focus->temp_item_selected = FALSE;

	minibuffer_show(window_with_focus, type, 0);
}

void menu_rox_help(gpointer data, guint action, GtkWidget *widget)
{
	if (action == HELP_ABOUT)
		infobox_new(app_dir);
	else if (action == HELP_DIR)
		filer_opendir(make_path(app_dir, "Help"), NULL, NULL, FALSE);
	else if (action == HELP_MANUAL)
	{
		gchar *manual = NULL;

		if (current_lang)
		{
			manual = g_strconcat(app_dir, "/Help/Manual-",
					     current_lang, ".html", NULL);
			if (!file_exists(manual) && strchr(current_lang, '_'))
			{
				/* Try again without the territory */
				strcpy(strrchr(manual, '_'), ".html");
			}
			if (!file_exists(manual))
				null_g_free(&manual);
		}

		if (!manual)
			manual = g_strconcat(app_dir,
						"/Help/Manual.html", NULL);

		run_by_path(manual);

		g_free(manual);
	}
	else
		g_warning("Unknown help action %d\n", action);
}

/* Set n items from position 'from' in 'menu' to the 'shaded' state */
void menu_set_items_shaded(GtkWidget *menu, gboolean shaded, int from, int n)
{
	GList	*items, *item;

	items = gtk_container_get_children(GTK_CONTAINER(menu));

	item = g_list_nth(items, from);
	while (item && n--)
	{
		gtk_widget_set_sensitive(GTK_BIN(item->data)->child, !shaded);
		item = item->next;
	}
	g_list_free(items);
}

static void save_menus(void)
{
	char *menurc = choices_find_xdg_path_save(MENUS_NAME, PROJECT, SITE, TRUE);
	if (menurc)
	{
		gtk_accel_map_save(menurc);
		g_free(menurc);
	}
}

static void select_nth_item(GtkMenuShell *shell, int n)
{
	GList	  *items;
	GtkWidget *item;

	items = gtk_container_get_children(GTK_CONTAINER(shell));
	item = g_list_nth_data(items, n);

	g_return_if_fail(item != NULL);

	g_list_free(items);

	gtk_menu_shell_select_item(shell, item);
}

static void clipboard_get(GtkClipboard *clipboard,
		GtkSelectionData *selection_data, guint info, gpointer user_data)
{
	if (!selected_paths) return;
	bool gnome = info == TARGET_GNOME_COPIED_FILES;
	if (!gnome && info != TARGET_URI_LIST) return;

	GString *data = g_string_new(gnome ? clipboard_action : "");
	const char *n = gnome ? "\r\n" : "\n";

	for (GList *next = selected_paths; next; next = next->next)
	{
		EscapedPath *uri = encode_path_as_uri(next->data);
		g_string_append_printf(data, "%s%s", (char *) uri, n);
		g_free(uri);
	}

	gtk_selection_data_set(selection_data,
			gnome ? gnome_copied_files : text_uri_list,
			8, data->str, data->len);

	g_string_free(data, TRUE);
}

static void clipboard_clear(GtkClipboard *clipboard, gpointer user_data)
{
	if (!selected_paths)
		return;

	destroy_glist(&selected_paths);
}

static void paste_from_clipboard(gpointer data, guint action, GtkWidget *unused)
{
	const gchar *error = NULL;
	gboolean ignore_no_local_paths = FALSE;

	GtkSelectionData *selection =
				gtk_clipboard_wait_for_contents(clipboard, gnome_copied_files) ?:
				gtk_clipboard_wait_for_contents(clipboard, text_uri_list);
	if (!selection)
	{
		delayed_error(_("The clipboard is empty."));
		return;
	}

	char **uri_list = gtk_selection_data_get_uris(selection);
	if (!uri_list)
	{
		char *tmp = g_strndup(selection->data, selection->length);
		uri_list = g_strsplit_set(tmp, "\r\n", -1);
		g_free(tmp);
	}

	/* Either one local URI, or a list. If everything in the list
	* isn't local then we are stuck.
	*/

	GQueue gq = G_QUEUE_INIT;
	for (gchar **uri_iter = uri_list + 1; *uri_iter; uri_iter++)
	{
		if (**uri_iter == '\0') continue;

		char *path = get_local_path((EscapedPath *) *uri_iter);
		if (path)
		{
			gchar *source_real_path = pathdup(path);
			gchar *source_dirname = g_path_get_dirname(source_real_path);
			if (strcmp(source_dirname, window_with_focus->real_path) == 0 &&
				strcmp(*uri_list, "cut") != 0)
			{
				gchar *source_basename = g_path_get_basename(path);
				gchar *new_name = NULL;
				GList *one_path = NULL;

				ignore_no_local_paths = TRUE;
				one_path = g_list_append(one_path, path);

				new_name = g_strdup_printf(_("Copy of %s"), source_basename);
				int i = 2;
				struct stat dest_info;
				while (mc_lstat(make_path(source_dirname, new_name), &dest_info) == 0)
				{
					g_free(new_name);
					new_name = g_strdup_printf(_("Copy(%d) of %s"), i, source_basename);
					i++;
				}
				action_copy(one_path, window_with_focus->sym_path, new_name, -1);

				g_free(new_name);
				g_free(source_basename);
				destroy_glist(&one_path);
			}
			else
				g_queue_push_tail(&gq, path);

			g_free(source_real_path);
			g_free(source_dirname);
		}
		else
			error = _("Some of these files are on a "
					"different machine - they will be "
					"ignored - sorry");
	}

	if (!gq.head)
	{
		if (ignore_no_local_paths == FALSE)
			error = _("None of these files are on the local "
				"machine - I can't operate on multiple "
				"remote files - sorry.");
	}
	else
	{
		if (!strcmp(*uri_list, "cut"))
		{
			action_move(gq.head, window_with_focus->sym_path, NULL, -1);
			gtk_clipboard_clear(clipboard);
		}
		else
			action_copy(gq.head, window_with_focus->sym_path, NULL, -1);

		destroy_glist(&gq.head);
	}

	if (error)
		delayed_error(_("Error getting file list: %s"), error);

	g_strfreev(uri_list);
	gtk_selection_data_free(selection);
}

static void file_op(gpointer data, FileOp action, GtkWidget *unused)
{
	DirItem	*item;
	const guchar *path;
	int	n_selected;
	ViewIter iter;

	g_return_if_fail(window_with_focus != NULL);

	n_selected = view_count_selected(window_with_focus->view);

	if (n_selected < 1)
	{
		const char *prompt;

		switch (action)
		{
			case FILE_DUPLICATE_ITEM:
				prompt = _("Duplicate ... ?");
				break;
			case FILE_RENAME_ITEM:
				prompt = _("Rename ... ?");
				break;
			case FILE_LINK_ITEM:
				prompt = _("Symlink ... ?");
				break;
			case FILE_OPEN_FILE:
				prompt = _("Shift Open ... ?");
				break;
			case FILE_PROPERTIES:
				prompt = _("Properties of ... ?");
				break;
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
			case FILE_XATTRS:
				prompt = _("Extended attributes of ... ?");
				break;
#endif
			case FILE_SET_TYPE:
				prompt = _("Set type of ... ?");
				break;
			case FILE_RUN_ACTION:
				prompt = _("Set run action for ... ?");
				break;
			case FILE_SET_ICON:
				prompt = _("Set icon for ... ?");
				break;
			case FILE_SEND_TO:
				prompt = _("Send ... to ... ?");
				break;
			case FILE_DELETE:
				prompt = _("DELETE ... ?");
				break;
			case FILE_USAGE:
				prompt = _("Count the size of ... ?");
				break;
			case FILE_CHMOD_ITEMS:
				prompt = _("Set permissions on ... ?");
				break;
			case FILE_FIND:
				prompt = _("Search inside ... ?");
				break;
			case FILE_COPY_TO_CLIPBOARD:
				prompt = _("Copy ... to clipboard ?");
				break;
			case FILE_CUT_TO_CLIPBOARD:
				prompt = _("Cut ... to clipboard ?");
				break;
			default:
				g_warning("Unknown action!");
				return;
		}
		filer_target_mode(window_with_focus, target_callback,
					GINT_TO_POINTER(action), prompt);
		return;
	}

	switch (action)
	{
		case FILE_SEND_TO:
			send_to(window_with_focus);
			return;
		case FILE_DELETE:
			delete(window_with_focus);
			return;
		case FILE_USAGE:
			usage(window_with_focus);
			return;
		case FILE_CHMOD_ITEMS:
			chmod_items(window_with_focus);
			return;
		case FILE_SET_TYPE:
			set_type_items(window_with_focus);
			return;
		case FILE_FIND:
			find(window_with_focus);
			return;
		case FILE_PROPERTIES:
		{
			GList *items;

			items = filer_selected_items(window_with_focus);
			infobox_show_list(items);
			destroy_glist(&items);
			return;
		}
		case FILE_RENAME_ITEM:
			if (n_selected > 1)
			{
				GList *items = NULL;
				ViewIter iter;

				view_get_iter(window_with_focus->view, &iter, VIEW_ITER_SELECTED);
				while ((item = iter.next(&iter)))
					items = g_list_prepend(items, item->leafname);
				items = g_list_reverse(items);

				bulk_rename(window_with_focus->sym_path, items);
				g_list_free(items);
				return;
			}
			break;	/* Not a bulk rename... see below */

		case FILE_COPY_TO_CLIPBOARD:
		case FILE_CUT_TO_CLIPBOARD:
			gtk_clipboard_clear(clipboard);

			if (action == FILE_COPY_TO_CLIPBOARD)
				clipboard_action = "copy\n";
			else
				clipboard_action = "cut\n";

			selected_paths = filer_selected_items(window_with_focus);

			gtk_clipboard_set_with_data(clipboard, clipboard_targets, 2,
				clipboard_get, clipboard_clear, NULL);
			return;
		default:
			break;
	}

	/* All the following actions require exactly one file selected */

	if (n_selected > 1)
	{
		report_error(_("You cannot do this to more than "
				"one item at a time"));
		return;
	}

	view_get_iter(window_with_focus->view, &iter, VIEW_ITER_SELECTED);

	item = iter.next(&iter);
	g_return_if_fail(item != NULL);
	/* iter may be passed to filer_openitem... */

	if (item->base_type == TYPE_UNKNOWN)
		item = dir_update_item(window_with_focus->directory,
					item->leafname);

	if (!item)
	{
		report_error(_("Item no longer exists!"));
		return;
	}

	path = make_path(window_with_focus->sym_path, item->leafname);

	switch (action)
	{
		case FILE_DUPLICATE_ITEM:
			src_dest_action_item(path, di_image(item),
					_("Duplicate"), copy_cb,
					GDK_ACTION_COPY);
			break;
		case FILE_RENAME_ITEM:
			src_dest_action_item(path, di_image(item),
					_("Rename"), rename_cb,
					GDK_ACTION_MOVE);
			break;
		case FILE_LINK_ITEM:
			src_dest_action_item(path, di_image(item),
					_("Symlink"), link_cb,
					GDK_ACTION_LINK);
			break;
		case FILE_OPEN_FILE:
			filer_openitem(window_with_focus, &iter,
				OPEN_SAME_WINDOW | OPEN_SHIFT);
			break;
		case FILE_RUN_ACTION:
			run_action(item);
			break;
		case FILE_SET_ICON:
			icon_set_handler_dialog(item, path);
			break;
#if defined(HAVE_GETXATTR) || defined(HAVE_ATTROPEN)
		case FILE_XATTRS:
			if(access(path, R_OK) == 0)
				xattrs_browser(item, path);
			break;
#endif
		default:
			g_warning("Unknown action!");
			return;
	}
}

static void show_key_help(GtkWidget *button, gpointer data)
{
	gboolean can_change_accels;

	g_object_get(G_OBJECT(gtk_settings_get_default()),
		     "gtk-can-change-accels", &can_change_accels,
		     NULL);

	if (!can_change_accels)
	{
		info_message(_("User-definable shortcuts are disabled by "
			"default in Gtk2, and you have not enabled "
			"them. You can turn this feature on by:\n\n"
			"1) using an XSettings manager, such as ROX-Session "
			"or gnome-settings-daemon, or\n\n"
			"2) adding this line to ~/.gtkrc-2.0:\n"
			"\tgtk-can-change-accels = 1\n"
			"\t(this only works if NOT using XSETTINGS)"));
		return;
	}

	info_message(_("To set a keyboard short-cut for a menu item:\n\n"
	"- Open the menu over a filer window,\n"
	"- Move the pointer over the item you want to use,\n"
	"- Press the key you want attached to it.\n\n"
	"The key will appear next to the menu item and you can just press "
	"that key without opening the menu in future."));
}

static GList *set_keys_button(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *button, *align;

	g_return_val_if_fail(option == NULL, NULL);

	align = gtk_alignment_new(0, 0.5, 0, 0);
	button = gtk_button_new_with_label(_("Set keyboard shortcuts"));
	gtk_container_add(GTK_CONTAINER(align), button);
	g_signal_connect(button, "clicked", G_CALLBACK(show_key_help), NULL);

	return g_list_append(NULL, align);
}
