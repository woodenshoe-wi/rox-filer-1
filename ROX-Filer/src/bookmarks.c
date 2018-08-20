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

/* bookmarks.c - handles the bookmarks menu */

#include "config.h"

#include <stdbool.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <string.h>
#include <sys/param.h>

#include "global.h"

#include "bookmarks.h"
#include "choices.h"
#include "filer.h"
#include "xml.h"
#include "support.h"
#include "gui_support.h"
#include "main.h"
#include "mount.h"
#include "action.h"
#include "options.h"
#include "bind.h"
#include "menu.h"
#include "diritem.h"
#include "pixmaps.h"

static GList *history = NULL;		/* Most recent first */
static GList *history_tail = NULL;	/* Oldest item */
static GHashTable *history_hash = NULL;	/* Path -> GList link */
static gint history_free = 30;		/* Space left in history */

static XMLwrapper *bookmarks = NULL;
static GtkWidget *bookmarks_window = NULL;

/* Static prototypes */
static void update_bookmarks(void);
static xmlNode *bookmark_find(const gchar *mark);
static void bookmarks_save(void);
static void bookmarks_add(GtkMenuItem *menuitem, gpointer user_data);
static void bookmarks_activate(GtkMenuShell *item, FilerWindow *filer_window);
static GtkWidget *bookmarks_build_menu(FilerWindow *filer_window);
static void bposition_menu(GtkMenu *menu, gint *x, gint *y,
		   	  gboolean *push_in, gpointer data);
static void position_menu_widget(GtkMenu *menu, gint *x, gint *y,
		   	  gboolean *push_in, gpointer data);
static void cell_edited(GtkCellRendererText *cell,
	     const gchar *path_string,
	     const gchar *new_text,
	     gpointer data);
static void reorder_up(GtkButton *button, GtkTreeView *view);
static void reorder_down(GtkButton *button, GtkTreeView *view);
static void edit_response(GtkWidget *window, gint response,
			  GtkTreeModel *model);
static void edit_delete(GtkButton *button, GtkTreeView *view);
static gboolean dir_dropped(GtkWidget *window, GdkDragContext *context,
			    int x, int y,
			    GtkSelectionData *selection_data, guint info,
			    guint time, GtkTreeView *view);
static void bookmarks_add_dir(const guchar *dir);
static void commit_edits(GtkTreeModel *model);



//menu icons
typedef struct {
	GtkWidget *fix;
	GtkWidget *lbl;
	DirItem *ditem;
	char *path;
} BItem;
static MenuIconStyle style;
static int iconw;
static GList *items;
static GList *itemshist; //temp
static guint iconloop;
static GThread *icont;
static bool iconfinish;
static GMutex itemm;
static void resetitems()
{
	if (iconloop)
	{
		iconfinish = true;
		if (icont)
			g_thread_join(icont);
		icont = NULL;
		g_source_remove(iconloop);
		iconloop = 0;
	}

	style = get_menu_icon_style();
	switch (style) {
	case MIS_LARGE:
		iconw = ICON_WIDTH * 1.2; break;
	case MIS_SMALL:
		iconw = small_width + 1; break;
	default:
		iconw = small_width / 6;
	}

	if (items)
	{
		for (GList *next = items; next; next = next->next)
		{
			BItem *bi = next->data;
			g_free(bi->path); //free the path
			if (bi->ditem)
				diritem_free(bi->ditem);
		}
		g_list_free_full(items, g_free);
		items = NULL;
	}
}
static gpointer icon_thread(gpointer data)
{
	for (GList *next = items; next; next = next->next)
	{
		if (iconfinish) break;
		BItem *bi = next->data;

		DirItem *ditem = diritem_new("");
		diritem_restat(bi->path, ditem, NULL, TRUE);

		g_mutex_lock(&itemm);
		bi->ditem = ditem;
		g_mutex_unlock(&itemm);
	}

	iconfinish = true;
	return NULL;
}
static gboolean iconloopcb(gpointer p)
{
	g_mutex_lock(&itemm);
	for (GList *next = items; next; next = next->next)
	{
		BItem *bi = next->data;
		if (!bi->ditem) continue;

		GtkWidget *img = menu_make_image(bi->ditem, style);

		gtk_widget_show(img);
		gtk_fixed_put((void *)bi->fix, img, -1, -1);

		diritem_free(bi->ditem);
		bi->ditem = NULL;
	}
	bool finish = iconfinish;
	g_mutex_unlock(&itemm);

	if (!finish) return TRUE;

	resetitems();
	return FALSE;
}
static void makeicons()
{
	iconfinish = false;
	icont = g_thread_new("b_icon_t", icon_thread, NULL);
	iconloop = g_idle_add(iconloopcb, NULL);
}

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

/* Shows the bookmarks menu */
void bookmarks_show_menu(FilerWindow *filer_window, GtkWidget *widget)
{
	GdkEvent *event;
	static GtkMenu *menu = NULL;
	int	button = 0;

	event = gtk_get_current_event();
	if (event)
	{
		if (event->type == GDK_BUTTON_RELEASE ||
		    event->type == GDK_BUTTON_PRESS)
			button = ((GdkEventButton *) event)->button;
		gdk_event_free(event);
	}

	resetitems();
	if (menu)
		gtk_widget_destroy((GtkWidget *) menu);

	menu = GTK_MENU(bookmarks_build_menu(filer_window));

	if (widget)
		gtk_menu_popup(menu, NULL, NULL, position_menu_widget, widget,
			button, gtk_get_current_event_time());
	else
		gtk_menu_popup(menu, NULL, NULL, bposition_menu, filer_window,
			button, gtk_get_current_event_time());
}

/* Show the Edit Bookmarks dialog */
void bookmarks_edit(void)
{
	GtkListStore *model;
	GtkWidget *list, *hbox, *button, *swin;
	GtkTreeSelection *selection;
	GtkCellRenderer *cell;
	xmlNode *node;
	GtkTreeIter iter;

	if (bookmarks_window)
	{
		gtk_window_present(GTK_WINDOW(bookmarks_window));
		return;
	}

	update_bookmarks();

	bookmarks_window = gtk_dialog_new();
	number_of_windows++;

	gtk_window_set_position(GTK_WINDOW(bookmarks_window),
			GTK_WIN_POS_MOUSE);


	gtk_dialog_add_button(GTK_DIALOG(bookmarks_window),
			GTK_STOCK_CLOSE, GTK_RESPONSE_OK);

	g_signal_connect(bookmarks_window, "destroy",
			 G_CALLBACK(gtk_widget_destroyed), &bookmarks_window);
	g_signal_connect(bookmarks_window, "destroy",
			 G_CALLBACK(one_less_window), NULL);

	swin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(swin),
			GTK_SHADOW_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(swin),
			GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(bookmarks_window)->vbox),
			swin, TRUE, TRUE, 0);

	model = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);

	list = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));

	cell = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(cell), "edited",
		    G_CALLBACK(cell_edited), model);
	g_object_set(G_OBJECT(cell), "editable", TRUE, NULL);
	g_object_set_data(G_OBJECT(cell), "column", GINT_TO_POINTER(1));
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(list), -1,
		_("Title"), cell, "text", 1, NULL);

	cell = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(cell), "edited",
		    G_CALLBACK(cell_edited), model);
	g_object_set(G_OBJECT(cell), "editable", TRUE, NULL);
	g_object_set_data(G_OBJECT(cell), "column", GINT_TO_POINTER(0));
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(list), -1,
		_("Path"), cell, "text", 0, NULL);

	gtk_tree_view_column_set_resizable(
			gtk_tree_view_get_column(GTK_TREE_VIEW(list), 0),
			TRUE);

	gtk_tree_view_set_reorderable(GTK_TREE_VIEW(list), TRUE);
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(list), TRUE);

	node = xmlDocGetRootElement(bookmarks->doc);
	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		GtkTreeIter iter;
		gchar *mark, *title;

		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "bookmark") != 0)
			continue;

		mark = xmlNodeListGetString(bookmarks->doc,
					    node->xmlChildrenNode, 1);
		if (!mark)
			continue;

		title=xmlGetProp(node, "title");
		if(!title)
			title=mark;

		gtk_list_store_append(model, &iter);
		gtk_list_store_set(model, &iter, 0, mark, 1, title, -1);
		if(title!=mark)
			xmlFree(title);

		xmlFree(mark);
	}

	gtk_widget_set_size_request(list, 400, 300);
	gtk_container_add(GTK_CONTAINER(swin), list);

	hbox = gtk_hbutton_box_new();
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(bookmarks_window)->vbox),
			hbox, FALSE, TRUE, 0);
	gtk_container_set_border_width(GTK_CONTAINER(hbox), 5);

	button = gtk_button_new_from_stock(GTK_STOCK_DELETE);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);
	g_signal_connect(button, "clicked", G_CALLBACK(edit_delete), list);

	button = gtk_button_new_from_stock(GTK_STOCK_GO_UP);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);
	g_signal_connect(button, "clicked", G_CALLBACK(reorder_up), list);
	button = gtk_button_new_from_stock(GTK_STOCK_GO_DOWN);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, TRUE, 0);
	g_signal_connect(button, "clicked", G_CALLBACK(reorder_down), list);

	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(list));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	/* Select the first item, otherwise the first click starts edit
	 * mode, which is very confusing!
	 */
	if (gtk_tree_model_get_iter_first(GTK_TREE_MODEL(model), &iter))
		gtk_tree_selection_select_iter(selection, &iter);

	g_signal_connect(bookmarks_window, "response",
			 G_CALLBACK(edit_response), model);

	/* Allow directories to be dropped in */
	{
		GtkTargetEntry targets[] = { {"text/uri-list", 0, 0} };
		gtk_drag_dest_set(bookmarks_window, GTK_DEST_DEFAULT_ALL,
				targets, G_N_ELEMENTS(targets),
				GDK_ACTION_COPY |GDK_ACTION_PRIVATE);
		g_signal_connect(bookmarks_window, "drag-data-received",
				G_CALLBACK(dir_dropped), list);
	}

	g_signal_connect_swapped(model, "row-changed",
			 G_CALLBACK(commit_edits), model);
	g_signal_connect_swapped(model, "row-inserted",
			 G_CALLBACK(commit_edits), model);
	g_signal_connect_swapped(model, "row-deleted",
			 G_CALLBACK(commit_edits), model);
	g_signal_connect_swapped(model, "rows-reordered",
			 G_CALLBACK(commit_edits), model);

	gtk_widget_show_all(bookmarks_window);
}

static void history_remove(const char *path)
{
	GList *old;

	old = g_hash_table_lookup(history_hash, path);
	if (old)
	{
		g_hash_table_remove(history_hash, path);

		if (history_tail == old)
			history_tail = old->prev;
		g_free(old->data);
		history = g_list_delete_link(history, old);

		history_free++;
	}
}

/* Add this path to the global history of visited directories. If it
 * already exists there, make it the most recent. If its parent exists
 * already, remove the parent.
 */
void bookmarks_add_history(const gchar *path)
{
	char *new;

	new = g_strdup(path);
	ensure_utf8(&new);

	if (!history_hash)
		history_hash = g_hash_table_new(g_str_hash, g_str_equal);

	history_remove(new);

	{
		char *parent;
		parent = g_dirname(path);
		history_remove(parent);
		g_free(parent);
	}

	history = g_list_prepend(history, new);
	if (!history_tail)
		history_tail = history;
	g_hash_table_insert(history_hash, new, history);

	history_free--;
	if (history_free == -1)
	{
		g_return_if_fail(history_tail != NULL);
		history_remove((char *) history_tail->data);
	}
}

gchar *bookmarks_get_recent(void)
{
	if (history->next)
		return history->next->data;
	else
		return NULL;
}

gchar *bookmarks_get_top(void)
{
	static gchar *path;
	gchar *mark;
	xmlNode *node;

	g_free(path);

	update_bookmarks();
	node = xmlDocGetRootElement(bookmarks->doc);
	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "bookmark") != 0)
			continue;

		mark = xmlNodeListGetString(bookmarks->doc,
							node->xmlChildrenNode, 1);
		if (mark)
		{
			path = expand_path(mark);
			xmlFree(mark);
			return path;
		}
	}
		return NULL;
}

void bookmarks_add_uri(const EscapedPath *uri)
{
	char *path;
	struct stat info;

	path = get_local_path(uri);

	if (!path)
	{
		delayed_error(_("Can't bookmark non-local resource '%s'\n"),
					uri);
		return;
	}

	if (mc_stat(path, &info) == 0 && S_ISDIR(info.st_mode))
		bookmarks_add_dir(path);
	else
		delayed_error(_("'%s' isn't a directory"), path);
	g_free(path);
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Initialise the bookmarks document to be empty. Does not save. */
static void bookmarks_new(void)
{
	if (bookmarks)
		g_object_unref(G_OBJECT(bookmarks));
	bookmarks = xml_new(NULL);
	bookmarks->doc = xmlNewDoc("1.0");
	xmlDocSetRootElement(bookmarks->doc,
		xmlNewDocNode(bookmarks->doc, NULL, "bookmarks", NULL));
}

static void bposition_menu(GtkMenu *menu, gint *x, gint *y,
		   	  gboolean *push_in, gpointer data)
{
	FilerWindow *filer_window = (FilerWindow *) data;

	gdk_window_get_origin(GTK_WIDGET(filer_window->view)->window, x, y);
}

static void position_menu_widget(GtkMenu *menu, gint *x, gint *y,
		   	  gboolean *push_in, gpointer data)
{
	GtkWidget *widget = (GtkWidget *) data;

	gdk_window_get_origin(gtk_widget_get_parent_window(widget), x, y);
	*x = *x + widget->allocation.x;

	widget = gtk_widget_get_parent(widget);
	*y = *y + widget->allocation.y + widget->allocation.height;
}

/* Makes sure that 'bookmarks' is up-to-date, reloading from file if it has
 * changed. If no bookmarks were loaded and there is no file then initialise
 * bookmarks to an empty document.
 */
static void update_bookmarks()
{
	gchar *path;

	/* Update the bookmarks, if possible */
	path = choices_find_xdg_path_load("Bookmarks.xml", PROJECT, SITE);
	if (path)
	{
		XMLwrapper *wrapper;
		wrapper = xml_cache_load(path);
		if (wrapper)
		{
			if (bookmarks)
				g_object_unref(bookmarks);
			bookmarks = wrapper;
		}

		g_free(path);
	}

	if (!bookmarks)
		bookmarks_new();
}

/* Return the node for the 'mark' bookmark */
static xmlNode *bookmark_find(const gchar *mark)
{
	xmlNode *node;

	update_bookmarks();

	node = xmlDocGetRootElement(bookmarks->doc);

	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		gchar *path;
		gboolean same;

		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "bookmark") != 0)
			continue;

		path = xmlNodeListGetString(bookmarks->doc,
					node->xmlChildrenNode, 1);
		if (!path)
			continue;

		same = strcmp(mark, path) == 0;
		xmlFree(path);

		if (same)
			return node;
	}

	return NULL;
}

/* Save the bookmarks to a file */
static void bookmarks_save()
{
	guchar	*save_path;

	save_path = choices_find_xdg_path_save("Bookmarks.xml", PROJECT, SITE,
					       TRUE);
	if (save_path)
	{
		save_xml_file(bookmarks->doc, save_path);
		g_free(save_path);
	}
}

/* Add a bookmark if it doesn't already exist, and save the
 * bookmarks.
 */
static void bookmarks_add(GtkMenuItem *menuitem, gpointer user_data)
{
	FilerWindow *filer_window = (FilerWindow *) user_data;

	bookmarks_add_dir(filer_window->sym_path);
}

static void bookmarks_add_dir(const guchar *dir)
{
	xmlNode	*bookmark, *node;
	gchar *basename, *path, *title, *alist = g_new0(gchar, 27);

	path = collapse_path(dir);
	if (bookmark_find(path)){
		g_free(path);
		return;
	}

	basename = g_path_get_basename(path);
	node = xmlDocGetRootElement(bookmarks->doc);
	bookmark = xmlNewTextChild(node, NULL, "bookmark", path);

	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		title = xmlGetProp(node, "title");
		if (title){
			get_mnemonic(title, alist);
			xmlFree(title);
		}
	}

	xmlSetProp(bookmark, "title", add_mnemonic(basename, alist));

	bookmarks_save();

	if (bookmarks_window)
		gtk_widget_destroy(bookmarks_window);

	g_free(basename);
	g_free(path);
	g_free(alist);
}

/* Called when a bookmark has been chosen */
static void bookmarks_activate(GtkMenuShell *item, FilerWindow *filer_window)
{
	const gchar *mark;
	GtkLabel *label;
	GdkEvent *event;
	gboolean new_win=FALSE;

	mark=g_object_get_data(G_OBJECT(item), "bookmark-path");
	if(!mark) {
		label = GTK_LABEL(GTK_BIN(item)->child);
		mark = gtk_label_get_text(label);
	}

	event=gtk_get_current_event();
	if(event)
	{
		if(event->type==GDK_BUTTON_PRESS ||
		   event->type==GDK_BUTTON_RELEASE)
		{
			GdkEventButton *button=(GdkEventButton *) event;

			new_win=o_new_button_1.int_value?
				button->button==1: button->button!=1;
		}
		gdk_event_free(event);
	}

	if (strcmp(mark, filer_window->sym_path) != 0)
	{
		if(new_win)
			filer_opendir(mark, filer_window, NULL, FALSE);
		else
			filer_change_to(filer_window, mark, NULL);
	}
	if (g_hash_table_lookup(fstab_mounts, filer_window->real_path) &&
		!mount_is_mounted(filer_window->real_path, NULL, NULL))
	{
		GList	*paths;

		paths = g_list_prepend(NULL, filer_window->real_path);
		action_mount(paths, FALSE, TRUE, -1);
		g_list_free(paths);
	}
}

static void edit_delete(GtkButton *button, GtkTreeView *view)
{
	GtkTreeModel *model;
	GtkListStore *list;
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	gboolean more, any = FALSE;

	model = gtk_tree_view_get_model(view);
	list = GTK_LIST_STORE(model);

	selection = gtk_tree_view_get_selection(view);

	more = gtk_tree_model_get_iter_first(model, &iter);

	while (more)
	{
		GtkTreeIter old = iter;

		more = gtk_tree_model_iter_next(model, &iter);

		if (gtk_tree_selection_iter_is_selected(selection, &old))
		{
			any = TRUE;
			gtk_list_store_remove(list, &old);
		}
	}

	if (!any)
	{
		report_error(_("You should first select some rows to delete"));
		return;
	}
}

static void reorder(GtkTreeView *view, int dir)
{
	GtkTreeModel *model;
	GtkListStore *list;
	GtkTreePath *cursor = NULL;
	GtkTreeIter iter, old, new;
	GValue mark = {0};
	GValue title = {0};
	gboolean    ok;

	g_return_if_fail(view != NULL);
	g_return_if_fail(dir == 1 || dir == -1);

	model = gtk_tree_view_get_model(view);
	list = GTK_LIST_STORE(model);

	gtk_tree_view_get_cursor(view, &cursor, NULL);
	if (!cursor)
	{
		report_error(_("Put the cursor on an entry in the "
			       "list to move it"));
		return;
	}

	gtk_tree_model_get_iter(model, &old, cursor);
	if (dir > 0)
	{
		gtk_tree_path_next(cursor);
		ok = gtk_tree_model_get_iter(model, &iter, cursor);
	}
	else
	{
		ok = gtk_tree_path_prev(cursor);
		if (ok)
			gtk_tree_model_get_iter(model, &iter, cursor);
	}
	if (!ok)
	{
		gtk_tree_path_free(cursor);
		report_error(_("This item is already at the end"));
		return;
	}

	gtk_tree_model_get_value(model, &old, 0, &mark);
	gtk_tree_model_get_value(model, &old, 1, &title);
	if (dir > 0)
		gtk_list_store_insert_after(list, &new, &iter);
	else
		gtk_list_store_insert_before(list, &new, &iter);
	gtk_list_store_set(list, &new, 0, g_value_get_string(&mark), -1);
	gtk_list_store_set(list, &new, 1, g_value_get_string(&title), -1);
	gtk_list_store_remove(list, &old);

	g_value_unset(&mark);
	g_value_unset(&title);

	gtk_tree_view_set_cursor(view, cursor, 0, FALSE);
	gtk_tree_path_free(cursor);
}

static void reorder_up(GtkButton *button, GtkTreeView *view)
{
	reorder(view, -1);
}

static void reorder_down(GtkButton *button, GtkTreeView *view)
{
	reorder(view, 1);
}

static gboolean dir_dropped(GtkWidget *window, GdkDragContext *context,
			    int x, int y,
			    GtkSelectionData *selection_data, guint info,
			    guint time, GtkTreeView *view)
{
	GtkListStore *model;
	GList *uris, *next;

	if (!selection_data->data)
	{
		/* Timeout? */
		gtk_drag_finish(context, FALSE, FALSE, time);	/* Failure */
		return TRUE;
	}

	model = GTK_LIST_STORE(gtk_tree_view_get_model(view));

	uris = uri_list_to_glist(selection_data->data);

	for (next = uris; next; next = next->next)
	{
		guchar *path;

		path = get_local_path((EscapedPath *) next->data);

		if (path)
		{
			GtkTreeIter iter;
			struct stat info;

			if (mc_stat(path, &info) == 0 && S_ISDIR(info.st_mode))
			{
				gtk_list_store_append(model, &iter);
				gtk_list_store_set(model, &iter, 0, path,
						   1, path, -1);
			}
			else
				delayed_error(_("'%s' isn't a directory"),
						path);

			g_free(path);
		}
		else
			delayed_error(_("Can't bookmark non-local directories "
					"like '%s'"), (gchar *) next->data);
	}

	destroy_glist(&uris);

	return TRUE;
}

static void commit_edits(GtkTreeModel *model)
{
	GtkTreeIter iter;

	bookmarks_new();

	if (gtk_tree_model_get_iter_first(model, &iter))
	{
		GValue mark = {0}, title={0};
		xmlNode *root = xmlDocGetRootElement(bookmarks->doc);

		do
		{
			xmlNode *bookmark;

			gtk_tree_model_get_value(model, &iter, 0, &mark);
			bookmark = xmlNewTextChild(root, NULL, "bookmark",
					g_value_get_string(&mark));
			g_value_unset(&mark);
			gtk_tree_model_get_value(model, &iter, 1, &title);
			xmlSetProp(bookmark, "title",
				   g_value_get_string(&title));
			g_value_unset(&title);
		} while (gtk_tree_model_iter_next(model, &iter));
	}

	bookmarks_save();
}

static void edit_response(GtkWidget *window, gint response, GtkTreeModel *model)
{
	commit_edits(model);

	gtk_widget_destroy(window);
}

static void cell_edited(GtkCellRendererText *cell,
	     const gchar *path_string,
	     const gchar *new_text,
	     gpointer data)
{
	GtkTreeModel *model = (GtkTreeModel *) data;
	GtkTreePath *path;
	GtkTreeIter iter;
	gint col;

	path = gtk_tree_path_new_from_string(path_string);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_path_free(path);
	col=GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell), "column"));

	gtk_list_store_set(GTK_LIST_STORE(model), &iter, col, new_text, -1);
}

static void activate_edit(GtkMenuShell *item, gpointer data)
{
	bookmarks_edit();
}

static void free_path_for_item(GtkWidget *widget, gpointer udata)
{
	gchar *path=(gchar *) udata;
	g_free(path);
}


static GtkWidget *build_history_menu(FilerWindow *filer_window)
{
	GtkWidget *menu;
	GList	  *next;

	menu = gtk_menu_new();

	if (!history)
		return menu;

	g_return_val_if_fail(history_hash != NULL, menu);
	g_return_val_if_fail(history_tail != NULL, menu);

	int count = 0;
	for (next = history; next; next = next->next)
	{
		GtkWidget *item;
		char *path = (char *) next->data;
		gchar *copy;

		if (!(next->prev) && strcmp(path, filer_window->sym_path) == 0)
			continue;

		GtkWidget *label, *fix;
		PangoLayout *layout;
		int i = 0, width, height;
		char *pathp, *bpath, *nn = next->next ? (char *) next->next->data : "";
		pathp = path;

		while ((nn+=1) && (pathp+=1) && (++i))
			if (*nn != *pathp)
				break;

		bpath = g_strndup(path, i);
		item = gtk_menu_item_new();

		fix = gtk_fixed_new();
		BItem *bi = g_new0(BItem, 1);
		bi->fix = fix;
		bi->path = g_strdup(path);
		itemshist = g_list_append(itemshist, bi);

		label =  gtk_label_new(bpath);

		gtk_widget_modify_fg(label,
				GTK_STATE_NORMAL,
				&label->style->text[GTK_STATE_INSENSITIVE]);

		layout = gtk_label_get_layout(GTK_LABEL(label));
		pango_layout_get_pixel_size(layout, &width, &height);

		gtk_fixed_put(GTK_FIXED(fix), label, iconw, 0);
		label =  gtk_label_new(pathp);
		gtk_fixed_put(GTK_FIXED(fix), label, width + iconw, 0);

		gtk_container_add(GTK_CONTAINER(item), fix);

		g_free(bpath);

		copy=g_strdup(path);
		g_object_set_data(G_OBJECT(item), "bookmark-path", copy);
		g_signal_connect(item, "destroy",
				 G_CALLBACK(free_path_for_item), copy);

		if (strcmp(path, filer_window->sym_path) == 0)
			gtk_widget_set_sensitive(item, FALSE);
		else
			g_signal_connect(item, "activate",
					G_CALLBACK(bookmarks_activate),
					filer_window);

		gtk_widget_show_all(item);

		//attach with 0,2 removes margin
		gtk_menu_attach(GTK_MENU(menu), item, 0, 2, count, count + 1);
		count++;
	}

	return menu;
}

/* Builds the bookmarks' menu. Done whenever the bookmarks icon has been
 * clicked.
 */
static GtkWidget *bookmarks_build_menu(FilerWindow *filer_window)
{
	GtkWidget *menu;
	GtkWidget *item;
	xmlNode *node;
	gboolean need_separator = TRUE;
	int maxwidth = 0, count = 4;

	menu = gtk_menu_new();

	item = gtk_menu_item_new_with_label(_("Add New Bookmark"));
	g_signal_connect(item, "activate",
			 G_CALLBACK(bookmarks_add), filer_window);
	gtk_widget_show(item);
	gtk_menu_attach(GTK_MENU(menu), item, 0, 1, 0, 1);
	gtk_menu_shell_select_item(GTK_MENU_SHELL(menu), item);

	item = gtk_menu_item_new_with_label(_("Edit Bookmarks"));
	g_signal_connect(item, "activate", G_CALLBACK(activate_edit), NULL);
	gtk_widget_show(item);
	gtk_menu_attach(GTK_MENU(menu), item, 0, 1, 1, 2);

	item = gtk_menu_item_new_with_label(_("Recently Visited"));
	gtk_widget_show(item);
	gtk_menu_attach(GTK_MENU(menu), item, 1, 2, 0, 1);
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item),
			build_history_menu(filer_window));

	/* Now add all the bookmarks to the menu */

	update_bookmarks();

	node = xmlDocGetRootElement(bookmarks->doc);

	for (node = node->xmlChildrenNode; node; node = node->next)
	{
		gchar *mark, *title, *path, *dirname;
		GtkWidget *label, *fix;
		PangoLayout *layout;
		int width, height;

		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "bookmark") != 0)
			continue;

		mark = xmlNodeListGetString(bookmarks->doc,
					    node->xmlChildrenNode, 1);
		if (!mark)
			continue;

		path=expand_path(mark);

		title=xmlGetProp(node, "title");
		if(!title)
			title=mark;

		item = gtk_menu_item_new();

		g_object_set_data(G_OBJECT(item), "bookmark-path", path);
		g_signal_connect(item, "destroy",
				 G_CALLBACK(free_path_for_item), path);

		g_signal_connect(item, "activate",
				G_CALLBACK(bookmarks_activate),
				filer_window);

		if (need_separator)
		{
			GtkWidget *sep;
			sep = gtk_separator_menu_item_new();
			gtk_widget_show(sep);
			gtk_menu_shell_append(GTK_MENU_SHELL(menu), sep);
			need_separator = FALSE;
		}

		fix = gtk_fixed_new();
		BItem *bi = g_new0(BItem, 1);
		bi->fix = fix;
		bi->path = g_strdup(path);
		items = g_list_append(items, bi);

		label =  gtk_label_new_with_mnemonic(title);

		layout = gtk_label_get_layout(GTK_LABEL(label));
		pango_layout_get_pixel_size(layout, &width, &height);
		if (width + height/2 + iconw > maxwidth)
			maxwidth = width + height/2 + iconw;

		gtk_fixed_put(GTK_FIXED(fix), label, iconw, 0);

		dirname = g_path_get_dirname(mark);
		label = gtk_label_new(dirname);
		bi->lbl = label;

		gtk_widget_modify_fg (label,
				GTK_STATE_NORMAL,
				&label->style->text[GTK_STATE_INSENSITIVE]);

		gtk_label_set_max_width_chars(GTK_LABEL(label), 30);
		gtk_label_set_ellipsize(GTK_LABEL(label), PANGO_ELLIPSIZE_MIDDLE);

		gtk_fixed_put(GTK_FIXED(fix), label, maxwidth, 0);

		gtk_container_add(GTK_CONTAINER(item), fix);
		gtk_menu_attach(GTK_MENU(menu), item, 0, 2, count, count + 1);

		gtk_widget_show_all(item);

		count++;

		if(title!=mark)
			xmlFree(title);
		xmlFree(mark);

		g_free(dirname);
	}

	for (GList *next = items; next; next = next->next)
		gtk_fixed_move((void *)((BItem *)next->data)->fix,
				((BItem *)next->data)->lbl, maxwidth, 0);

	items = g_list_concat(items, itemshist);
	itemshist = NULL;
	makeicons();

	return menu;
}
