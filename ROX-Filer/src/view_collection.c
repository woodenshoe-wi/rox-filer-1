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

/* view_collection.c - a subclass of Collection, used for displaying files */

#include "config.h"

#include <gtk/gtk.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>

#include "global.h"

#include "collection.h"
#include "view_iface.h"
#include "view_collection.h"
#include "type.h"
#include "pixmaps.h"
#include "dir.h"
#include "diritem.h"
#include "gui_support.h"
#include "support.h"
#include "dnd.h"
#include "bind.h"
#include "options.h"

#include "toolbar.h"	/* for resizing */
#include "filer.h"
#include "display.h"
#include "usericons.h"
#include "fscache.h"

#define MIN_ITEM_WIDTH 64

static gpointer parent_class = NULL;

struct _ViewCollectionClass {
	GtkViewportClass parent;
};

typedef struct _Template Template;

struct _Template {
	GdkRectangle	icon;
	GdkRectangle	leafname;
	GdkRectangle	details;
	GdkRectangle	image;
};

/* Static prototypes */
static void view_collection_finialize(GObject *object);
static void view_collection_class_init(gpointer gclass, gpointer data);
static void view_collection_init(GTypeInstance *object, gpointer gclass);

static void draw_item(GtkWidget *widget,
			int idx,
			GdkRectangle *area,
			gpointer user_data,
			gboolean cursor);
static void fill_template(GdkRectangle *area, CollectionItem *item,
			ViewCollection *view_collection, Template *template);
static void huge_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template, gboolean full);
static void large_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template, gboolean full);
static void small_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template);
static void small_full_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template);
static gboolean test_point(Collection *collection,
				int point_x, int point_y,
				CollectionItem *item,
				int width, int height,
				gpointer user_data);
static void draw_string(cairo_t *cr,
		PangoLayout  *layout,
		GdkRectangle *area,  /* Area available on screen */
		int          width,  /* Width of the full string */
		int          height, /* height of the full string */
		GdkColor     *fg,
		GdkColor     *bg);
static void view_collection_iface_init(gpointer giface, gpointer iface_data);
static gint coll_motion_notify(GtkWidget *widget,
			       GdkEventMotion *event,
			       ViewCollection *view_collection);
static gint coll_button_release(GtkWidget *widget,
			        GdkEventButton *event,
			        ViewCollection *view_collection);
static gint coll_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      ViewCollection *view_collection);
static void size_allocate(GtkWidget *w, GtkAllocation *a, gpointer data);
static void style_set(Collection 	*collection,
		      GtkStyle		*style,
		      ViewCollection	*view_collection);
static void display_free_colitem(Collection *collection,
				 CollectionItem *colitem);
static void lost_selection(Collection  *collection,
			   guint        time,
			   gpointer     user_data);
static void selection_changed(Collection *collection,
			      gint time,
			      gpointer user_data);
static gfloat calc_size(FilerWindow *filer_window, CollectionItem *colitem,
		int *width, int *height, gint number_of_items);
static void make_iter(ViewCollection *view_collection, ViewIter *iter,
		      IterFlags flags);
static void make_item_iter(ViewCollection *vc, ViewIter *iter, int i);

static void view_collection_sort(ViewIface *view);
static void view_collection_style_changed(ViewIface *view, int flags);
static void view_collection_add_items(ViewIface *view, GPtrArray *items);
static void view_collection_update_items(ViewIface *view, GPtrArray *items);
static void view_collection_delete_if(ViewIface *view,
			  gboolean (*test)(gpointer item, gpointer data),
			  gpointer data);
static void view_collection_clear(ViewIface *view);
static void view_collection_select_all(ViewIface *view);
static void view_collection_clear_selection(ViewIface *view);
static int view_collection_count_items(ViewIface *view);
static int view_collection_count_selected(ViewIface *view);
static void view_collection_show_cursor(ViewIface *view);
static void view_collection_get_iter(ViewIface *view,
				     ViewIter *iter, IterFlags flags);
static void view_collection_get_iter_at_point(ViewIface *view, ViewIter *iter,
					      GdkWindow *src, int x, int y);
static void view_collection_cursor_to_iter(ViewIface *view, ViewIter *iter);
static void view_collection_set_selected(ViewIface *view,
					 ViewIter *iter,
					 gboolean selected);
static gboolean view_collection_get_selected(ViewIface *view, ViewIter *iter);
static void view_collection_select_only(ViewIface *view, ViewIter *iter);
static void view_collection_set_frozen(ViewIface *view, gboolean frozen);
static void view_collection_wink_item(ViewIface *view, ViewIter *iter);
static void view_collection_autosize(ViewIface *view, gboolean turn);
static gboolean view_collection_cursor_visible(ViewIface *view);
static void view_collection_set_base(ViewIface *view, ViewIter *iter);
static void view_collection_start_lasso_box(ViewIface *view,
					     GdkEventButton *event);
static void view_collection_extend_tip(ViewIface *view, ViewIter *iter,
					GString *tip);
static gboolean view_collection_auto_scroll_callback(ViewIface *view);

static DirItem *iter_next(ViewIter *iter);
static DirItem *iter_prev(ViewIter *iter);
static DirItem *iter_peek(ViewIter *iter);
static void reset_thumb_func(ViewCollection *vc);
static void clear_thumb_func(ViewCollection *vc);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

GtkWidget *view_collection_new(FilerWindow *filer_window)
{
	ViewCollection *view_collection;

	view_collection = g_object_new(view_collection_get_type(), NULL);
	view_collection->filer_window = filer_window;

	view_collection->thumb_func = 0;
	reset_thumb_func(view_collection);

	/* Starting with GTK+-2.2.2, the vadjustment is reset after init
	 * (even though it's already set during init) to a new adjustment.
	 * Change it back:
	 */
	gtk_viewport_set_vadjustment(GTK_VIEWPORT(view_collection),
				 view_collection->collection->vadj);

	gtk_range_set_adjustment(GTK_RANGE(filer_window->scrollbar),
				 view_collection->collection->vadj);

	return GTK_WIDGET(view_collection);
}

GType view_collection_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (ViewCollectionClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			view_collection_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(ViewCollection),
			0,			/* n_preallocs */
			view_collection_init
		};
		static const GInterfaceInfo iface_info =
		{
			view_collection_iface_init, NULL, NULL
		};

		type = g_type_register_static(gtk_viewport_get_type(),
						"ViewCollection", &info, 0);
		g_type_add_interface_static(type, VIEW_TYPE_IFACE, &iface_info);
	}

	return type;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void view_collection_destroy(GtkObject *view_collection)
{
	VIEW_COLLECTION(view_collection)->filer_window = NULL;

	clear_thumb_func(VIEW_COLLECTION(view_collection));

	(*GTK_OBJECT_CLASS(parent_class)->destroy)(view_collection);
}

static void view_collection_finialize(GObject *object)
{
	/* ViewCollection *view_collection = (ViewCollection *) object; */

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void view_collection_grab_focus(GtkWidget *focus_widget)
{
	ViewCollection *view_collection = VIEW_COLLECTION(focus_widget);
	gtk_widget_grab_focus(GTK_WIDGET(view_collection->collection));
}

static void view_collection_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;
	GtkWidgetClass *widget = (GtkWidgetClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	widget->grab_focus = view_collection_grab_focus;

	object->finalize = view_collection_finialize;
	GTK_OBJECT_CLASS(object)->destroy = view_collection_destroy;
}

static gboolean set_bg_src(cairo_t *cr, GtkWidget *widget)
{
	static const float p = 65535.0;
	if (o_use_background_colour.int_value)
	{
		GdkColor base;
		gdk_color_parse(o_background_colour.value, &base);
		cairo_set_source_rgba(cr,
				base.red / p,
				base.green / p,
				base.blue / p,
				(100 - o_view_alpha.int_value) / 100.0);

		return TRUE;
	}

	if (o_display_colour_types.int_value)
	{
		cairo_set_source_rgba(cr,
				1.0 - type_colours[TYPE_FILE].red / p,
				1.0 - type_colours[TYPE_FILE].green / p,
				1.0 - type_colours[TYPE_FILE].blue / p,
				(100 - o_view_alpha.int_value) / 100.0);
		return TRUE;
	}

	GdkColor base = widget->style->base[GTK_STATE_NORMAL];
	cairo_set_source_rgba(cr,
			base.red / p,
			base.green / p,
			base.blue / p,
			(100 - o_view_alpha.int_value) / 100.0);

	return FALSE;
}
static gboolean transparent_expose(GtkWidget *widget,
			GdkEventExpose *event,
			ViewCollection *view)
{
	cairo_t *cr = gdk_cairo_create(widget->window);
	GdkColor *fg = view->filer_window->dir_colour;
	GdkRectangle *rects;
	int i, n_rects;
	static const float p = 65535.0;

	gdk_cairo_region(cr, event->region);
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	if (view->filer_window->directory->error)
	{
		cairo_set_source_rgba(cr, 0.9, .0, .0, .6);
		cairo_paint(cr);

		cairo_set_font_size(cr, small_height * 3/4.);
		cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 1.0);
		cairo_select_font_face(cr, "sans-serif", 0, CAIRO_FONT_WEIGHT_BOLD);
		cairo_move_to(cr, 2, small_height * 3/4.);
		cairo_show_text(cr, view->filer_window->directory->error);

		cairo_select_font_face(cr, "sans-serif", 0, 0);
		cairo_move_to(cr, 2, small_height * 2);
		cairo_show_text(cr, _("- Double click here to close the windows"));
	}
	else
	{
		if (set_bg_src(cr, widget))
			cairo_paint(cr);
		else if (o_view_alpha.int_value)
		{
			cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
			cairo_paint_with_alpha(cr, o_view_alpha.int_value / 100.0);
		}

		if (view->collection->number_of_items == 0 &&
				!view->filer_window->first_scan)
		{
			//static const int size = ICON_HEIGHT / 2;
			static const int size = ICON_HEIGHT / 3 + 1;

			cairo_surface_t *cs = cairo_image_surface_create(
					CAIRO_FORMAT_ARGB32, size, size);

			cairo_t *scr = cairo_create(cs);

			cairo_set_operator(scr, CAIRO_OPERATOR_SOURCE);
			cairo_set_source(scr, cairo_get_source(cr));
			if (o_view_alpha.int_value)
				cairo_paint_with_alpha(scr, 0.6);
			else
				cairo_paint(scr);

			cairo_set_source_rgba(scr, 1.0, 1.0, 1.0, 0.4);
			cairo_set_operator(scr, CAIRO_OPERATOR_DIFFERENCE);

			//cairo_rectangle(scr, 0, 0, size / 2, size / 2);
			//cairo_rectangle(scr, size / 2, size / 2, size / 2, size / 2);
			//cairo_fill(scr);
			cairo_set_line_width(scr, 0.4);
			static const double center = size * 2/3;
			static const double edge = size / 6;
			static const double end = size;
			cairo_move_to(scr, center, center);
			cairo_line_to(scr, 0     , edge  );
			cairo_move_to(scr, center, center);
			cairo_line_to(scr, edge  , 0     );
			cairo_move_to(scr, center, center);
			cairo_line_to(scr, end   , end   );
			cairo_stroke_preserve(scr);
			cairo_set_operator(scr, CAIRO_OPERATOR_LIGHTEN);
			cairo_stroke(scr);

			cairo_pattern_t *cp = cairo_pattern_create_for_surface(cs);

			cairo_pattern_set_extend(cp, CAIRO_EXTEND_REFLECT);
			//cairo_pattern_set_extend(cp, CAIRO_EXTEND_REPEAT);

			cairo_set_source(cr, cp);
			cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
			cairo_paint(cr);

			cairo_pattern_destroy(cp);
			cairo_destroy(scr);
			cairo_surface_destroy(cs);
		}
	}

	if (!fg)
		goto end;

	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
	cairo_new_path(cr);
	cairo_set_line_width(cr, 1.0);
	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	cairo_set_source_rgba(cr,
			fg->red / p,
			fg->green / p,
			fg->blue / p,
			//(100 - o_view_alpha.int_value) / 100.0);
		1);

	gdk_region_get_rectangles(event->region, &rects, &n_rects);
	for (i = 0; i < n_rects; i++)
	{
		if (rects[i].x > 4)
			continue;

		cairo_move_to(cr, 4, rects[i].y);
		cairo_line_to(cr, 4, rects[i].y + rects[i].height);
		cairo_move_to(cr, 2, rects[i].y);
		cairo_line_to(cr, 2, rects[i].y + rects[i].height);
		cairo_move_to(cr, 1, rects[i].y);
		cairo_line_to(cr, 1, rects[i].y + rects[i].height);
	}
	cairo_stroke(cr);
	g_free(rects);

end:
	cairo_destroy(cr);
	return FALSE;
}
static void view_collection_init(GTypeInstance *object, gpointer gclass)
{
	ViewCollection *view_collection = (ViewCollection *) object;
	GtkViewport *viewport = (GtkViewport *) object;
	GtkWidget *collection;
	GtkAdjustment *adj;

	collection = collection_new();

	g_signal_connect(collection, "expose-event",
			G_CALLBACK(transparent_expose), object);

	view_collection->collection = COLLECTION(collection);

	adj = view_collection->collection->vadj;
	gtk_viewport_set_vadjustment(viewport, adj);
	gtk_viewport_set_hadjustment(viewport, NULL); /* Or Gtk will crash */
	gtk_viewport_set_shadow_type(viewport, GTK_SHADOW_NONE);
	gtk_container_add(GTK_CONTAINER(object), collection);
	gtk_widget_show(collection);
	gtk_widget_set_size_request(GTK_WIDGET(view_collection), 4, 4);

	gtk_container_set_resize_mode(GTK_CONTAINER(viewport),
			GTK_RESIZE_IMMEDIATE);

	view_collection->collection->free_item = display_free_colitem;
	view_collection->collection->draw_item = draw_item;
	view_collection->collection->test_point = test_point;
	view_collection->collection->cb_user_data = view_collection;

	g_signal_connect(collection, "style_set",
			G_CALLBACK(style_set),
			view_collection);

	g_signal_connect(collection, "lose_selection",
			G_CALLBACK(lost_selection), view_collection);
	g_signal_connect(collection, "selection_changed",
			G_CALLBACK(selection_changed), view_collection);

	g_signal_connect(collection, "button-release-event",
			G_CALLBACK(coll_button_release), view_collection);
	g_signal_connect(collection, "button-press-event",
			G_CALLBACK(coll_button_press), view_collection);
	g_signal_connect(collection, "motion-notify-event",
			G_CALLBACK(coll_motion_notify), view_collection);
	g_signal_connect(viewport, "size-allocate",
			G_CALLBACK(size_allocate), view_collection);

	gtk_widget_set_events(collection,
			GDK_BUTTON1_MOTION_MASK | GDK_BUTTON2_MOTION_MASK |
			GDK_BUTTON3_MOTION_MASK | GDK_POINTER_MOTION_MASK |
			GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
}

static void draw_dir_mark(cairo_t *cr,
		GtkWidget *widget, GdkRectangle *rect, GdkColor *colour)
{
	int size = MAX(rect->width, rect->height);
	size = MIN(
			MAX(size / 7, small_height * 2/3),
			MIN(small_height, size /2));
	int right = rect->x + rect->width;
	int mid = rect->y + rect->height / 2;

	cairo_set_antialias(cr, CAIRO_ANTIALIAS_GOOD);
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	cairo_move_to(cr, right, mid - size);
	cairo_line_to(cr, right, mid + size);
	cairo_line_to(cr, right - size, mid);
	cairo_line_to(cr, right, mid - size);

	set_bg_src(cr, widget);
	cairo_fill(cr);

	size -= 1.4;

	gdk_cairo_set_source_color(cr, colour);
	cairo_set_line_width(cr, 0.9);
	cairo_move_to(cr, right, mid + size);
	cairo_line_to(cr, right - size, mid);
	cairo_line_to(cr, right, mid - size);
	size -= 2.1;
	right += 1;
	cairo_move_to(cr, right, mid + size);
	cairo_line_to(cr, right - size, mid);
	cairo_line_to(cr, right, mid - size);
	cairo_stroke(cr);
}
static void draw_cursor(GtkWidget *widget,
		GdkRectangle *rect, Collection *col, GdkColor *colour)
{
	cairo_t *cr = gdk_cairo_create(widget->window);

	cairo_set_operator(cr, CAIRO_OPERATOR_OVER);
	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	cairo_set_line_width(cr, 1.0);

	double dashes[] = {3.0, 1.0};
	cairo_set_dash(cr, dashes, 2, 0);

	if (gtk_widget_has_focus(widget))
		gdk_cairo_set_source_color(cr, colour);
	else
		cairo_set_source_rgb(cr, .7, .7, .7);

	cairo_move_to(cr, rect->x + .5, rect->y + .5);
	cairo_rel_line_to(cr, col->item_width - 1, 0);
	cairo_rel_line_to(cr, 0, col->item_height - 1);
	cairo_move_to(cr, rect->x + .5, rect->y + .5);
	cairo_rel_line_to(cr, 0, col->item_height - 1);
	cairo_rel_line_to(cr, col->item_width - 1, 0);

	cairo_stroke(cr);

	cairo_destroy(cr);
}

static gboolean next_thumb(ViewCollection *vc)
{
	int i;

	for (i = 0; i < 3; i++)
	{
		if (g_queue_is_empty(vc->thumbs_queue))
		{
			vc->thumb_func = 0;
			g_object_unref(vc);
			return FALSE;
		} else {
			int idx = GPOINTER_TO_INT(g_queue_pop_tail(vc->thumbs_queue));
			if (idx >= vc->collection->number_of_items)
				continue;

			FilerWindow    *fw = vc->filer_window;
			CollectionItem *colitem = &vc->collection->items[idx];
			DirItem        *item = (DirItem *) colitem->data;
			ViewData       *view = (ViewData *) colitem->view_data;

			if (view->iconstatus != 4)
				continue;

			if (!view->thumb)
			{
				gchar *path = pathdup(
						make_path(fw->real_path, item->leafname));
				view->thumb = pixmap_load_thumb(path);
				g_free(path);
			}

			if (!view->image || view->thumb)
			{
				if (!view->image)
				{
					view->image = di_image(item);
					if (view->image)
						g_object_ref(view->image);
				}
				collection_draw_item(vc->collection, idx, TRUE);
			}

			view->iconstatus = 2;
		}
	}

	return TRUE;
}

static void clear_thumb_func(ViewCollection *vc)
{
	if (vc->thumb_func)
	{
		g_source_remove(vc->thumb_func);
		vc->thumb_func = 0;
		g_object_unref(vc);
	}
	if (vc->thumbs_queue)
	{
		g_queue_free(vc->thumbs_queue);
		vc->thumbs_queue = NULL;
	}
}
static void reset_thumb_func(ViewCollection *vc)
{
	clear_thumb_func(vc);
	vc->thumbs_queue = g_queue_new();
}

static int is_linked(FilerWindow *fw, DirItem *item)
{
	return !fw->right_link ? FALSE :
		strcmp(g_strrstr(fw->right_link->sym_path, "/") + 1, item->leafname) ?
		FALSE : TRUE;
}

static void draw_item(GtkWidget *widget,
		int idx,
		GdkRectangle *area,
		gpointer user_data,
		gboolean cursor)
{
	ViewCollection *vc = (ViewCollection *) user_data;
	FilerWindow    *fw = vc->filer_window;
	CollectionItem *colitem = &vc->collection->items[idx];
	DirItem        *item = (DirItem *) colitem->data;
	ViewData       *view = (ViewData *) colitem->view_data;
	GdkColor       *select_colour = NULL, *type_colour;
	GdkColor       *fg = &widget->style->fg[GTK_STATE_NORMAL];
	Template       template;

	cairo_t *cr;
	static GdkColor red = {0, 0xffff, 0, 0};

	g_return_if_fail(view != NULL);

	if (view->iconstatus == 0) {
		if (fw->display_style == HUGE_ICONS && fw->sort_type == SORT_NAME &&
				vc->collection->vadj->value == 0) return;
		goto end_image;
	}

	if (view->iconstatus < 2)
	{
		gboolean re = view->iconstatus == -1;
		view->iconstatus = 2;

		g_clear_object(&view->image);
		g_clear_object(&view->thumb);

		view->image = get_globicon(
				make_path(fw->sym_path, item->leafname));

		if (!view->image)
		{
			const guchar *path = make_path(fw->real_path, item->leafname);

			view->image = get_globicon(path);

			//.DirIcon
			if (!view->image && fw->show_thumbs &&
					item->base_type == TYPE_FILE)
				view->image = g_fscache_lookup_full(pixmap_cache, path,
						FSCACHE_LOOKUP_ONLY_NEW, NULL);
		}

		if (!view->image)
		{
			if (fw->show_thumbs &&
					!(item->flags & ITEM_FLAG_APPDIR) &&
					(item->base_type == TYPE_FILE ||
					 (item->base_type == TYPE_DIRECTORY &&
					  o_display_show_dir_thumbs.int_value == 1)))
			{
				//delay loading in scroll is not good,
				//because half of view is blank and also too blink.
				if (!re && (
						fw->display_style != HUGE_ICONS ||
						fw->scanning ||
						vc->collection->vadj->value == 0
				))
					view->iconstatus = 4;
				else
					view->iconstatus = 3;
			}
			else
			{
				view->image = di_image(item);
				if (view->image)
					g_object_ref(view->image);
			}
		}
	}

	if (view->iconstatus > 2)
	{
		if (view->iconstatus == 4)
		{
			g_queue_push_head(vc->thumbs_queue, GUINT_TO_POINTER(idx));
			if (!vc->thumb_func)
			{
				g_object_ref(vc);
				vc->thumb_func = g_idle_add_full(
						G_PRIORITY_HIGH_IDLE + 20, //G_PRIORITY_DEFAULT_IDLE
						(GSourceFunc) next_thumb, vc, NULL);
			}

			if (fw->display_style == HUGE_ICONS) return;
		}
		else if (view->iconstatus == 3)
		{
			view->iconstatus = 2;
			g_clear_object(&view->thumb);
			gchar *path = pathdup(make_path(fw->real_path, item->leafname));
			view->thumb = pixmap_load_thumb(path);
			g_free(path);
		}

		if (!view->thumb && !view->image)
		{
			view->image = di_image(item);
			if (view->image)
				g_object_ref(view->image);
		}
	}

end_image:

	cr = gdk_cairo_create(widget->window);
	type_colour = type_get_colour(item, fg);

	if (colitem->selected)
		select_colour = &widget->style->base[fw->selection_state];

	if (!view->name)
		view->name = make_layout(fw, item);
	if (view->name_width == 0)
		pango_layout_get_pixel_size(view->name,
				&view->name_width, &view->name_height);

	PangoLayout *details = NULL;
	if (fw->details_type != DETAILS_NONE)
		details = make_details_layout(fw, item, view, FALSE);

	fill_template(area, colitem, vc, &template);

	if (cursor)
		draw_cursor(widget, area, vc->collection, type_colour);

	GdkPixbuf *sendi = view->thumb;

	if (!sendi && view->image)
	{
		if (template.icon.width <= small_width &&
				template.icon.height <= small_height)
		{
			if (!view->image->sm_pixbuf)
				pixmap_make_small(view->image);

			sendi = view->image->sm_pixbuf;
		}
		else if (template.icon.width <= ICON_WIDTH &&
				template.icon.height <= ICON_HEIGHT)
			sendi = view->image->pixbuf;
		else
			sendi = view->image->src_pixbuf;
	}

	draw_huge_icon(widget->window, widget->style, &template.icon, item,
			sendi, colitem->selected, select_colour);

	//	g_clear_object(&(view->thumb));

	if (item->base_type == TYPE_DIRECTORY)
	{
		gboolean link = is_linked(fw, item);
		if (link || (view->thumb &&
				(template.icon.width > small_width ||
				 template.icon.height > small_height))
				)
			draw_dir_mark(cr, widget, &template.icon,
					link ? &red :
						colitem->selected ? select_colour : type_colour);
	}


	fg = colitem->selected ?
		&widget->style->text[fw->selection_state] : type_colour;

	draw_string(cr, view->name,
			&template.leafname,
			view->name_width,
			view->name_height,
			fg,
			select_colour);

	if (fw->details_type != DETAILS_NONE && details)
	{
		draw_string(cr, details,
				&template.details,
				template.details.width,
				0,
				fg,
				select_colour);
		g_object_unref(details);
	}

	cairo_destroy(cr);
}

/* A template contains the locations of the three rectangles (for the icon,
 * name and extra details).
 * Fill in the empty 'template' with the rectanges for this item.
 */
static void fill_template(GdkRectangle *area, CollectionItem *colitem,
			ViewCollection *view_collection, Template *template)
{
	DisplayStyle style = view_collection->filer_window->display_style;
	ViewData     *view = (ViewData *) colitem->view_data;

	if (view_collection->filer_window->details_type != DETAILS_NONE)
	{
		template->details.width = view->details_width;
		template->details.height = view->details_height;

		if (style == SMALL_ICONS)
			small_full_template(area, colitem,
						view_collection, template);
		else if (style == LARGE_ICONS)
			large_template(area, colitem,
						view_collection, template, TRUE);
		else
			huge_template(area, colitem,
						view_collection, template, TRUE);
	}
	else
	{
		if (style == HUGE_ICONS)
			huge_template(area, colitem,
					view_collection, template, FALSE);
		else if (style == LARGE_ICONS)
			large_template(area, colitem,
					view_collection, template, FALSE);
		else
			small_template(area, colitem,
					view_collection, template);
	}
}

static void huge_template(
		GdkRectangle *area,
		CollectionItem *colitem,
		ViewCollection *view_collection,
		Template *template,
		gboolean full
		)
{
	gfloat scale = view_collection->filer_window->icon_scale;
	ViewData *view = (ViewData *) colitem->view_data;
	MaskedPixmap *image = view->image;
	int col_width = view_collection->collection->item_width;
	int iw, ih;

	if (image || view->thumb)
	{
		if (view->thumb)
		{
			iw = gdk_pixbuf_get_width(view->thumb);
			ih = gdk_pixbuf_get_height(view->thumb);
		} else {
			iw = image->huge_width;
			ih = image->huge_height;
		}

		if (iw <= ICON_WIDTH &&
			ih <= ICON_HEIGHT)
			scale = 1.0;
		else
			scale *= (gfloat) huge_size / MAX(iw, ih);

		iw *= scale;
		ih *= scale;

		iw = MAX(iw, ICON_WIDTH);
		ih = MAX(ih, ICON_HEIGHT);
	}
	else
		iw = ih = huge_size * scale;

	template->icon.width = iw;
	template->icon.height = ih;

	if (full)
	{
		int max_text_width = col_width - 2;

		template->leafname.width = MIN(max_text_width, view->name_width);
		template->leafname.height = MIN(view->name_height,
				area->height - ih - template->details.height - 2);

		template->icon.x = area->x + (col_width - iw) / 2;
		template->icon.y = area->y +
			(area->height - template->details.height -
			 template->leafname.height - ih - 1);

		template->leafname.x = area->x + MAX((col_width - view->name_width) / 2, 3);
		template->leafname.y = template->icon.y + ih;

		template->details.x = area->x + (col_width - template->details.width) / 2;
		template->details.y = area->y + area->height - template->details.height - 1;
	}
	else
	{
		template->leafname.width = view->name_width;
		template->leafname.height = MIN(view->name_height,
				area->height - ih - 2);

		template->leafname.x = area->x +
			MAX((col_width - template->leafname.width) >> 1, 2);
		template->icon.x = area->x + ((col_width - iw) >> 1);

		template->icon.y = area->y +
			(area->height - template->leafname.height - ih - 2) / 2 + 1;
		template->leafname.y = template->icon.y + ih;
	}

}

static void large_template(
		GdkRectangle *area,
		CollectionItem *colitem,
		ViewCollection *view_collection,
		Template *template,
		gboolean full
		)
{
	int col_width = view_collection->collection->item_width;
	int iw, ih, ix, iy, tx, ty;
	ViewData *view = (ViewData *) colitem->view_data;
	MaskedPixmap *image = view->image;

	if (view->thumb)
	{
		iw = gdk_pixbuf_get_width(view->thumb);
		ih = gdk_pixbuf_get_height(view->thumb);
		double scale = MAX(iw / (double) ICON_WIDTH,
				ih / (double) ICON_HEIGHT + 0.0);

		iw /= scale;
		ih /= scale;

		iw = MAX(iw, ICON_WIDTH * 3/4);
		ih = MAX(ih, ICON_HEIGHT * 3/4);
	}
	else if (image)
	{
		iw = MIN(image->width, ICON_WIDTH);
		ih = MIN(image->height, ICON_HEIGHT);
	}
	else
	{
		iw = ICON_WIDTH;
		ih = ICON_HEIGHT;
	}

//	ih = MIN(ICON_HEIGHT, ih);

	template->icon.width = iw;
	template->icon.height = ih;

	if (full)
	{
		template->icon.width += 3;
		int max_text_width = area->width - ICON_WIDTH - 4;

		template->icon.x = area->x + (ICON_WIDTH - iw) / 2 + 2;
		template->icon.y = area->y + (area->height - ih) / 2 + 1;

		tx = area->x + ICON_WIDTH + 4;
		ty = area->y + area->height / 2
				- (view->name_height + 2 + template->details.height) / 2;

		template->leafname.x = tx;
		template->leafname.y = ty;

		template->leafname.width = MIN(max_text_width, view->name_width);
		template->leafname.height = view->name_height;

		template->details.x = tx;
		template->details.y = ty + view->name_height + 2;
	}
	else
	{
		ix = area->x + ((col_width - iw) >> 1);

		template->leafname.width = view->name_width;
		template->leafname.height = MIN(view->name_height, area->height - ICON_HEIGHT - 2);

		tx = area->x + MAX((col_width - template->leafname.width) >> 1, 2);
		ty = area->y + ICON_HEIGHT + 1;

		template->leafname.x = tx;
		template->leafname.y = ty;

		iy = ty - ih;
		iy -= (ICON_HEIGHT - ih) / 6;
		iy = MAX(area->y, iy);

		template->icon.x = ix;
		template->icon.y = iy;
	}
}

static void small_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template)
{
	int margin = small_height / 4;

	int	text_x = area->x + small_width + margin;
	int	low_text_y;
	int	max_text_width = area->width - small_width - margin;
	ViewData *view = (ViewData *) colitem->view_data;

	low_text_y = area->y + area->height / 2 - view->name_height / 2;

	template->leafname.x = text_x;
	template->leafname.y = low_text_y;
	template->leafname.width = MIN(max_text_width, view->name_width);
	template->leafname.height = view->name_height;

	template->icon.x = area->x + 2;
	template->icon.y = area->y + 1;
	template->icon.width = small_width + margin - 1;
	template->icon.height = small_height;
}

static void small_full_template(GdkRectangle *area, CollectionItem *colitem,
			   ViewCollection *view_collection, Template *template)
{
	int	col_width = view_collection->collection->item_width;
	GdkRectangle temparea = *area;

	temparea.width = col_width - template->details.width;

	small_template(&temparea, colitem, view_collection, template);

	template->icon.x = area->x + 2;
	template->icon.y = area->y + 1;
	template->details.x = area->x + col_width - template->details.width;
	template->details.y =
		area->y + area->height / 2 - template->details.height / 2;
}

#define INSIDE(px, py, area, ladj, radj)	\
	(px > area.x + ladj && py >= area.y && \
	 px < area.x - radj + area.width && py <= area.y + area.height)

static gboolean test_point(Collection *collection,
				int point_x, int point_y,
				CollectionItem *colitem,
				int width, int height,
				gpointer user_data)
{
	Template	template;
	GdkRectangle	area;
	ViewData	*view = (ViewData *) colitem->view_data;
	ViewCollection	*view_collection = (ViewCollection *) user_data;

	area.x = 0;
	area.y = 0;
	area.width = width;
	area.height = height;

	fill_template(&area, colitem, view_collection, &template);

	DisplayStyle style = view_collection->filer_window->display_style;
	int ladj = style == LARGE_ICONS ?  0 : small_height / 4;
	int radj = style == HUGE_ICONS ?  ladj : 0;

	return INSIDE(point_x, point_y, template.leafname, 0, 0) ||
	       INSIDE(point_x, point_y, template.icon, ladj, radj) ||
	       (view->details_width && INSIDE(point_x, point_y, template.details, 0, 0));
}

/* 'box' renders a background box if the string is also selected */
static void draw_string(cairo_t *cr,
		PangoLayout  *layout,
		GdkRectangle *area,  /* Area available on screen */
		int          width,  /* Width of the full string */
		int          height, /* height of the full string */
		GdkColor     *fg,
		GdkColor     *bg)
{
	gboolean saved = FALSE;
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	if (width > area->width || height > area->height || bg)
	{
		cairo_save(cr);
		saved = TRUE;
		cairo_rectangle(cr, area->x, area->y, area->width, area->height);
		cairo_clip(cr);
	}

	if (bg)
	{
		gdk_cairo_set_source_color(cr, bg);
		cairo_paint(cr);
	}

	gdk_cairo_set_source_color(cr, fg);

	cairo_move_to(cr, area->x, area->y);
	pango_cairo_show_layout(cr, layout);

	if (saved)
		cairo_restore(cr);

	if (width > area->width || height > area->height)
	{
		GdkColor red = {0, 0xffff, 0, 0};
		gdk_cairo_set_source_color(cr, &red);
		cairo_set_line_width(cr, 1.0);

		gint right = area->x + area->width;
		gint bottom = area->y + area->height;

		if (width > area->width)
		{
			cairo_move_to(cr, right - .5, area->y + 1);
			cairo_line_to(cr, right - .5, bottom - 1);
		}
		if (height > area->height)
		{
			cairo_move_to(cr, right - small_width, bottom - .5);
			cairo_line_to(cr, right - 1,           bottom - .5);
		}

		cairo_stroke(cr);
	}
}

static void view_collection_scroll_to_top(ViewIface *view)
{
	Collection *col = ((ViewCollection *) view)->collection;
	gtk_adjustment_set_value(col->vadj, 0);
}

/* Create the handers for the View interface */
static void view_collection_iface_init(gpointer giface, gpointer iface_data)
{
	ViewIfaceClass *iface = giface;

	g_assert(G_TYPE_FROM_INTERFACE(iface) == VIEW_TYPE_IFACE);

	/* override stuff */
	iface->sort = view_collection_sort;
	iface->style_changed = view_collection_style_changed;
	iface->add_items = view_collection_add_items;
	iface->update_items = view_collection_update_items;
	iface->delete_if = view_collection_delete_if;
	iface->clear = view_collection_clear;
	iface->select_all = view_collection_select_all;
	iface->clear_selection = view_collection_clear_selection;
	iface->count_items = view_collection_count_items;
	iface->count_selected = view_collection_count_selected;
	iface->show_cursor = view_collection_show_cursor;
	iface->get_iter = view_collection_get_iter;
	iface->get_iter_at_point = view_collection_get_iter_at_point;
	iface->cursor_to_iter = view_collection_cursor_to_iter;
	iface->set_selected = view_collection_set_selected;
	iface->get_selected = view_collection_get_selected;
	iface->set_frozen = view_collection_set_frozen;
	iface->select_only = view_collection_select_only;
	iface->wink_item = view_collection_wink_item;
	iface->autosize = view_collection_autosize;
	iface->cursor_visible = view_collection_cursor_visible;
	iface->set_base = view_collection_set_base;
	iface->start_lasso_box = view_collection_start_lasso_box;
	iface->extend_tip = view_collection_extend_tip;
	iface->auto_scroll_callback = view_collection_auto_scroll_callback;
	iface->scroll_to_top = view_collection_scroll_to_top;
}

static void view_collection_extend_tip(ViewIface *view, ViewIter *iter,
					GString *tip)
{
	ViewCollection*view_collection = (ViewCollection *) view;
	Collection *collection = view_collection->collection;
	FilerWindow *filer_window = view_collection->filer_window;
	Template template;
	int i = iter->i;
	CollectionItem	*colitem = &collection->items[i];
	ViewData *view_data = (ViewData *) colitem->view_data;
	GdkRectangle area;
	int row,col;

	collection_item_to_rowcol(collection, i, &row, &col);

	g_return_if_fail(iter->view == (ViewIface *) view_collection);
	g_return_if_fail(i >= 0 && i < collection->number_of_items);

	/* TODO: What if the window is narrower than 1 column? */

	area.x = col * collection->item_width;
	area.y = row * collection->item_height;
	area.height = collection->item_height;

	if (filer_window->display_style == SMALL_ICONS && col == collection->columns - 1)
		area.width = GTK_WIDGET(collection)->allocation.width - area.x;
	else
		area.width = collection->item_width;

	fill_template(&area, colitem, view_collection, &template);

	if (template.leafname.width < view_data->name_width ||
		template.leafname.height < view_data->name_height)
	{
		DirItem *item = (DirItem *) collection->items[i].data;

		g_string_append(tip, item->leafname);
		g_string_append_c(tip, '\n');
	}
}

static gint coll_motion_notify(GtkWidget *widget,
			       GdkEventMotion *event,
			       ViewCollection *view_collection)
{
	return filer_motion_notify(view_collection->filer_window, event);
}

/* Viewport is to be resized, so calculate increments */
static void size_allocate(GtkWidget *w, GtkAllocation *a, gpointer data)
{
	Collection *col = ((ViewCollection *) data)->collection;

	col->vadj->step_increment = col->item_height;
	col->vadj->page_increment = col->vadj->page_size;
}

static gint coll_button_release(GtkWidget *widget,
			        GdkEventButton *event,
				ViewCollection *view_collection)
{
	if (dnd_motion_release(event))
	{
		if (motion_buttons_pressed == 0 &&
					view_collection->collection->lasso_box)
		{
			filer_set_autoscroll(view_collection->filer_window,
					     FALSE);

			collection_end_lasso(view_collection->collection,
				event->button != 1 ||
				event->state & GDK_CONTROL_MASK ||
				event->state & GDK_MOD1_MASK ?
					GDK_INVERT : GDK_SET);
		}

		return FALSE;
	}

	filer_perform_action(view_collection->filer_window, event);

	return FALSE;
}

static gint coll_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      ViewCollection *view_collection)
{
	collection_set_cursor_item(view_collection->collection, -1, TRUE);

	if (dnd_motion_press(widget, event))
		filer_perform_action(view_collection->filer_window, event);
	else {
		/* rocker gesture */
		if (motion_state == MOTION_READY_FOR_DND)
			dnd_motion_disable();

		filer_set_autoscroll(view_collection->filer_window, FALSE);
		collection_end_lasso(view_collection->collection, GDK_CLEAR);

		change_to_parent(view_collection->filer_window);
	}

	return FALSE;
}

/* Nothing is selected anymore - give up primary */
static void lost_selection(Collection  *collection,
			   guint        time,
			   gpointer     user_data)
{
	ViewCollection *view_collection = VIEW_COLLECTION(user_data);

	filer_lost_selection(view_collection->filer_window, time);
}

static void selection_changed(Collection *collection,
			      gint time,
			      gpointer user_data)
{
	ViewCollection *view_collection = VIEW_COLLECTION(user_data);

	filer_selection_changed(view_collection->filer_window, time);
}

static void display_free_colitem(Collection *collection,
				 CollectionItem *colitem)
{
	ViewData	*view = (ViewData *) colitem->view_data;

	if (!view)
		return;

	if (view->name)
		g_object_unref(view->name);

	if (view->image)
		g_object_unref(view->image);

	if (view->thumb)
		g_object_unref(view->thumb);

	g_free(view);
}

static void style_set(Collection 	*collection,
		      GtkStyle		*style,
		      ViewCollection	*view_collection)
{
	view_collection_style_changed(VIEW(view_collection),
			VIEW_UPDATE_VIEWDATA | VIEW_UPDATE_NAME);
}

/* Return the size needed for this item */
//if reached max size then return true;
static gfloat calc_size(FilerWindow *fw, CollectionItem *colitem,
		int *mwidth, int *mheight, gint number_of_items)
{
	DisplayStyle style = fw->display_style;
	DetailsType  type  = fw->details_type;
	ViewData     *view = (ViewData *) colitem->view_data;
	gfloat       scale = fw->icon_scale;

	int w, h;
	gboolean max_h = FALSE;
	gboolean max_w = FALSE;

	static const int MARGIN = 2;

	int nw = view->name_width;

	if (style == SMALL_ICONS) {
		max_h = TRUE;

		if (type == DETAILS_NONE)
		{
			w = nw;
			if (w >= o_small_width.int_value)
			{
				max_w = TRUE;
				w = o_small_width.int_value;
			}
			w += small_width * 1.2;
			h = small_height;
		}
		else
		{
			w = small_width * 1.2 + view->details_width;
			if (nw >= o_small_width.int_value)
			{
				if (type != DETAILS_TYPE)
					max_w = TRUE;

				w += o_small_width.int_value;
			}
			else
				w += nw;

			h = MAX(view->name_height, view->details_height);
			h = MAX(h, small_height);
		}
	}
	else
	{//large or huge
		int pix_size = 0;
		int dw = view->details_width;
		int nh = view->name_height;
		int ml = 0;

		if (o_max_length.int_value)
			nw = MIN(nw, ml = o_max_length.int_value);

		if (type == DETAILS_NONE || style == HUGE_ICONS)
		{
			pix_size = style == HUGE_ICONS ?
				huge_size * scale : ICON_WIDTH;
			int ha = style == HUGE_ICONS ? pix_size : ICON_HEIGHT;
			nw = MAX(pix_size, nw);

			if (type != DETAILS_NONE)
			{
				ha += view->details_height;
				nw = MAX(dw, nw);
				if (type == DETAILS_SIZE)
					nw = MAX(nw, fw_mono_width * 6);
			}

			if (type != DETAILS_TYPE && o_max_length.int_value)
			{
				int mnh =
					((o_max_length.int_value - 1) /
					 MAX(o_large_width.int_value, 1) + 1) * fw_font_height;

				if (nh >= mnh)
				{
					max_h = TRUE;
					nh = mnh;
				}
			}
			h = nh + ha;

			if (max_h && type != DETAILS_TYPE)
			{
				w = MAX(pix_size,
						o_large_width.int_value);

				if (style == HUGE_ICONS)
					w = MAX(w, huge_size);

				if (type != DETAILS_NONE)
					w = MAX(dw, huge_size);

				if (w < nw + number_of_items * o_large_width.int_value / 3000)
					max_w = TRUE;
				else
					w = nw;
			}
			else
				w = nw;
		}
		else
		{
			if (type != DETAILS_TYPE && o_max_length.int_value &&
				(nw >= ml || dw > ml))
				max_h = max_w = TRUE;

			w = ICON_WIDTH + 2 + MAX(dw, nw);
			h = ICON_HEIGHT;
		}
	}

	w += MARGIN;
	h += MARGIN;

	*mwidth = MAX(*mwidth, w);
	*mheight = MAX(*mheight, h);

	if (fw->name_scale != 1.0)
	{
		*mwidth  = fw->name_scale_itemw * fw->name_scale;
		max_w = TRUE;
	}

	return max_h && max_w ? scale : .0;
}

static void update_item(ViewCollection *view_collection, int i)
{
	Collection *collection = view_collection->collection;
	int	old_w = collection->item_width;
	int	old_h = collection->item_height;
	int	w = old_w, h = old_h;
	CollectionItem *colitem;
	FilerWindow *filer_window = view_collection->filer_window;

	g_return_if_fail(i >= 0 && i < collection->number_of_items);
	colitem = &collection->items[i];

	if (filer_window->onlyicon)
	{
		if (((ViewData *) colitem->view_data)->iconstatus > 1)
		{
			((ViewData *) colitem->view_data)->iconstatus = -1;
			collection_draw_item(collection, i, TRUE);
		}
		return;
	}

	display_update_view(filer_window,
			(DirItem *) colitem->data,
			(ViewData *) colitem->view_data,
			FALSE,
			collection->reached_scale != .0);

	if (collection->reached_scale == .0)
	{
		collection->reached_scale =
			calc_size(filer_window, colitem, &w, &h,
					collection->number_of_items);

		if (w > old_w || h > old_h)
		{
			collection_set_item_size(collection,
						 MAX(old_w, w),
						 MAX(old_h, h));

			//only lager
			filer_window->may_resize = TRUE;
		}
	}

	if (!filer_window->req_sort) //will redraw soon
		collection_draw_item(collection, i, TRUE);
}

/* Implementations of the View interface. See view_iface.c for comments. */

static void view_collection_style_changed(ViewIface *view, int flags)
{
	ViewCollection *view_collection = VIEW_COLLECTION(view);
	FilerWindow	*filer_window = view_collection->filer_window;
	int		i;
	Collection	*col = view_collection->collection;
	int		width = MIN_ITEM_WIDTH;
	int		height = small_height;
	int		n = col->number_of_items;

	if (filer_window->under_init) return;

	if (flags != VIEW_UPDATE_VIEWDATA)
		col->reached_scale = .0;

	if (flags & VIEW_UPDATE_NAME && !(flags & VIEW_UPDATE_SCALE))
		filer_window->name_scale = 1.0;

	if (n == 0 && filer_window->display_style != SMALL_ICONS)
		height = ICON_HEIGHT;

	view_collection->collection->vertical_order = FALSE;
	if (filer_window->display_style == SMALL_ICONS &&
	    o_vertical_order_small.int_value)
	  view_collection->collection->vertical_order = TRUE;
	if (filer_window->display_style != SMALL_ICONS &&
	    o_vertical_order_large.int_value)
	  view_collection->collection->vertical_order = TRUE;

	/* Recalculate all the ViewData structs for this window
	 * (needed if the text or image has changed in any way) and
	 * get the size of each item.
	 */
	for (i = 0; i < n; i++)
	{
		CollectionItem *ci = &col->items[i];
		gboolean nosize = FALSE;

		if (flags & (VIEW_UPDATE_VIEWDATA | VIEW_UPDATE_NAME) ||
				(nosize = col->reached_scale == .0 &&
				 !((ViewData *) ci->view_data)->name_width)
		)
			display_update_view(filer_window,
					(DirItem *) ci->data,
					(ViewData *) ci->view_data,
					(flags & VIEW_UPDATE_NAME) || nosize,
					col->reached_scale != .0);

		if (flags != VIEW_UPDATE_VIEWDATA && col->reached_scale == .0)
			col->reached_scale = calc_size(filer_window, ci, &width, &height,
					col->number_of_items);
	}

	if (flags != VIEW_UPDATE_VIEWDATA)
		collection_set_item_size(col, width, height);

	reset_thumb_func(view_collection);

	gtk_widget_queue_draw(GTK_WIDGET(view_collection));
}

typedef int (*SortFn)(gconstpointer a, gconstpointer b);

static SortFn sort_fn(FilerWindow *fw)
{
	switch (fw->sort_type)
	{
		case SORT_NAME: return sort_by_name;
		case SORT_TYPE: return sort_by_type;
		case SORT_DATEA: return sort_by_datea;
		case SORT_DATEC: return sort_by_datec;
		case SORT_DATEM: return sort_by_datem;
		case SORT_SIZE: return sort_by_size;
		case SORT_PERM: return sort_by_perm;
		case SORT_OWNER: return sort_by_owner;
		case SORT_GROUP: return sort_by_group;
		default:
			g_assert_not_reached();
	}

	return NULL;
}

static void view_collection_sort(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	FilerWindow	*filer_window = view_collection->filer_window;

	collection_qsort(view_collection->collection, sort_fn(filer_window),
			filer_window->sort_order);
}


#define PUSHWEIGHT 44
static int oldnum, newnum, cutrest;
static void addt(int start, ViewCollection *view)
{
	start--; //to ignore NULL it is added 1
	Collection *coll = view->collection;
	FilerWindow *fw = view->filer_window;

	for (int i = start; i < newnum && i < start + PUSHWEIGHT; i++)
	{
		CollectionItem *colitem = &coll->items[i];
		display_update_view(fw, colitem->data, colitem->view_data, TRUE, cutrest);
	}
}
static gpointer addloopt(ViewCollection *view)
{
	int pross = g_get_num_processors();
	GThreadPool *pool = g_thread_pool_new((GFunc)addt, view, pross, TRUE, NULL);

	for (int i = oldnum; i < newnum; i += PUSHWEIGHT)
		if (cutrest || pross == 1)
			//pool_push is heavy
			addt(i + 1, view);
		else
			g_thread_pool_push(pool, GINT_TO_POINTER(i + 1), NULL);

	g_thread_pool_free(pool, FALSE, TRUE);
	return NULL;
}
static void view_collection_add_items(ViewIface *view, GPtrArray *items)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;
	FilerWindow	*filer_window = view_collection->filer_window;
	int old_w = collection->item_width;
	int old_h = collection->item_height;
	int mw = old_w, mh = old_h;

	oldnum = collection->number_of_items;

	//gint64 startt = g_get_monotonic_time();

	for (int i = 0; i < items->len; i++)
	{
		DirItem *item = (DirItem *) items->pdata[i];
		if (!filer_match_filter(filer_window, item))
			continue;

		collection_insert(collection, item, g_new0(ViewData, 1));
	}

	newnum = collection->number_of_items;
	cutrest = collection->reached_scale != .0;

	GThread *loopt = g_thread_new("addloop", (GThreadFunc)addloopt, view_collection);
	g_thread_yield();

	if (!cutrest) for (int i = oldnum; i < newnum; i++)
	{
		CollectionItem *colitem = &collection->items[i];
		ViewData *view = (ViewData *)colitem->view_data;
		while (!view->name_width) g_thread_yield();

		if (.0 != (collection->reached_scale =
				calc_size(filer_window, colitem, &mw, &mh, newnum)))
		{
			cutrest = 1;
			break;
		}
	}

	g_thread_join(loopt);

	//D(time %f, (g_get_monotonic_time() - startt) / 1000000.0)

	if (mw > old_w || mh > old_h)
		collection_set_item_size(collection,
					 MAX(old_w, mw),
					 MAX(old_h, mh));

	if (oldnum != newnum)
	{
		gtk_widget_queue_resize(GTK_WIDGET(collection));
		view_collection_sort(view);
	}
}

static void view_collection_update_items(ViewIface *view, GPtrArray *items)
{
	ViewCollection *view_collection = VIEW_COLLECTION(view);
	Collection     *collection = view_collection->collection;
	FilerWindow    *filer_window = view_collection->filer_window;
	int      i;
	gboolean mayfirsttime = FALSE;

	g_return_if_fail(items->len > 0);

	for (i = 0; i < items->len; i++)
	{
		DirItem *item = (DirItem *) items->pdata[i];
		int j = -1;

		if (!filer_match_filter(filer_window, item))
			continue;

		if (!mayfirsttime)
			j = collection_find_item(collection, item,
					sort_fn(filer_window), filer_window->sort_order);

		if (j < 0)
		{
			mayfirsttime = TRUE;
			j = collection_find_item(collection, item,
						sort_by_name, filer_window->sort_order);
		}

		if (j < 0)
		{
			mayfirsttime = FALSE;
			int k = collection->number_of_items;
			while (k--)
				if (item == collection->items[k].data)
				{
					j = k;
					break;
				}
		}

		if (j < 0)
			g_warning("Failed to find '%s'\n", (const gchar *) item->leafname);
		else
			update_item(view_collection, j);
	}

	if (filer_window->sort_type != SORT_NAME)
		gtk_widget_queue_draw(GTK_WIDGET(view_collection));
}

static void view_collection_delete_if(ViewIface *view,
			  gboolean (*test)(gpointer item, gpointer data),
			  gpointer data)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	collection_delete_if(collection, test, data);
}

static void view_collection_clear(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	collection_clear(collection);
}

static void view_collection_select_all(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	collection_select_all(collection);
}

static void view_collection_clear_selection(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	collection_clear_selection(collection);
}

static int view_collection_count_items(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	return collection->number_of_items;
}

static int view_collection_count_selected(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	return collection->number_selected;
}

static void view_collection_show_cursor(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	collection_move_cursor(collection, 0, 0, 0);
}

/* The first time the next() method is used, this is called */
static DirItem *iter_init(ViewIter *iter)
{
	ViewCollection *view_collection = (ViewCollection *) iter->view;
	Collection *collection = view_collection->collection;
	int i = -1;
	int n = collection->number_of_items;
	int flags = iter->flags;

	iter->peek = iter_peek;

	if (iter->n_remaining == 0)
		return NULL;

	if (flags & VIEW_ITER_FROM_CURSOR)
	{
		i = collection->cursor_item;
		if (i == -1 && (
					!(flags & VIEW_ITER_EVEN_OLD_CURSOR) ||
					collection->cursor_item_old == -1
					))
			return NULL;	/* No cursor */
	}
	else if (flags & VIEW_ITER_FROM_BASE)
		i = view_collection->cursor_base;

	if (flags & VIEW_ITER_EVEN_OLD_CURSOR)
		i = collection->cursor_item != -1 ?
			collection->cursor_item : collection->cursor_item_old;

	if (i < 0 || i >= n)
	{
		/* Either a normal iteration, or an iteration from an
		 * invalid starting point.
		 */
		if (flags & VIEW_ITER_BACKWARDS)
			i = n - 1;
		else
			i = 0;
	}

	if (i < 0 || i >= n)
		return NULL;	/* No items at all! */

	iter->next = flags & VIEW_ITER_BACKWARDS ? iter_prev : iter_next;
	iter->n_remaining--;
	iter->i = i;

	if (flags & VIEW_ITER_SELECTED && !collection->items[i].selected)
		return iter->next(iter);
	if (iter->flags & VIEW_ITER_DIR &&
			((DirItem *) collection->items[i].data)->base_type != TYPE_DIRECTORY)
		return iter->next(iter);

	return iter->peek(iter);
}
/* Advance iter to point to the next item and return the new item
 * (this saves you calling peek after next each time).
 */
static DirItem *iter_next(ViewIter *iter)
{
	Collection *collection = ((ViewCollection *) iter->view)->collection;
	int n = collection->number_of_items;
	int i = iter->i;

	g_return_val_if_fail(iter->n_remaining >= 0, NULL);

	/* i is the last item returned (always valid) */

	g_return_val_if_fail(i >= 0 && i < n, NULL);

	while (iter->n_remaining)
	{
		i++;
		iter->n_remaining--;

		if (i == n)
		{
			if (iter->flags & VIEW_ITER_NO_LOOP)
				break;

			i = 0;
		}

		g_return_val_if_fail(i >= 0 && i < n, NULL);

		if (iter->flags & VIEW_ITER_SELECTED &&
		    !collection->items[i].selected)
			continue;

		if (iter->flags & VIEW_ITER_DIR &&
		    ((DirItem *) collection->items[i].data)->base_type != TYPE_DIRECTORY)
			continue;

		iter->i = i;
		return collection->items[i].data;
	}

	iter->i = -1;
	return NULL;
}

/* Like iter_next, but in the other direction */
static DirItem *iter_prev(ViewIter *iter)
{
	Collection *collection = ((ViewCollection *) iter->view)->collection;
	int n = collection->number_of_items;
	int i = iter->i;

	g_return_val_if_fail(iter->n_remaining >= 0, NULL);

	/* i is the last item returned (always valid) */

	g_return_val_if_fail(i >= 0 && i < n, NULL);

	while (iter->n_remaining)
	{
		i--;
		iter->n_remaining--;

		if (i == -1)
		{
			if (iter->flags & VIEW_ITER_NO_LOOP)
				break;

			i = collection->number_of_items - 1;
		}

		g_return_val_if_fail(i >= 0 && i < n, NULL);

		if (iter->flags & VIEW_ITER_SELECTED &&
		    !collection->items[i].selected)
			continue;

		if (iter->flags & VIEW_ITER_DIR &&
		    ((DirItem *) collection->items[i].data)->base_type != TYPE_DIRECTORY)
			continue;

		iter->i = i;
		return collection->items[i].data;
	}

	iter->i = -1;
	return NULL;
}

static DirItem *iter_peek(ViewIter *iter)
{
	Collection *collection = ((ViewCollection *) iter->view)->collection;
	int i = iter->i;

	if (i == -1)
		return NULL;

	g_return_val_if_fail(i >= 0 && i < collection->number_of_items, NULL);

	return collection->items[i].data;
}

static void make_iter(ViewCollection *view_collection, ViewIter *iter,
		      IterFlags flags)
{
	Collection *collection = view_collection->collection;

	iter->view = (ViewIface *) view_collection;
	iter->next = iter_init;
	iter->peek = NULL;
	iter->i = -1;

	iter->flags = flags;

	if (flags & VIEW_ITER_ONE_ONLY)
	{
		iter->n_remaining = 1;
		iter->next(iter);
	}
	else
		iter->n_remaining = collection->number_of_items;
}

/* Set the iterator to return 'i' on the next peek() */
static void make_item_iter(ViewCollection *view_collection,
			   ViewIter *iter, int i)
{
	Collection *collection = view_collection->collection;

	g_return_if_fail(i >= -1 && i < collection->number_of_items);

	make_iter(view_collection, iter, 0);

	iter->i = i;
	iter->next = iter_next;
	iter->peek = iter_peek;
	iter->n_remaining = 0;
}

static void view_collection_get_iter(ViewIface *view,
				     ViewIter *iter, IterFlags flags)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);

	make_iter(view_collection, iter, flags);
}

static void view_collection_get_iter_at_point(ViewIface *view, ViewIter *iter,
					      GdkWindow *src, int x, int y)
{
	ViewCollection *view_collection = VIEW_COLLECTION(view);
	Collection *collection = view_collection->collection;
	int i;

	if (src == ((GtkWidget *) view)->window)
	{
		/* The event is on the Viewport, not the collection... */
		y += collection->vadj->value;
	}

	i = collection_get_item(collection, x, y);
	make_item_iter(view_collection, iter, i);
}

static void view_collection_cursor_to_iter(ViewIface *view, ViewIter *iter)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;
	FilerWindow	*filer_window = view_collection->filer_window;

	g_return_if_fail(iter == NULL ||
			 iter->view == (ViewIface *) view_collection);

	collection_set_cursor_item(collection, iter ? iter->i : -1,
			filer_window->auto_scroll == -1);

	if (!iter)
		collection->cursor_item_old = -1;
}

static void view_collection_set_selected(ViewIface *view,
					 ViewIter *iter,
					 gboolean selected)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	g_return_if_fail(iter != NULL &&
			 iter->view == (ViewIface *) view_collection);
	g_return_if_fail(iter->i >= 0 && iter->i < collection->number_of_items);

	if (selected)
		collection_select_item(collection, iter->i);
	else
		collection_unselect_item(collection, iter->i);
}

static gboolean view_collection_get_selected(ViewIface *view, ViewIter *iter)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	g_return_val_if_fail(iter != NULL &&
			iter->view == (ViewIface *) view_collection, FALSE);
	g_return_val_if_fail(iter->i >= 0 &&
				iter->i < collection->number_of_items, FALSE);

	return collection->items[iter->i].selected;
}

static void view_collection_select_only(ViewIface *view, ViewIter *iter)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	g_return_if_fail(iter != NULL &&
			 iter->view == (ViewIface *) view_collection);
	g_return_if_fail(iter->i >= 0 && iter->i < collection->number_of_items);

	collection_clear_except(collection, iter->i);
}

static void view_collection_set_frozen(ViewIface *view, gboolean frozen)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	if (frozen)
		collection->block_selection_changed++;
	else
		collection_unblock_selection_changed(collection,
				gtk_get_current_event_time(), TRUE);
}

static void view_collection_wink_item(ViewIface *view, ViewIter *iter)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	if (!iter)
	{
		collection_wink_item(collection, -1);
		return;
	}

	g_return_if_fail(iter != NULL &&
			 iter->view == (ViewIface *) view_collection);
	g_return_if_fail(iter->i >= 0 && iter->i < collection->number_of_items);

	collection_wink_item(collection, iter->i);

	FilerWindow *fw = view_collection->filer_window;
	DirItem *item = iter->peek(iter);
	if (fw->right_link
			&& item->base_type == TYPE_DIRECTORY
			&& is_linked(fw, item))
		collection->winks_left = 1;
}

static void view_collection_autosize(ViewIface *view, gboolean turn)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	FilerWindow	*filer_window = view_collection->filer_window;
	Collection	*collection = view_collection->collection;
	int 		n;
	int		w = collection->item_width;
	int		h = collection->item_height;
	int 		x;
	int		rows, cols;
	int		max_x, max_y, min_x = 0;
	const float	r = 1.6;
	int		t = 0, tn;
	int		space = 0, exh = 0;

	if (filer_window->directory->error && filer_window->presented) return;

	/* Get the extra height required for the toolbar and minibuffer,
	 * if visible.
	 */
	if (o_toolbar.int_value != TOOLBAR_NONE)
		t = filer_window->toolbar->allocation.height;
	if (gtk_widget_get_visible(filer_window->message))
		t += filer_window->message->allocation.height;

	n = collection->number_of_items;
	if (n == 0)
		h = (filer_window->display_style != SMALL_ICONS ? ICON_HEIGHT : 0) +
			small_height;

	max_x = ((o_filer_width_limit.int_value == 0 ?
			 o_filer_size_limit.int_value :
			 o_filer_width_limit.int_value) * monitor_width) / 100;
	max_y = (o_filer_size_limit.int_value * monitor_height) / 100;

	if (filer_window->toolbar)
	{
		if(o_toolbar_min_width.int_value)
			min_x = toolbar_min_width;
		else
			min_x = 200;

		if (filer_window->scrollbar)
			min_x -= filer_window->scrollbar->allocation.width;
	}

	/* Leave some room for extra icons, but only in Small Icons mode
	 * otherwise it takes up too much space.
	 * Also, don't add space if the minibuffer is open.
	 */
	if (space == 0)
		space = filer_window->display_style == SMALL_ICONS ?
			h / (o_filer_auto_resize.int_value != RESIZE_ALWAYS ? 1 : 4) : 1;

	tn = t + space;
	t = tn + 44 /* window decoration and charm. when small then wide */;

	/* Aim for a size where
	 * 	   x = r(y + t + h),		(1)
	 * unless that's too wide.
	 *
	 * t = toolbar (and minibuffer) height
	 * r = desired (width / height) ratio
	 *
	 * Want to display all items:
	 * 	   (x/w)(y/h) = n
	 * 	=> xy = nwh
	 *	=> x(x/r - t - h) = nwh		(from 1)
	 *	=> xx - x.rt - hr(1 - nw) = 0
	 *	=> 2x = rt +/- sqrt(rt.rt + 4hr(nw - 1))
	 * Now,
	 * 	   4hr(nw - 1) > 0
	 * so
	 * 	   sqrt(rt.rt + ...) > rt
	 *
	 * So, the +/- must be +:
	 *
	 *	=> x = (rt + sqrt(rt.rt + 4hr(nw - 1))) / 2
	 */

	x = (r * t + sqrt(r*r*t*t + 4*h*r * (n*w - 1))) / 2 ;
	x = ((x + w / 2) / w) * w;

	x = MIN(x, n * w);

	/* Limit x */
	if (x < min_x)
	{
		if (n * w > min_x)
		{
			cols = MAX(min_x / w, 1);
			x = cols * w;
			if (min_x != x
				&& n % cols
				&& n % cols <= n / cols
				/* Put window decoration away
				 * because in this case, wider is good */
				&& (tn + h * (n / cols)) * (x + w - min_x) < (min_x * h)
			)
				x += w;
		}
		else
			x = n * w;
	}

	if (x > max_x)
		x = max_x;

	cols = x / w;
	cols = MAX(cols, 1);

	collection->columns = cols;
	gtk_widget_queue_resize(GTK_WIDGET(collection));

	rows = MAX((n + cols - 1) / cols, 1);

	GtkRequisition req;
	if (gtk_widget_get_visible(filer_window->thumb_bar))
	{
		gtk_widget_size_request(filer_window->thumb_bar, &req);
		exh += req.height;
	}

	if (gtk_widget_get_visible(filer_window->minibuffer_area))
	{
		gtk_widget_size_request(filer_window->minibuffer_area, &req);
		exh += req.height;
	}

	gtk_widget_get_requisition(filer_window->scrollbar, &req);

	bool inbar = false;

	int vw = MAX(w * MAX(cols, 1), min_x);
	int vh = CLAMP(h * rows + space, MAX(req.height, small_height * 2),
			max_y - (inbar ? exh : 0));
	gboolean notauto = FALSE;

	if (turn && (vw != max_x || vh != max_y) &&
		GTK_WIDGET(view_collection)->allocation.width  == vw &&
		GTK_WIDGET(view_collection)->allocation.height == vh)
	{
		vw = max_x;
		vh = max_y - (inbar ? exh : 0);
		notauto = TRUE;
	}

	filer_window_set_size(filer_window, vw, vh + exh, notauto);
}

static gboolean view_collection_cursor_visible(ViewIface *view)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);
	Collection	*collection = view_collection->collection;

	return collection->cursor_item != -1;
}

static void view_collection_set_base(ViewIface *view, ViewIter *iter)
{
	ViewCollection	*view_collection = VIEW_COLLECTION(view);

	view_collection->cursor_base = iter->i;
}

static void view_collection_start_lasso_box(ViewIface *view,
					    GdkEventButton *event)
{
	ViewCollection	*view_collection = (ViewCollection *) view;
	Collection	*collection = view_collection->collection;

	filer_set_autoscroll(view_collection->filer_window, TRUE);
	collection_lasso_box(collection, event->x, event->y);
}


/* Change the adjustment by this amount. Bounded. */
static void diff_vpos(Collection *collection, int diff)
{
	int	old = collection->vadj->value;
	int	value = old + diff;

	value = CLAMP(value, 0,
			collection->vadj->upper - collection->vadj->page_size);
	gtk_adjustment_set_value(collection->vadj, value);

	if (collection->vadj->value != old)
		dnd_spring_abort();
}

static gboolean view_collection_auto_scroll_callback(ViewIface *view)
{
	ViewCollection	*view_collection = (ViewCollection *) view;
	Collection	*collection = view_collection->collection;
	GdkWindow	*window = ((GtkWidget *) collection)->window;
	gint		x, y, w, h;
	GdkModifierType	mask;
	int		diff = 0;

	gdk_window_get_pointer(window, &x, &y, &mask);
	w = gdk_window_get_width(window);

	h = collection->vadj->page_size;
	y -= collection->vadj->value;

	if ((x < 0 || x > w || y < 0 || y > h) && !collection->lasso_box)
		return FALSE;		/* Out of window - stop */

	int step = MAX(AUTOSCROLL_STEP, h / AUTOSCROLL_AREA);

	if      (y <     step) diff = y - step;
	else if (y > h - step) diff = step + y - h;

	if (diff)
		diff_vpos(collection, diff * AUTOSCROLL_SPEED);

	return TRUE;
}
