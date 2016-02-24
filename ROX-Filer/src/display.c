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

/* display.c - code for arranging and displaying file items */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#include <unistd.h>
#include <time.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>

#include "global.h"

#include "main.h"
#include "filer.h"
#include "display.h"
#include "support.h"
#include "gui_support.h"
#include "pixmaps.h"
#include "menu.h"
#include "dnd.h"
#include "run.h"
#include "mount.h"
#include "type.h"
#include "options.h"
#include "action.h"
#include "minibuffer.h"
#include "dir.h"
#include "diritem.h"
#include "fscache.h"
#include "view_iface.h"
#include "xtypes.h"
#include "usericons.h"

/* Options bits */
static Option o_display_caps_first;
static Option o_display_dirs_first;
Option o_display_size;
Option o_display_details;
Option o_display_sort_by;
Option o_large_width;
Option o_small_width;
Option o_max_length;
Option o_display_show_hidden;
Option o_display_show_thumbs;
Option o_display_show_headers;
Option o_display_show_full_type;
Option o_display_inherit_options;
static Option o_filer_change_size_num;
Option o_vertical_order_small, o_vertical_order_large;
Option o_xattr_show;
static Option o_wrap_by_char;
static Option o_huge_size;
int huge_size = HUGE_SIZE;

/* Static prototypes */
static void display_details_set(FilerWindow *filer_window, DetailsType details);
static void display_style_set(FilerWindow *filer_window, DisplayStyle style);
static void options_changed(void);
static char *getdetails(FilerWindow *filer_window, DirItem *item);
static void display_set_actual_size_real(FilerWindow *filer_window);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void display_init()
{
	option_add_int(&o_display_caps_first, "display_caps_first", FALSE);
	option_add_int(&o_display_dirs_first, "display_dirs_first", FALSE);
	option_add_int(&o_display_size, "display_icon_size", AUTO_SIZE_ICONS);
	option_add_int(&o_display_details, "display_details", DETAILS_NONE);
	option_add_int(&o_display_sort_by, "display_sort_by", SORT_NAME);
	option_add_int(&o_large_width, "display_large_width", 155);
	option_add_int(&o_small_width, "display_small_width", 250);
	option_add_int(&o_max_length, "display_max_length", 0);
	option_add_int(&o_display_show_hidden, "display_show_hidden", FALSE);
	option_add_int(&o_display_show_thumbs, "display_show_thumbs", FALSE);
	option_add_int(&o_display_show_headers, "display_show_headers", TRUE);
	option_add_int(&o_display_show_full_type, "display_show_full_type", FALSE);
	option_add_int(&o_display_inherit_options,
		       "display_inherit_options", FALSE); 
	option_add_int(&o_filer_change_size_num, "filer_change_size_num", 30); 
	option_add_int(&o_vertical_order_small, "vertical_order_small", FALSE);
	option_add_int(&o_vertical_order_large, "vertical_order_large", FALSE);
	option_add_int(&o_xattr_show, "xattr_show", TRUE);
	option_add_int(&o_wrap_by_char, "wrap_by_char", FALSE);
	option_add_int(&o_huge_size, "huge_size", HUGE_SIZE);

	option_add_notify(options_changed);
}

void draw_emblem_on_icon(GdkWindow *window, GtkStyle   *style,
				const char *stock_id,
				int *x, int y,
				GdkColor *color)
{
	GtkIconSet *icon_set;
	GdkPixbuf  *pixbuf, *ctemp;

	icon_set = gtk_style_lookup_icon_set(style,
					     stock_id);
	if (icon_set)
	{
		pixbuf = gtk_icon_set_render_icon(icon_set,
						  style,
						  GTK_TEXT_DIR_LTR,
						  GTK_STATE_NORMAL,
						  mount_icon_size,
						  NULL,
						  NULL);
	}
	else
	{
		pixbuf=im_unknown->pixbuf;
		g_object_ref(pixbuf);
	}

	if (color)
	{
		pixbuf = create_spotlight_pixbuf(ctemp = pixbuf, color);
		g_object_unref(ctemp);
	}

	gdk_pixbuf_render_to_drawable_alpha(pixbuf,
				window,
				0, 0, 				/* src */
				*x, y,		                /* dest */
				-1, -1,
				GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
				GDK_RGB_DITHER_NORMAL, 0, 0);
	
	*x+=gdk_pixbuf_get_width(pixbuf)+1;
	g_object_unref(pixbuf);
}

static void draw_mini_emblem_on_icon(GdkWindow *window,
		GtkStyle *style, const char *stock_id, int *x, int y, GdkColor *color)
{
	GtkIconSet *icon_set;
	static GdkPixbuf  *pixbuf, *ctemp, *scaled = NULL;
	static char *bs_id = NULL;
	static int w, h, tsize, dy, dx;
	static gboolean colored = FALSE;

	/* there are full of sym emblem */
	if (!bs_id || strcmp(bs_id, stock_id) != 0 || colored)
	{
		colored = color != NULL;

		g_free(bs_id);
		bs_id = g_strdup(stock_id);
		if (scaled)
			g_object_unref(scaled);

		icon_set = gtk_style_lookup_icon_set(style,
							 stock_id);
		if (icon_set)
		{
			pixbuf = gtk_icon_set_render_icon(icon_set,
							  style,
							  GTK_TEXT_DIR_LTR,
							  GTK_STATE_NORMAL,
							  mount_icon_size,
							  NULL,
							  NULL);
		}
		else
		{
			pixbuf=im_unknown->pixbuf;
			g_object_ref(pixbuf);
		}

		if (color)
		{
			pixbuf = create_spotlight_pixbuf(ctemp = pixbuf, color);
			g_object_unref(ctemp);
		}

		dy = small_height * 1 / 5;
		tsize = small_height * 4 / 5;
		dy += small_height - (dy + tsize);
		gtk_icon_size_lookup(mount_icon_size, &w, &h);
		if (h < tsize)
		{
			dy += tsize - h;
			scaled = pixbuf;
		}
		else
		{
			scaled = gdk_pixbuf_scale_simple(pixbuf,
						tsize, tsize, GDK_INTERP_TILES);

			g_object_unref(pixbuf);
		}
		dx = gdk_pixbuf_get_width(scaled);
	}

	gdk_draw_pixbuf(window,
				NULL,
				scaled,
				0, 0, /* src */
				*x - 1, y + dy + 1, /* dest */
				-1, -1,
				GDK_RGB_DITHER_NORMAL, 0, 0);

	*x += dx * 2 / 3;
}

static void draw_noimage(GdkWindow *window, GdkRectangle *rect)
{
	cairo_t *cr;
	GdkRectangle dr;

	cr = gdk_cairo_create(window);
	cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

	dr.x      = rect->x + rect->width / 6;
	dr.width  = rect->width / 6 * 4;
	dr.y      = rect->y + rect->height / 6;
	dr.height = rect->height / 6 * 4;
	gdk_cairo_rectangle(cr, &dr);

	cairo_pattern_t *linpat = cairo_pattern_create_linear (
		dr.x + dr.width / 4, dr.y + dr.height / 4,
		dr.x + dr.width, dr.y + dr.height);

	cairo_pattern_add_color_stop_rgb (linpat, 0, 0.1, 0.1, 0.2);
	cairo_pattern_add_color_stop_rgb (linpat, 1, 0.4, 0.4, 0.6);

	cairo_set_line_width (cr, 1.1);

	cairo_set_source (cr, linpat);
	cairo_stroke(cr);
	cairo_destroy(cr);
}

static void draw_label_bg(GdkWindow *window,
			GdkRectangle *rect,
			GdkColor *colour)
{
	cairo_t *cr;
	GdkRectangle drect;
	int b, h;

	if (!colour) return;

	cr = gdk_cairo_create(window);
	gdk_cairo_set_source_color(cr, colour);
	cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

	drect.x      = rect->x;
	drect.width  = rect->width;

	b = rect->y + rect->height + 1;
	h = 2;
	drect.y      = b - h;
	drect.height = h;
	gdk_cairo_rectangle(cr, &drect);

	b = b - h - 1;
	h = 1;
	drect.y      = b - h;
	drect.height = h;
	gdk_cairo_rectangle(cr, &drect);

	cairo_fill(cr);

	cairo_destroy(cr);
}

/* Draw this icon (including any symlink or mount symbol) inside the
 * given rectangle.
 */
void draw_huge_icon(
			GdkWindow *window,
			GtkStyle *style,
			GdkRectangle *area,
			DirItem  *item,
			GdkPixbuf *image,
			gboolean selected,
			GdkColor *color
) {
	int       image_x, image_y, width, height;
	GdkPixbuf *pixbuf, *scaled = NULL;
	gfloat    scale;

	if (!image)
		return draw_noimage(window, area);

	width = gdk_pixbuf_get_width(image);
	height = gdk_pixbuf_get_height(image);

	scale = MIN(area->width / (gfloat) width, area->height / (gfloat) height);

	width *= scale;
	height *= scale;

	if (scale != 1.0)
		scaled = gdk_pixbuf_scale_simple(image,
					width, height, GDK_INTERP_BILINEAR);
	else
		scaled = image;

	image_x = area->x + ((area->width - width) >> 1);
	image_y = area->y + MAX(0, area->height - height - 6);

	draw_label_bg(window, area,
			selected && item->label ? color : item->label);

	 pixbuf = selected
			? create_spotlight_pixbuf(scaled, color)
			: scaled;

	gdk_pixbuf_render_to_drawable_alpha(
			pixbuf,
			window,
			0, 0, 				/* src */
			image_x, image_y,	/* dest */
			width, height,
			GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
			GDK_RGB_DITHER_NORMAL, 0, 0);

	if (scale != 1.0)
		g_object_unref(scaled);

	if (selected)
		g_object_unref(pixbuf);


	if (area->width <= small_width && area->height <= small_height)
	{
		image_x -= (small_width - width) / 2;

		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			const char *mp = item->flags & ITEM_FLAG_MOUNTED
						? ROX_STOCK_MOUNTED
						: ROX_STOCK_MOUNT;
			draw_mini_emblem_on_icon(window, style, mp,
						&image_x, area->y, NULL);
		}
		if (item->flags & ITEM_FLAG_SYMLINK)
		{
			draw_mini_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
						&image_x, area->y, NULL);
		}
		if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
		{
			draw_mini_emblem_on_icon(window, style, ROX_STOCK_XATTR,
						&image_x, area->y, item->label);
		}
	}
	else if (area->width <= ICON_WIDTH && area->height <= ICON_HEIGHT)
	{
		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			const char *mp = item->flags & ITEM_FLAG_MOUNTED
						? ROX_STOCK_MOUNTED
						: ROX_STOCK_MOUNT;
			draw_emblem_on_icon(window, style, mp,
						&image_x, area->y + 2, NULL);
		}
		if (item->flags & ITEM_FLAG_SYMLINK)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
						&image_x, area->y + 2, NULL);
		}
		if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
						&image_x, area->y + 2, item->label);
		}
	}
	else
	{
		image_x += width / 19;
		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			const char *mp = item->flags & ITEM_FLAG_MOUNTED
						? ROX_STOCK_MOUNTED
						: ROX_STOCK_MOUNT;
			draw_emblem_on_icon(window, style, mp,
						&image_x, area->y + height / 19, NULL);
		}
		if (item->flags & ITEM_FLAG_SYMLINK)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
						&image_x, area->y + height / 19, NULL);
		}
		if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
						&image_x, area->y + height / 19, item->label);
		}
	}
}

/* The sort functions aren't called from outside, but they are
 * passed as arguments to display_set_sort_fn().
 */

#define IS_A_DIR(item) (item->base_type == TYPE_DIRECTORY && \
			!(item->flags & ITEM_FLAG_APPDIR))

#define SORT_DIRS	\
	if (o_display_dirs_first.int_value) {	\
		gboolean id1 = IS_A_DIR(i1);	\
		gboolean id2 = IS_A_DIR(i2);	\
		if (id1 && !id2) return -1;				\
		if (id2 && !id1) return 1;				\
	}

int sort_by_name(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	CollateKey *n1 = i1->leafname_collate;
	CollateKey *n2 = i2->leafname_collate;
	int retval;

	SORT_DIRS;

	retval = collate_key_cmp(n1, n2, o_display_caps_first.int_value);

	return retval ? retval : strcmp(i1->leafname, i2->leafname);
}

int sort_by_type(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	MIME_type *m1, *m2;

	int	 diff = i1->base_type - i2->base_type;

	if (!diff)
		diff = (i1->flags & ITEM_FLAG_APPDIR)
		     - (i2->flags & ITEM_FLAG_APPDIR);
	if (diff)
		return diff > 0 ? 1 : -1;

	m1 = i1->mime_type;
	m2 = i2->mime_type;
	
	if (m1 && m2)
	{
		diff = strcmp(m1->media_type, m2->media_type);
		if (!diff)
			diff = strcmp(m1->subtype, m2->subtype);
	}
	else if (m1 || m2)
		diff = m1 ? 1 : -1;
	else
		diff = 0;

	if (diff)
		return diff > 0 ? 1 : -1;
	
	return sort_by_name(item1, item2);
}

int sort_by_owner(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	const gchar *name1;
	const gchar *name2;

	if(i1->uid==i2->uid)
		return sort_by_name(item1, item2);

	name1=user_name(i1->uid);
	name2=user_name(i2->uid);

	return strcmp(name1, name2);
}

int sort_by_group(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;
	const gchar *name1;
	const gchar *name2;

	if(i1->gid==i2->gid)
		return sort_by_name(item1, item2);

	name1=group_name(i1->gid);
	name2=group_name(i2->gid);

	return strcmp(name1, name2);
}

int sort_by_date(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;

	/* SORT_DIRS; -- too confusing! */

	return i1->mtime < i2->mtime ? -1 :
		i1->mtime > i2->mtime ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_size(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;

	SORT_DIRS;

	return i1->size < i2->size ? -1 :
		i1->size > i2->size ? 1 :
		sort_by_name(item1, item2);
}

void display_set_sort_type(FilerWindow *filer_window, SortType sort_type,
			   GtkSortType order)
{
	if (filer_window->sort_type == sort_type &&
	    filer_window->sort_order == order)
		return;

	filer_window->sort_type = sort_type;
	filer_window->sort_order = order;

	view_sort(filer_window->view);
}

/* Change the icon size and style.
 * force_resize should only be TRUE for new windows.
 */
void display_set_layout(FilerWindow  *filer_window,
			DisplayStyle style,
			DetailsType  details,
			gboolean     force_resize)
{
	gboolean style_changed = FALSE, details_changed;
	DisplayStyle real_style = filer_window->display_style;

	g_return_if_fail(filer_window != NULL);

	if (style == AUTO_SIZE_ICONS)
		filer_window->icon_scale = 1.0;

	details_changed = filer_window->details_type != details;
	style_changed = details_changed ||
				filer_window->display_style_wanted != style;
	  
	display_style_set(filer_window, style);
	display_details_set(filer_window, details);

	if (details_changed || real_style != filer_window->display_style)
		view_style_changed(filer_window->view, VIEW_UPDATE_NAME);
	else
		/* Recreate layouts because wrapping may have changed */
		view_style_changed(filer_window->view, 0);

	if (force_resize || o_filer_auto_resize.int_value == RESIZE_ALWAYS
	    || (o_filer_auto_resize.int_value == RESIZE_STYLE && style_changed))
	{
		view_autosize(filer_window->view);
	}

	if (filer_window->toolbar_size_text)
	{
		gchar *size_label = g_strdup_printf("%s%s", N_("Size"),
			filer_window->display_style_wanted == LARGE_ICONS ? "┤" :
			filer_window->display_style_wanted == SMALL_ICONS ? "┐" :
			filer_window->display_style_wanted == HUGE_ICONS  ? "┘" :
			filer_window->display_style_wanted == AUTO_SIZE_ICONS ?
				filer_window->display_style == LARGE_ICONS ? "├" : "┌" :
			"┼" );
		gtk_label_set_text(filer_window->toolbar_size_text, size_label);

		g_free(size_label);
	}
}

/* Set the 'Show Thumbnails' flag for this window */
void display_set_thumbs(FilerWindow *filer_window, gboolean thumbs)
{
	if (filer_window->show_thumbs == thumbs)
		return;

	filer_window->show_thumbs = thumbs;

	view_style_changed(filer_window->view, VIEW_UPDATE_VIEWDATA);

	if (!thumbs)
		filer_cancel_thumbnails(filer_window);

	filer_set_title(filer_window);

	filer_create_thumbs(filer_window);
}

void display_update_hidden(FilerWindow *filer_window)
{
	filer_detach_rescan(filer_window);	/* (updates titlebar) */

	display_set_actual_size(filer_window, FALSE);
}

/* Set the 'Show Hidden' flag for this window */
void display_set_hidden(FilerWindow *filer_window, gboolean hidden)
{
	if (filer_window->show_hidden == hidden)
		return;

	/*
	filer_window->show_hidden = hidden;
	*/
	filer_set_hidden(filer_window, hidden);

	display_update_hidden(filer_window);
}

/* Set the 'Filter Directories' flag for this window */
void display_set_filter_directories(FilerWindow *filer_window, gboolean filter_directories)
{
	if (filer_window->filter_directories == filter_directories)
		return;

	/*
	filer_window->show_hidden = hidden;
	*/
	filer_set_filter_directories(filer_window, filter_directories);

	display_update_hidden(filer_window);
}

void display_set_filter(FilerWindow *filer_window, FilterType type,
			const gchar *filter_string)
{
	if (filer_set_filter(filer_window, type, filter_string))
		display_update_hidden(filer_window);
}


/* Highlight (wink or cursor) this item in the filer window. If the item
 * isn't already there but we're scanning then highlight it if it
 * appears later.
 */
void display_set_autoselect(FilerWindow *filer_window, const gchar *leaf)
{
	gchar *new;
	
	g_return_if_fail(filer_window != NULL);
	g_return_if_fail(leaf != NULL);

	new = g_strdup(leaf);	/* leaf == old value sometimes */

	null_g_free(&filer_window->auto_select);

	if (view_autoselect(filer_window->view, new))
		g_free(new);
	else
		filer_window->auto_select = new;
}

/* Change the icon size (wraps) */
void display_change_size(FilerWindow *filer_window, gboolean bigger)
{
	DisplayStyle	new = SMALL_ICONS;

	g_return_if_fail(filer_window != NULL);

	switch (filer_window->display_style)
	{
		case LARGE_ICONS:
			new = bigger ? HUGE_ICONS : SMALL_ICONS;
			break;
		case HUGE_ICONS:
			if (!bigger)
				new = LARGE_ICONS;
			else if (filer_window->icon_scale != 1.0)
			{
				/* icon scale skiped this */
				filer_window->display_style_wanted = AUTO_SIZE_ICONS;
				new = HUGE_ICONS;
			}
			else
				return;
			break;
		default:
			if (bigger)
				new = LARGE_ICONS;
			else if (filer_window->icon_scale != 1.0)
				/* icon scale skiped this */
				filer_window->display_style_wanted = AUTO_SIZE_ICONS;
			else
				return;
			break;
	}

	/* scale is just temporary */
	filer_window->icon_scale = 1.0;

	display_set_layout(filer_window, new, filer_window->details_type,
			   FALSE);
}

ViewData *display_create_viewdata(FilerWindow *filer_window, DirItem *item)
{
	ViewData *view;

	view = g_new(ViewData, 1);

	view->details = NULL;
	view->image = NULL;
	view->thumb = NULL;

	display_update_view(filer_window, item, view, TRUE);

	return view;
}

/* Set the display style to the desired style. If the desired style
 * is AUTO_SIZE_ICONS, choose an appropriate size. Also resizes filer
 * window, if requested.
 */
void display_set_actual_size(FilerWindow *filer_window, gboolean force_resize)
{
	display_set_layout(filer_window, filer_window->display_style_wanted,
			   filer_window->details_type, force_resize);
}


/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void options_changed(void)
{
	GList		*next;
	int flags = 0;

	huge_size = o_huge_size.int_value;

	if (o_display_show_headers.has_changed)
		flags |= VIEW_UPDATE_HEADERS;

	if (o_large_width.has_changed ||
		o_small_width.has_changed ||
		o_max_length.has_changed ||
		o_wrap_by_char.has_changed ||
		o_huge_size.has_changed
	)
		flags |= VIEW_UPDATE_NAME; /* Recreate PangoLayout */

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		if (o_display_dirs_first.has_changed ||
		    o_display_caps_first.has_changed)
			view_sort(VIEW(filer_window->view));

		view_style_changed(filer_window->view, flags);

		if (o_filer_auto_resize.int_value == RESIZE_ALWAYS)
			view_autosize(filer_window->view);
	}
}

/* Return a new string giving details of this item, or NULL if details
 * are not being displayed. If details are not yet available, return
 * a string of the right length.
 */
static char *getdetails(FilerWindow *filer_window, DirItem *item)
{
	mode_t	m = item->mode;
	guchar 	*buf = NULL;
	gboolean scanned = item->base_type != TYPE_UNKNOWN;
	gboolean vertical = filer_window->display_style == HUGE_ICONS;

	if (scanned && item->lstat_errno)
		buf = g_strdup_printf(_("lstat(2) failed: %s"),
				g_strerror(item->lstat_errno));
	else if (filer_window->details_type == DETAILS_TYPE)
	{
		MIME_type	*type = item->mime_type;

		if (!scanned)
			return g_strdup("application/octet-stream");

		buf = g_strdup_printf("%s/%s",
				      type->media_type, type->subtype);
	}
	else if (filer_window->details_type == DETAILS_TIMES)
	{
		guchar	*ctime, *mtime, *atime;
		
		ctime = pretty_time(&item->ctime);
		mtime = pretty_time(&item->mtime);
		atime = pretty_time(&item->atime);
		if (vertical)
			buf = g_strdup_printf("a[%s]\nc[%s]\nm[%s]", atime, ctime, mtime);
		else
			buf = g_strdup_printf("a[%s] c[%s] m[%s]", atime, ctime, mtime);
		g_free(ctime);
		g_free(mtime);
		g_free(atime);
	}
	else if (filer_window->details_type == DETAILS_PERMISSIONS)
	{
		if (!scanned) {
			if (vertical)
				return g_strdup("---,---,---/--"
#ifdef S_ISVTX
					"-"
#endif
					" 12345678 12345678");
			else
				return g_strdup("---,---,---/--"
#ifdef S_ISVTX
					"-"
#endif
					"\n 12345678 12345678");
		}

		if (vertical)
			buf = g_strdup_printf("%s\n%-8.8s %-8.8s",
					pretty_permissions(m),
					user_name(item->uid),
					group_name(item->gid));
		else
			buf = g_strdup_printf("%s %-8.8s %-8.8s",
					pretty_permissions(m),
					user_name(item->uid),
					group_name(item->gid));

	}
	else
	{
		if (!scanned)
		{
			if (filer_window->display_style == SMALL_ICONS)
				return g_strdup("1234M");
			else
				return g_strdup("1234 bytes");
		}

		if (item->base_type != TYPE_DIRECTORY)
		{
			if (filer_window->display_style == SMALL_ICONS)
				buf = g_strdup(format_size_aligned(item->size));
			else
				buf = g_strdup(format_size(item->size));
		}
		else
			buf = g_strdup("-");
	}
		
	return buf;
}

/* Note: Call style_changed after this */
static void display_details_set(FilerWindow *filer_window, DetailsType details)
{
	filer_window->details_type = details;
}

/* Note: Call style_changed after this */
static void display_style_set(FilerWindow *filer_window, DisplayStyle style)
{
	filer_window->display_style_wanted = style;
	display_set_actual_size_real(filer_window);
}

PangoLayout *make_layout(FilerWindow *fw, DirItem *item)
{
	DisplayStyle style = fw->display_style;
	int	wrap_width = -1;
	PangoLayout *ret;
	PangoAttrList *list = NULL;

	if (g_utf8_validate(item->leafname, -1, NULL))
	{
		ret = gtk_widget_create_pango_layout(
				fw->window, item->leafname);
		pango_layout_set_auto_dir(ret, FALSE);
	}
	else
	{
		PangoAttribute	*attr;
		gchar *utf8;

		utf8 = to_utf8(item->leafname);
		ret = gtk_widget_create_pango_layout(
				fw->window, utf8);
		g_free(utf8);

		attr = pango_attr_foreground_new(0xffff, 0, 0);
		attr->start_index = 0;
		attr->end_index = -1;

		list = pango_attr_list_new();
		pango_attr_list_insert(list, attr);

	}

	if (item->flags & ITEM_FLAG_RECENT)
	{
		PangoAttribute	*attr;

		attr = pango_attr_weight_new(PANGO_WEIGHT_BOLD);
		attr->start_index = 0;
		attr->end_index = -1;
		if (!list)
			list = pango_attr_list_new();
		pango_attr_list_insert(list, attr);
	}

	if (list)
	{
		pango_layout_set_attributes(ret, list);
		pango_attr_list_unref(list);
	}

	if (style == HUGE_ICONS)
		wrap_width = MAX(huge_size, o_large_width.int_value) * PANGO_SCALE;
		/* Since this function is heavy, this is skepped.
		wrap_width = HUGE_WRAP * filer_window->icon_scale * PANGO_SCALE;
		*/

	if (fw->details_type == DETAILS_NONE && style == LARGE_ICONS)
		wrap_width = o_large_width.int_value * PANGO_SCALE;

	if (wrap_width != -1)
	{
		if (o_wrap_by_char.int_value)
			pango_layout_set_wrap(ret, PANGO_WRAP_CHAR);
		else {
#ifdef USE_PANGO_WRAP_WORD_CHAR
			pango_layout_set_wrap(ret, PANGO_WRAP_WORD_CHAR);
#endif
		}

		pango_layout_set_width(ret, wrap_width);
	}

	return ret;
}
/* Each displayed item has a ViewData structure with some cached information
 * to help quickly draw the item (eg, the PangoLayout). This function updates
 * this information.
 */
void display_update_view(FilerWindow *filer_window,
			 DirItem *item,
			 ViewData *view,
			 gboolean update_name_layout)
{
	int	w, h;
	char	*str;
	static PangoFontDescription *monospace = NULL;
	gboolean basic = o_fast_font_calc.int_value;

	if (filer_window->details_type != DETAILS_NONE)
	{
		if (!monospace)
			monospace = pango_font_description_from_string("monospace");

		if (view->details)
		{
			g_object_unref(G_OBJECT(view->details));
			view->details = NULL;
		}

		str = getdetails(filer_window, item);
		if (str)
		{
			PangoAttrList	*details_list;
			int	perm_offset = -1;

			view->details = gtk_widget_create_pango_layout(
					filer_window->window, str);
			g_free(str);

			pango_layout_set_font_description(view->details, monospace);
			pango_layout_get_size(view->details, &w, &h);
			view->details_width = w / PANGO_SCALE;
			view->details_height = h / PANGO_SCALE;

			if (filer_window->details_type == DETAILS_PERMISSIONS)
				perm_offset = 0;
			if (perm_offset > -1)
			{
				PangoAttribute	*attr;

				attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);

				perm_offset += 4 * applicable(item->uid, item->gid);
				attr->start_index = perm_offset;
				attr->end_index = perm_offset + 3;

				details_list = pango_attr_list_new();
				pango_attr_list_insert(details_list, attr);
				pango_layout_set_attributes(view->details,
						details_list);

				pango_attr_list_unref(details_list);
			}
		}
	}

	if (item->base_type != TYPE_UNKNOWN) {
		if (view->image)
		{
			g_object_unref(view->image);
			view->image = NULL;
		}

		view->image = get_globicon(
				make_path(filer_window->sym_path, item->leafname));

		if (!view->image)
		{
			const guchar *path = make_path(filer_window->real_path, item->leafname);

			view->image = get_globicon(path);

			//.DirIcon
			if (!view->image && filer_window->show_thumbs &&
					item->base_type == TYPE_FILE)
				view->image = g_fscache_lookup_full(pixmap_cache, path,
						FSCACHE_LOOKUP_ONLY_NEW, NULL);

			if (!view->image &&
				(item->base_type != TYPE_FILE || !filer_window->show_thumbs))
			{
				view->image = di_image(item);
				if (view->image) {
					g_object_ref(view->image);
				}
			}
		}
	}

	if (!update_name_layout) return;

	basic &= !(item->flags & ITEM_FLAG_RECENT);
	basic &= (filer_window->display_style == SMALL_ICONS ||
				(filer_window->details_type != DETAILS_NONE &&
				 filer_window->display_style != HUGE_ICONS));

	if (basic)
	{
		h = fw_font_height;
		w = 0;
		gchar *name = item->leafname;

		for (; *name; name++)
			if (*name >= 0x20 && *name <= 0x7e)
				w += fw_font_widths[(gint) *name];
			else
			{
				basic = FALSE;
				break;
			}
	}

	if (!basic)
	{
		PangoLayout *layout = make_layout(filer_window, item);

		pango_layout_get_size(layout, &w, &h);

		g_object_unref(G_OBJECT(layout));
	}

	view->name_width = w / PANGO_SCALE;
	view->name_height = h / PANGO_SCALE;
}

/* Sets display_style from display_style_wanted.
 * See also display_set_actual_size().
 */
static void display_set_actual_size_real(FilerWindow *filer_window)
{
	DisplayStyle size = filer_window->display_style_wanted;
	int n;
	
	g_return_if_fail(filer_window != NULL);

	if (size == AUTO_SIZE_ICONS)
	{
		n = view_count_items(filer_window->view);

		if (n >= o_filer_change_size_num.int_value)
			size = SMALL_ICONS;
		else
			size = LARGE_ICONS;
	}
	
	filer_window->display_style = size;
}
