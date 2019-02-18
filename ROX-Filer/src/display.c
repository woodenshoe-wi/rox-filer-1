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
#include "view_iface.h"
#include "xtypes.h"

/* Options bits */
static Option o_display_caps_first;
Option o_display_dirs_first;
static Option o_display_newly_first;
Option o_display_size;
Option o_display_details;
Option o_display_sort_by;
Option o_large_width;
Option o_small_width;
Option o_max_length;
Option o_display_show_hidden;
Option o_display_show_thumbs;
Option o_display_show_dir_thumbs;
Option o_display_show_headers;
Option o_display_show_full_type;
Option o_display_less_clickable_cols;
Option o_display_name_width;
Option o_display_show_name;
Option o_display_show_type;
Option o_display_show_size;
Option o_display_show_permissions;
Option o_display_show_owner;
Option o_display_show_group;
Option o_display_show_mtime;
Option o_display_show_ctime;
Option o_display_show_atime;
Option o_display_save_col_order;
Option o_display_inherit_options;
static Option o_filer_change_size_num;
Option o_vertical_order_small, o_vertical_order_large;
Option o_xattr_show;
Option o_view_alpha;
Option o_use_background_colour;
Option o_background_colour;

static Option o_wrap_by_char;
static Option o_huge_size;
int huge_size = HUGE_SIZE;

/* Static prototypes */
static void options_changed(void);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void display_init()
{
	option_add_int(&o_display_caps_first, "display_caps_first", FALSE);
	option_add_int(&o_display_dirs_first, "display_dirs_first", FALSE);
	option_add_int(&o_display_newly_first, "display_newly_first", FALSE);

	option_add_int(&o_display_inherit_options,
		       "display_inherit_options", FALSE);
	option_add_int(&o_display_sort_by, "display_sort_by", SORT_NAME);
	option_add_int(&o_display_show_hidden, "display_show_hidden", TRUE);
	option_add_int(&o_xattr_show, "xattr_show", TRUE);
	option_add_int(&o_huge_size, "huge_size", HUGE_SIZE);

	option_add_int(&o_display_size, "display_icon_size", AUTO_SIZE_ICONS);
	option_add_int(&o_display_details, "display_details", DETAILS_NONE);
	option_add_int(&o_filer_change_size_num, "filer_change_size_num", 90);

	option_add_int(&o_large_width, "display_large_width", 120);
	option_add_int(&o_max_length, "display_max_length", 360);
	option_add_int(&o_wrap_by_char, "wrap_by_char", FALSE);
	option_add_int(&o_small_width, "display_small_width", 160);

	option_add_int(&o_view_alpha, "view_alpha", 22);

	option_add_int(&o_vertical_order_small, "vertical_order_small", TRUE);
	option_add_int(&o_vertical_order_large, "vertical_order_large", FALSE);
	option_add_int(&o_display_show_headers, "display_show_headers", TRUE);
	option_add_int(&o_display_show_full_type, "display_show_full_type", FALSE);

	option_add_int(&o_display_show_thumbs, "display_show_thumbs", FALSE);
	option_add_int(&o_display_show_dir_thumbs, "display_show_dir_thumbs", TRUE);

	option_add_int(&o_use_background_colour, "use_background_colour", FALSE);
	option_add_string(&o_background_colour, "background_colour", "#000000");
	option_add_int(&o_display_less_clickable_cols, "display_less_clickable_cols", FALSE);

	option_add_int(&o_display_name_width, "display_name_width", 0);

	option_add_int(&o_display_show_name, "display_show_name", TRUE);
	option_add_int(&o_display_show_type, "display_show_type", TRUE);
	option_add_int(&o_display_show_size, "display_show_size", TRUE);
	option_add_int(&o_display_show_permissions, "display_show_permissions", TRUE);
	option_add_int(&o_display_show_owner, "display_show_owner", TRUE);
	option_add_int(&o_display_show_group, "display_show_group", TRUE);
	option_add_int(&o_display_show_mtime, "display_show_mtime", TRUE);
	option_add_int(&o_display_show_ctime, "display_show_ctime", TRUE);
	option_add_int(&o_display_show_atime, "display_show_atime", FALSE);

	option_add_int(&o_display_save_col_order, "display_save_col_order", TRUE);

	option_add_notify(options_changed);

	huge_size = o_huge_size.int_value;
}

void draw_emblem_on_icon(GdkWindow *window, GtkStyle   *style,
				const char *stock_id,
				int *x, int y,
				GdkColor *colour)
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

	if (colour)
	{
		pixbuf = create_spotlight_pixbuf(ctemp = pixbuf, colour);
		g_object_unref(ctemp);
	}

	cairo_t *cr = gdk_cairo_create(window);
	gdk_cairo_set_source_pixbuf(cr, pixbuf, *x, y);
	cairo_paint(cr);
	cairo_destroy(cr);

	*x+=gdk_pixbuf_get_width(pixbuf)+1;
	g_object_unref(pixbuf);
}

static void draw_mini_emblem_on_icon(cairo_t *cr,
		GtkStyle *style, const char *stock_id,
		int *x, int y, int areah, GdkColor *colour)
{
	GtkIconSet *icon_set;
	static GdkPixbuf  *pixbuf, *ctemp, *scaled = NULL;
	static char *bs_id = NULL;
	static int w, h, tsize, dy, dx;
	static gboolean coloured = FALSE;

	/* there are full of sym emblem */
	if (!bs_id || strcmp(bs_id, stock_id) != 0 || coloured)
	{
		coloured = colour != NULL;

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

		if (colour)
		{
			pixbuf = create_spotlight_pixbuf(ctemp = pixbuf, colour);
			g_object_unref(ctemp);
		}

		dy = areah * 1 / 5;
		tsize = areah * 4 / 5;
		dy += areah - (dy + tsize);
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

	gdk_cairo_set_source_pixbuf(cr, scaled, *x - 1, y + dy + 1);
	cairo_paint(cr);

	*x += dx * 2 / 3;
}

static void draw_noimage(GdkWindow *window, GdkRectangle *rect)
{
	cairo_t *cr;
	GdkRectangle dr;

	cr = gdk_cairo_create(window);
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	dr.x      = rect->x + rect->width / 6;
	dr.width  = rect->width / 6 * 4;
	dr.y      = rect->y + rect->height / 6;
	dr.height = rect->height / 6 * 4;
	gdk_cairo_rectangle(cr, &dr);

	cairo_pattern_t *linpat = cairo_pattern_create_linear(
		dr.x + dr.width / 4, dr.y + dr.height / 4,
		dr.x + dr.width, dr.y + dr.height);

	cairo_pattern_add_color_stop_rgb(linpat, 0, 0.1, 0.1, 0.2);
	cairo_pattern_add_color_stop_rgb(linpat, 1, 0.4, 0.4, 0.6);

	cairo_set_line_width(cr, 2.0);

	cairo_set_source(cr, linpat);
	cairo_stroke(cr);

	cairo_pattern_destroy(linpat);
	cairo_destroy(cr);
}

static void draw_label_bg(cairo_t *cr,
			GdkRectangle *rect,
			GdkColor *colour)
{
	GdkRectangle drect;
	int b, h;

	if (!colour) return;

	gdk_cairo_set_source_color(cr, colour);

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
			GdkColor *colour
) {
	int       image_x, image_y, width, height, mw, mh;
	GdkPixbuf *pixbuf, *scaled = NULL;
	gfloat    scale, ws, hs;

	if (!image)
		return draw_noimage(window, area);

	width = gdk_pixbuf_get_width(image);
	height = gdk_pixbuf_get_height(image);

	ws = area->width / (gfloat) width;
	hs = area->height / (gfloat) height;
	scale = MAX(ws, hs);
	width *= scale;
	height *= scale;
	if (area->height < height || area->width < width)
	{
		scale = MIN(ws, hs);
		width = gdk_pixbuf_get_width(image) * scale;
		height = gdk_pixbuf_get_height(image) * scale;;
	}

	if (scale != 1.0 && width > 0 && height > 0)
		scaled = gdk_pixbuf_scale_simple(image,
					width, height, GDK_INTERP_BILINEAR);
	else
		scaled = image;

	image_x = area->x + ((area->width - width) >> 1);
	image_y = area->y + MAX(0, (area->height - height) / 2);

	cairo_t *cr = gdk_cairo_create(window);

	draw_label_bg(cr, area,
			selected && item->label ? colour : item->label);

	 pixbuf = selected
			? create_spotlight_pixbuf(scaled, colour)
			: scaled;

	gdk_cairo_set_source_pixbuf(cr, pixbuf, image_x, image_y);
	cairo_paint(cr);

	if (scale != 1.0 && width > 0 && height > 0)
		g_object_unref(scaled);

	if (selected)
		g_object_unref(pixbuf);

	gtk_icon_size_lookup(mount_icon_size, &mw, &mh);

	image_x = area->x;
	if (area->width < mw * 2 && area->height < mh * 2)
	{
		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			const char *mp = item->flags & ITEM_FLAG_MOUNTED
						? ROX_STOCK_MOUNTED
						: ROX_STOCK_MOUNT;
			draw_mini_emblem_on_icon(cr, style, mp,
						&image_x, area->y, area->height, NULL);
		}
		if (item->flags & ITEM_FLAG_SYMLINK)
		{
			draw_mini_emblem_on_icon(cr, style, ROX_STOCK_SYMLINK,
						&image_x, area->y, area->height, NULL);
		}
		if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
		{
			draw_mini_emblem_on_icon(cr, style, ROX_STOCK_XATTR,
						&image_x, area->y, area->height, item->label);
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

		int emb_y = MIN(image_y, area->y + area->height * 1 / 3);

		if (item->flags & ITEM_FLAG_MOUNT_POINT)
		{
			const char *mp = item->flags & ITEM_FLAG_MOUNTED
						? ROX_STOCK_MOUNTED
						: ROX_STOCK_MOUNT;
			draw_emblem_on_icon(window, style, mp,
						&image_x, emb_y + height / 19, NULL);
		}
		if (item->flags & ITEM_FLAG_SYMLINK)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_SYMLINK,
						&image_x, emb_y + height / 19, NULL);
		}
		if ((item->flags & ITEM_FLAG_HAS_XATTR) && o_xattr_show.int_value)
		{
			draw_emblem_on_icon(window, style, ROX_STOCK_XATTR,
						&image_x, emb_y + height / 19, item->label);
		}
	}

	cairo_destroy(cr);
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
	int retval;

	SORT_DIRS;

	if (o_display_caps_first.int_value)
	{
		if ((i1->flags & ITEM_FLAG_CAPS) && !(i2->flags & ITEM_FLAG_CAPS))
			return -1;
		else
		if ((i2->flags & ITEM_FLAG_CAPS) && !(i1->flags & ITEM_FLAG_CAPS))
			return 1;
	}

	retval = strcmp(i1->collatekey, i2->collatekey);

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

int sort_by_datea(const void *item1, const void *item2)
{
	const DirItem *i1, *i2;
	if (o_display_newly_first.int_value)
	{
		i1 = item2;
		i2 = item1;
	} else {
		i1 = item1;
		i2 = item2;
	}

	/* SORT_DIRS; -- too confusing! */

	return i1->atime < i2->atime ? -1 :
		i1->atime > i2->atime ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_datec(const void *item1, const void *item2)
{
	const DirItem *i1, *i2;
	if (o_display_newly_first.int_value)
	{
		i1 = item2;
		i2 = item1;
	} else {
		i1 = item1;
		i2 = item2;
	}
	return i1->ctime < i2->ctime ? -1 :
		i1->ctime > i2->ctime ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_datem(const void *item1, const void *item2)
{
	const DirItem *i1, *i2;
	if (o_display_newly_first.int_value)
	{
		i1 = item2;
		i2 = item1;
	} else {
		i1 = item1;
		i2 = item2;
	}
	return i1->mtime < i2->mtime ? -1 :
		i1->mtime > i2->mtime ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_size(const void *item1, const void *item2)
{
	const DirItem *i1 = (DirItem *) item1;
	const DirItem *i2 = (DirItem *) item2;

	if (i1->base_type != i2->base_type)
	{
		if (i1->base_type == TYPE_DIRECTORY)
			return o_display_dirs_first.int_value ? -1 : 1;
		if (i2->base_type == TYPE_DIRECTORY)
			return o_display_dirs_first.int_value ? 1 : -1;
	}

	return i1->size < i2->size ? -1 :
		i1->size > i2->size ? 1 :
		sort_by_name(item1, item2);
}

int sort_by_perm(const void *item1, const void *item2)
{
#define S_ALL (S_ISUID | S_ISGID | S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO)
	guint i1 = ((DirItem *)item1)->mode & S_ALL;
	guint i2 = ((DirItem *)item2)->mode & S_ALL;
#undef S_ALL

	return i1 < i2 ? -1 : i1 > i2 ? 1 : sort_by_name(item1, item2);
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
void display_set_layout(FilerWindow *fw,
			DisplayStyle want,
			DetailsType  details,
			gboolean     force_resize)
{
	gdk_window_set_cursor(fw->window->window, busy_cursor);
	gdk_flush();

	gboolean details_changed = fw->details_type != details;
	gboolean wanted_changed = details_changed || fw->display_style_wanted != want;
	DisplayStyle prev_style = fw->display_style;

	fw->details_type = details;
	fw->display_style = fw->display_style_wanted = want;

	if (want == AUTO_SIZE_ICONS)
	{
		fw->display_style =
			view_count_items(fw->view) >= o_filer_change_size_num.int_value ?
				SMALL_ICONS : LARGE_ICONS;

		fw->icon_scale = 1.0;
	}

	if (details_changed || prev_style != fw->display_style)
		view_style_changed(fw->view, VIEW_UPDATE_NAME);
	else
		view_style_changed(fw->view, 0);

	if (force_resize ||
			o_filer_auto_resize.int_value == RESIZE_ALWAYS ||
			(o_filer_auto_resize.int_value == RESIZE_STYLE && wanted_changed)
			)
		view_autosize(fw->view, FALSE);

	if (fw->toolbar_size_text)
	{
		gchar *size_label = g_strdup_printf("%s%s", _("Size"),
			want == LARGE_ICONS ? _("┤") :
			want == SMALL_ICONS ? _("┐") :
			want == HUGE_ICONS  ? _("┘") :
			want == AUTO_SIZE_ICONS ?
				fw->display_style == LARGE_ICONS ? _("├") : _("┌")
			: _("┼"));
		gtk_label_set_text(fw->toolbar_size_text, size_label);

		g_free(size_label);
	}

	gdk_window_set_cursor(fw->window->window, NULL);
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

	filer_create_thumbs(filer_window, NULL);
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

	if (filer_window->view_type == VIEW_TYPE_DETAILS &&
		filer_window->display_style_wanted == AUTO_SIZE_ICONS
		)
		new = bigger ? LARGE_ICONS : SMALL_ICONS;
	else
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
				else if (filer_window->display_style_wanted == AUTO_SIZE_ICONS)
					new = SMALL_ICONS;
				else
					return;
				break;
		}

	/* scale is just temporary */
	filer_window->icon_scale = 1.0;

	display_set_layout(filer_window, new, filer_window->details_type,
			   FALSE);
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

	if (o_display_show_headers.has_changed
			|| o_display_name_width.has_changed
			|| o_display_show_name.has_changed
			|| o_display_show_type.has_changed
			|| o_display_show_size.has_changed
			|| o_display_show_permissions.has_changed
			|| o_display_show_owner.has_changed
			|| o_display_show_group.has_changed
			|| o_display_show_mtime.has_changed
			|| o_display_show_ctime.has_changed
			|| o_display_show_atime.has_changed
	   )
		flags |= VIEW_UPDATE_HEADERS;

	if (o_large_width.has_changed ||
		o_small_width.has_changed ||
		o_max_length.has_changed ||
		o_wrap_by_char.has_changed ||
		o_huge_size.has_changed
	)
		flags |= VIEW_UPDATE_NAME; /* Recreate PangoLayout */

	if (o_display_show_dir_thumbs.has_changed ||
			o_jpeg_thumbs.has_changed ||
			o_pixmap_thumb_file_size.has_changed
	)
		flags |= VIEW_UPDATE_VIEWDATA;


	gboolean changed =
		o_display_show_thumbs.has_changed ||
		o_display_dirs_first.has_changed ||
		o_display_caps_first.has_changed ||
		o_display_newly_first.has_changed ||
		o_display_show_full_type.has_changed ||
		o_vertical_order_small.has_changed ||
		o_vertical_order_large.has_changed ||
		o_xattr_show.has_changed ||
		o_view_alpha.has_changed ||
		o_use_background_colour.has_changed ||
		o_background_colour.has_changed ||
		o_filer_auto_resize.has_changed ||
		o_filer_size_limit.has_changed ||
		o_filer_width_limit.has_changed;

	for (next = all_filer_windows; next; next = next->next)
	{
		FilerWindow *filer_window = (FilerWindow *) next->data;

		if (o_display_show_thumbs.has_changed)
			filer_set_title(filer_window);

		if (o_display_dirs_first.has_changed ||
		    o_display_caps_first.has_changed ||
		    o_display_newly_first.has_changed)
			view_sort(VIEW(filer_window->view));

		if (flags || changed)
		{
			view_style_changed(filer_window->view, flags);

			if (o_filer_auto_resize.int_value == RESIZE_ALWAYS)
				view_autosize(filer_window->view, FALSE);
		}
	}
}

/* Return a new string giving details of this item, or NULL if details
 * are not being displayed. If details are not yet available, return
 * a string of the right length.
 */
static char *getdetails(FilerWindow *filer_window,
		DirItem *item, ViewData *view, gboolean sizeonly)
{
	mode_t	m = item->mode;
	guchar 	*buf = NULL;
	gboolean scanned = item->base_type != TYPE_UNKNOWN;
	gboolean vertical = filer_window->display_style == HUGE_ICONS;

	int height = 1;
	int width = 0;

	if (scanned && item->lstat_errno)
		buf = g_strdup_printf(_("lstat(2) failed: %s"),
				g_strerror(item->lstat_errno));
	else if (filer_window->details_type == DETAILS_TYPE)
	{
		MIME_type	*type = item->mime_type;

		if (!scanned)
			buf = g_strdup("unknown/");
		else
			buf = g_strdup_printf("%s/%s",
					type->media_type, type->subtype);
	}
	else if (sizeonly && filer_window->details_type == DETAILS_TIMES)
	{ //time is heavy
		static int timelen = 0;
		if (!timelen)
		{
			gchar *ctime = pretty_time(&item->ctime);
			timelen = strlen(ctime);
			g_free(ctime);
		}
		width = timelen + 3;
		if (vertical)
			height = 3;
		else
			width = width * 3 + 2;
	}
	else if (filer_window->details_type == DETAILS_TIMES)
	{
		guchar	*ctime, *mtime, *atime;

		ctime = pretty_time(&item->ctime);
		mtime = pretty_time(&item->mtime);
		atime = pretty_time(&item->atime);
		if (vertical)
		{
			buf = g_strdup_printf("a[%s]\nc[%s]\nm[%s]", atime, ctime, mtime);
			height = 3;
			width = strlen(atime) + 3;
		}
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
				buf = g_strdup("---,---,---/--"
#ifdef S_ISVTX
					"-"
#endif
					"\n12345678 12345678");
			else
				buf = g_strdup("---,---,---/--"
#ifdef S_ISVTX
					"-"
#endif
					" 12345678 12345678");
		} else {
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

		if (vertical)
		{
			height = 2;
			width = 17;
		}
		else
#ifdef S_ISVTX
			width = 17 + 16;
#else
			width = 17 + 15;
#endif
	}
	else
	{
		if (!scanned)
		{
			if (filer_window->display_style == SMALL_ICONS)
				buf = g_strdup("1234b");
			else
				buf = g_strdup("1234 b");
		} else
//		if (item->base_type != TYPE_DIRECTORY)
		{
			if (filer_window->display_style == SMALL_ICONS)
				buf = g_strdup(format_size_aligned(item->size));
			else
				buf = g_strchomp(g_strdup(format_size(item->size)));
		}
//		else
//			buf = g_strdup("-");

	}

	if (!width)
		width = strlen(buf);
	view->details_width =  width * fw_mono_width;
	view->details_height = height * fw_mono_height;

	return buf;
}

PangoLayout *make_layout(FilerWindow *fw, DirItem *item)
{
	DisplayStyle style = fw->display_style;
	int	wrap_width = -1;
	PangoLayout *ret;
	PangoAttrList *list = NULL;

	//instead of gtk_widget_create_pango_layout for multi thread
	PangoContext *pctx = gtk_widget_create_pango_context(fw->window);
	ret = pango_layout_new(pctx);
	g_object_unref(pctx);

	if (g_utf8_validate(item->leafname, -1, NULL))
	{
		pango_layout_set_text(ret, item->leafname, -1);
//		ret = gtk_widget_create_pango_layout(fw->window, item->leafname);
		pango_layout_set_auto_dir(ret, FALSE);
	}
	else
	{
		PangoAttribute	*attr;
		gchar *utf8;

		utf8 = to_utf8(item->leafname);

		pango_layout_set_text(ret, utf8, -1);
//		ret = gtk_widget_create_pango_layout(fw->window, utf8);

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
		if (fw->name_scale != 1.0)
			wrap_width = fw->name_scale_itemw * fw->name_scale * PANGO_SCALE;

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

PangoLayout * make_details_layout(
		FilerWindow *fw, DirItem *item, ViewData *view, gboolean sizeonly)
{
	static PangoFontDescription *monospace = NULL;
	char *str;
	PangoLayout *ret = NULL;

	static GMutex m;
	if (!monospace) {
		g_mutex_lock(&m);
		if (!monospace)
			monospace = pango_font_description_from_string("monospace");
		g_mutex_unlock(&m);
	}

	g_mutex_lock(&m);
	str = getdetails(fw, item, view, sizeonly);
	g_mutex_unlock(&m);

	if (sizeonly)
		g_free(str);
	else if (str)
	{
		PangoAttrList *details_list;
		int perm_offset = -1;

		PangoContext *pctx = gtk_widget_create_pango_context(fw->window);
		ret = pango_layout_new(pctx);
		g_object_unref(pctx);

		pango_layout_set_text(ret, str, -1);
//		view->details = gtk_widget_create_pango_layout(fw->window, str);
		g_free(str);

		pango_layout_set_font_description(ret, monospace);

//		pango_layout_get_pixel_size(ret,
//				&view->details_width, &view->details_height);

		if (fw->details_type == DETAILS_PERMISSIONS)
			perm_offset = 0;
		if (perm_offset > -1)
		{
			PangoAttribute *attr;

			attr = pango_attr_underline_new(PANGO_UNDERLINE_SINGLE);

			perm_offset += 4 * applicable(item->uid, item->gid);
			attr->start_index = perm_offset;
			attr->end_index   = perm_offset + 3;

			details_list = pango_attr_list_new();
			pango_attr_list_insert(details_list, attr);
			pango_layout_set_attributes(ret, details_list);

			pango_attr_list_unref(details_list);
		}
	}

	return ret;
}

/* Each displayed item has a ViewData structure with some cached information
 * to help quickly draw the item (eg, the PangoLayout). This function updates
 * this information.
 */
void display_update_view(FilerWindow *fw,
			DirItem *item,
			ViewData *view,
			gboolean update_name_layout,
			gboolean clear)
{
	gboolean basic = o_fast_font_calc.int_value;

	if (view->iconstatus == 0 && item->base_type != TYPE_UNKNOWN)
		view->iconstatus = 1;

	if (!update_name_layout && view->iconstatus > 1)
		view->iconstatus = -1;

	if (!clear && fw->details_type != DETAILS_NONE)
		make_details_layout(fw, item, view, TRUE);

	if (!update_name_layout &&
			view->recent == (item->flags & ITEM_FLAG_RECENT))
		return;

	view->recent = item->flags & ITEM_FLAG_RECENT;
	g_clear_object(&view->name);

	if (clear)
	{
		view->name_width = 0;
		view->name_height = 0;
		return;
	}

	basic &= (fw->display_style == SMALL_ICONS ||
				(fw->details_type != DETAILS_NONE &&
				 fw->display_style != HUGE_ICONS));

	if (basic)
	{
		int w = 0;
		gchar *name = item->leafname;

		int (*widths)[] = (item->flags & ITEM_FLAG_RECENT) ?
			&fw_font_widthsb : &fw_font_widths;

		for (; *name; name++)
			if (*name >= 0x20 && *name <= 0x7e)
				w += (*widths)[(int) *name];
			else
			{
				basic = FALSE;
				break;
			}

		view->name_width = w;
		view->name_height = fw_font_height;
	}

	if (!basic)
	{
		PangoLayout *leafname = make_layout(fw, item);

		pango_layout_get_pixel_size(leafname,
				&view->name_width, &view->name_height);

		g_object_unref(G_OBJECT(leafname));
	}

}

