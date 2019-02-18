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

/* pixmaps.c - code for handling pixbufs (despite the name!) */

#include "config.h"
#define PIXMAPS_C

/* Remove pixmaps from the cache when they haven't been accessed for
 * this period of time (seconds).
 */

#define PIXMAP_PURGE_TIME 60 * 60 * 4
#define PIXMAP_THUMB_SIZE  256
#define PIXMAP_THUMB_TOO_OLD_TIME  5

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include <gtk/gtk.h>

#include "global.h"

#include "fscache.h"
#include "support.h"
#include "gui_support.h"
#include "pixmaps.h"
#include "main.h"
#include "filer.h"
#include "dir.h"
#include "diritem.h"
#include "choices.h"
#include "options.h"
#include "action.h"
#include "type.h"

GFSCache *pixmap_cache = NULL;
GFSCache *thumb_cache = NULL;

static const char * bad_xpm[] = {
"12 12 3 1",
" 	c #000000000000",
".	c #FFFF00000000",
"x	c #FFFFFFFFFFFF",
"            ",
" ..xxxxxx.. ",
" ...xxxx... ",
" x...xx...x ",
" xx......xx ",
" xxx....xxx ",
" xxx....xxx ",
" xx......xx ",
" x...xx...x ",
" ...xxxx... ",
" ..xxxxxx.. ",
"            "};

MaskedPixmap *im_error;
MaskedPixmap *im_unknown;

MaskedPixmap *im_appdir;

MaskedPixmap *im_dirs;

GtkIconSize mount_icon_size = -1;

int small_height = 0;
int small_width = 0;
int thumb_size = PIXMAP_THUMB_SIZE;

gchar *thumb_dir = "normal";

Option o_pixmap_thumb_file_size;
Option o_jpeg_thumbs;
static Option o_purge_time;
Option o_purge_days;


typedef struct _ChildThumbnail ChildThumbnail;

/* There is one of these for each active child process */
struct _ChildThumbnail {
	gchar	 *path;
	GFunc	 callback;
	gpointer data;
	pid_t	 child;
	guint	 timeout;
	guint	 order;
};
static guint ordered_num = 0;
static guint next_order = 0;

static const char *stocks[] = {
	ROX_STOCK_SHOW_DETAILS,
	ROX_STOCK_SHOW_HIDDEN,
	ROX_STOCK_MOUNT,
	ROX_STOCK_MOUNTED,
	ROX_STOCK_SYMLINK,
	ROX_STOCK_XATTR,
};

/* Static prototypes */

static void load_default_pixmaps(void);
static gint purge_pixmaps(gpointer data);
static gint purge_thumbs(gpointer data);
static MaskedPixmap *image_from_file(const char *path);
static MaskedPixmap *get_bad_image(void);
static GdkPixbuf *get_thumbnail_for(const char *path, gboolean forcheck);
static void ordered_update(ChildThumbnail *info);
static void thumbnail_done(ChildThumbnail *info);
static void create_thumbnail(const gchar *path, MIME_type *type);
static GList *thumbs_purge_cache(Option *option, xmlNode *node, guchar *label);
static gchar *thumbnail_path(const gchar *path);
static gchar *thumbnail_program(MIME_type *type);
static GdkPixbuf *extract_tiff_thumbnail(const gchar *path);
static void make_dir_thumb(const gchar *path);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/
static void set_thumb_size()
{
	switch (thumb_size = o_pixmap_thumb_file_size.int_value) {
	case 512:
		thumb_dir = "huge";
		break;
	case 256:
		thumb_dir = "large";
		break;
	case 128:
		thumb_dir = "normal";
		break;
	case 64:
		thumb_dir = "small";
		break;
	default:
		thumb_dir = "fail";
	}
}
static void options_changed()
{
	if (o_pixmap_thumb_file_size.has_changed)
	{
		set_thumb_size();
		g_fscache_purge(thumb_cache, 0);
	}

	if (o_jpeg_thumbs.has_changed)
		g_fscache_purge(thumb_cache, 0);

	if (o_purge_time.has_changed)
		g_fscache_purge(thumb_cache, o_purge_time.int_value);
}

void pixmaps_init(void)
{
	GtkIconFactory *factory;
	int i;

	option_add_int(&o_pixmap_thumb_file_size, "thumb_file_size", PIXMAP_THUMB_SIZE);
//	option_add_int(&o_purge_time, "purge_time", PIXMAP_PURGE_TIME);
	option_add_int(&o_purge_time, "purge_time", 0);
	option_add_int(&o_jpeg_thumbs, "jpeg_thumbs", TRUE);
	option_add_int(&o_purge_days, "purge_days", 90);
	option_add_notify(options_changed);

	gtk_widget_push_colormap(gdk_rgb_get_colormap());

	pixmap_cache = g_fscache_new((GFSLoadFunc) image_from_file, NULL, NULL);
	thumb_cache = g_fscache_new((GFSLoadFunc) image_from_file, NULL, NULL);

	g_timeout_add(6000, purge_thumbs, NULL);
	g_timeout_add(PIXMAP_PURGE_TIME / 2 * 1000, purge_pixmaps, NULL);

	factory = gtk_icon_factory_new();
	for (i = 0; i < G_N_ELEMENTS(stocks); i++)
	{
		GdkPixbuf *pixbuf;
		GError *error = NULL;
		gchar *path;
		GtkIconSet *iset;
		const gchar *name = stocks[i];

		path = g_strconcat(app_dir, "/images/", name, ".png", NULL);
		pixbuf = gdk_pixbuf_new_from_file(path, &error);
		if (!pixbuf)
		{
			g_warning("%s", error->message);
			g_error_free(error);
			pixbuf = gdk_pixbuf_new_from_xpm_data(bad_xpm);
		}
		g_free(path);

		iset = gtk_icon_set_new_from_pixbuf(pixbuf);
		g_object_unref(G_OBJECT(pixbuf));
		gtk_icon_factory_add(factory, name, iset);
		gtk_icon_set_unref(iset);
	}
	gtk_icon_factory_add_default(factory);

	mount_icon_size = gtk_icon_size_register("rox-mount-size", 14, 14);

	load_default_pixmaps();
	set_thumb_size();
	option_register_widget("thumbs-purge-cache", thumbs_purge_cache);
}

/* Load image <appdir>/images/name.png.
 * Always returns with a valid image.
 */
MaskedPixmap *load_pixmap(const char *name)
{
	guchar *path;
	MaskedPixmap *retval;

	path = g_strconcat(app_dir, "/images/", name, ".png", NULL);
	retval = image_from_file(path);
	g_free(path);

	if (!retval)
		retval = get_bad_image();

	return retval;
}

/* Create a MaskedPixmap from a GTK stock ID. Always returns
 * a valid image.
 */
static MaskedPixmap *mp_from_stock(const char *stock_id, int size)
{
	GtkIconSet *icon_set;
	GdkPixbuf  *pixbuf;
	MaskedPixmap *retval;

	/*icon_set = gtk_icon_factory_lookup_default(stock_id);*/
	icon_set = gtk_style_lookup_icon_set(gtk_widget_get_default_style(),
					     stock_id);
	if (!icon_set)
		return get_bad_image();

	pixbuf = gtk_icon_set_render_icon(icon_set,
                                     gtk_widget_get_default_style(), /* Gtk bug */
                                     GTK_TEXT_DIR_LTR,
                                     GTK_STATE_NORMAL,
                                     size,
                                     NULL,
                                     NULL);
	retval = masked_pixmap_new(pixbuf);
	g_object_unref(pixbuf);

	return retval;
}

void pixmap_make_small(MaskedPixmap *mp)
{
	if (mp->sm_pixbuf)
		return;

	g_return_if_fail(mp->src_pixbuf != NULL);

	mp->sm_pixbuf = scale_pixbuf(mp->src_pixbuf,
					small_width, small_height);

	if (!mp->sm_pixbuf)
	{
		mp->sm_pixbuf = mp->src_pixbuf;
		g_object_ref(mp->sm_pixbuf);
	}

	mp->sm_width = gdk_pixbuf_get_width(mp->sm_pixbuf);
	mp->sm_height = gdk_pixbuf_get_height(mp->sm_pixbuf);
}

/* -1:not thumb target 0:not created 1:created and loaded */
gint pixmap_check_thumb(const gchar *path)
{
	gboolean found;
	GdkPixbuf *pixmap = g_fscache_lookup_full(pixmap_cache, path,
			FSCACHE_LOOKUP_ONLY_NEW, &found);
	if (pixmap)
		g_object_unref(pixmap);
	else
		if (found) return -2;

	GdkPixbuf *image = pixmap_try_thumb(path, TRUE);

	if (image)
	{
		g_object_unref(image);
		return 1;
	}

	MIME_type *type = type_from_path(path);
	if (type)
	{
		gchar *thumb_prog = NULL;
		if (strcmp(type->media_type, "image") == 0 ||
				(thumb_prog = thumbnail_program(type)))
		{
			g_free(thumb_prog);
			return 0;
		}
	}

	return -1;
}


GdkPixbuf *pixmap_load_thumb(const gchar *path)
{
	GdkPixbuf *ret = NULL;
	gboolean found = FALSE;
	MaskedPixmap *pixmap;

	pixmap = g_fscache_lookup_full(pixmap_cache,
			path, FSCACHE_LOOKUP_ONLY_NEW, &found);

	if (found && !pixmap)
		return NULL;

	if (pixmap)
		g_object_unref(pixmap);

	found = FALSE;

	if (o_purge_time.int_value > 0)
		ret = g_fscache_lookup_full(thumb_cache,
				path, FSCACHE_LOOKUP_ONLY_NEW, &found);

	if (!found) {
		ret = get_thumbnail_for(path, FALSE);
		if (ret && o_purge_time.int_value > 0)
			g_fscache_insert(thumb_cache, path, ret, TRUE);
	}
	return ret;
}


static int thumb_prog_timeout(ChildThumbnail *info)
{
	info->timeout = 0;
	kill(info->child, 9);
	return FALSE;
}

/* Load image 'path' in the background and insert into pixmap_cache.
 * Call callback(data, path) when done (path is NULL => error).
 * If the image is already uptodate, or being created already, calls the
 * callback right away.
 */
void pixmap_background_thumb(const gchar *path, gboolean noorder,
		GFunc callback, gpointer data)
{
	gboolean	found;
	GdkPixbuf	*image;
	pid_t		child;
	ChildThumbnail	*info;
	MIME_type       *type;
	gchar		*thumb_prog, *base;

	image = pixmap_try_thumb(path, TRUE);

	if (image)
	{
		g_object_unref(image);
		/* Thumbnail loaded */
		callback(data, (gpointer)path);
		return;
	}

	/* Is it currently being created? */
	image = g_fscache_lookup_full(thumb_cache, path,
					FSCACHE_LOOKUP_ONLY_NEW, &found);

	if (found)
	{
		/* Thumbnail is known, or being created */
		if (image)
			g_object_unref(image);

		callback(data, NULL);
		//append to last for sym links what sharing the thumb
		info = g_new0(ChildThumbnail, 1);
		info->path = g_strdup(path);
		info->order = ordered_num++;
		ordered_update(info);
		return;
	}

	/* Not in memory, nor in the thumbnails directory.  We need to
	 * generate it */

	type = type_from_path(path);
	if (!type)
		type = text_plain;

	/* Add an entry, set to NULL, so no-one else tries to load this
	 * image.
	 */
	g_fscache_insert(thumb_cache, path, NULL, TRUE);

	thumb_prog = thumbnail_program(type);

	/* Only attempt to load 'images' types ourselves */
	if (thumb_prog == NULL && strcmp(type->media_type, "image") != 0)
	{
		callback(data, NULL);
		return;		/* Don't know how to handle this type */
	}

	info = g_new(ChildThumbnail, 1);
	info->path = g_strdup(path);
	info->callback = callback;
	info->data = data;
	info->timeout = 0;
	info->order = ordered_num++;
	if (noorder) info->order = 0;

	child = fork();
	if (child == -1)
	{
		g_free(thumb_prog);
		delayed_error("fork(): %s", g_strerror(errno));
		callback(data, NULL);
		return;
	}

	if (child == 0)
	{
		/* We are the child process.  (We are sloppy with freeing
		   memory, but since we go away very quickly, that's ok.) */
		if (thumb_prog)
		{
			DirItem *item;

			base = g_path_get_basename(thumb_prog);
			item = diritem_new(base);
			g_free(base);
			diritem_restat(thumb_prog, item, NULL, TRUE);
			if (item->flags & ITEM_FLAG_APPDIR)
				thumb_prog = g_strconcat(thumb_prog, "/AppRun",
						NULL);

			execl(thumb_prog, thumb_prog, path,
					thumbnail_path(path),
					g_strdup_printf("%d", thumb_size),
					NULL);

			_exit(1);
		}

		create_thumbnail(path, type);
		_exit(0);
	}

	g_free(thumb_prog);
	info->child = child;
	info->timeout = g_timeout_add_seconds(14,
			(GSourceFunc) thumb_prog_timeout, info);
	on_child_death(child, (CallbackFn) thumbnail_done, info);
}

/*
 * Return the thumbnail for a file, only if available.
 */
GdkPixbuf *pixmap_try_thumb(const gchar *path, gboolean forcheck)
{
	gboolean  found;
	GdkPixbuf *image;
	GdkPixbuf *pixbuf;

	if (o_purge_time.int_value > 0) {
		image = g_fscache_lookup_full(thumb_cache, path,
						FSCACHE_LOOKUP_ONLY_NEW, &found);

		if (found)
		{
			/* Thumbnail is known, or being created */
			if (image)
				return image;
		}
	}

	pixbuf = get_thumbnail_for(path, forcheck);

	if (!pixbuf)
	{
		struct stat info1, info2;
		char *dir;

		/* Skip zero-byte files. They're either empty, or
		 * special (may cause us to hang, e.g. /proc/kmsg). */
		if (mc_stat(path, &info1) == 0 && info1.st_size == 0) {
			return NULL;
		}

		dir = g_path_get_dirname(path);

		/* If the image itself is in ~/.cache/thumbnails, load it now
		 * (ie, don't create thumbnails for thumbnails!).
		 */
		if (mc_stat(dir, &info1) != 0)
		{
			g_free(dir);
			return NULL;
		}
		g_free(dir);

		if (mc_stat(make_path(make_path(home_dir, ".cache/thumbnails"), thumb_dir),
			    &info2) == 0 &&
			    info1.st_dev == info2.st_dev &&
			    info1.st_ino == info2.st_ino)
		{
			pixbuf = rox_pixbuf_new_from_file_at_scale(path,
					thumb_size, thumb_size,
								   TRUE, NULL);
			if (!pixbuf)
			{
				return NULL;
			}
		}
	}

	if (pixbuf)
	{
		if (o_purge_time.int_value > 0)
			g_fscache_insert(thumb_cache, path, pixbuf, TRUE);

		return pixbuf;
	}

	return NULL;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Create a thumbnail file for this image */
static void save_thumbnail(const char *pathname, GdkPixbuf *full)
{
	struct stat info;
	gchar *path;
	int original_width, original_height;
	GString *to;
	char *md5, *swidth, *sheight, *ssize, *smtime, *uri;
	mode_t old_mask;
	int name_len;
	GdkPixbuf *thumb;

	if (mc_stat(pathname, &info) != 0)
		return;

	thumb = scale_pixbuf(full, thumb_size, thumb_size);

	original_width = gdk_pixbuf_get_width(full);
	original_height = gdk_pixbuf_get_height(full);

	swidth = g_strdup_printf("%d", original_width);
	sheight = g_strdup_printf("%d", original_height);
	ssize = g_strdup_printf("%" SIZE_FMT, info.st_size);
	smtime = g_strdup_printf("%ld", (long) info.st_mtime);

	path = pathdup(pathname);
	uri = g_filename_to_uri(path, NULL, NULL);
	if (!uri)
	        uri = g_strconcat("file://", path, NULL);
	md5 = md5_hash(uri);
	g_free(path);

	to = g_string_new(home_dir);
	g_string_append(to, "/.cache");
	mkdir(to->str, 0700);
	g_string_append(to, "/thumbnails/");
	mkdir(to->str, 0700);

	g_string_append(to, thumb_dir);
	g_string_append(to, "/");

	mkdir(to->str, 0700);
	g_string_append(to, md5);
	name_len = to->len + 4; /* Truncate to this length when renaming */
	g_string_append_printf(to, ".%s.ROX-Filer-%ld",
			o_jpeg_thumbs.int_value ? "jpg" : "png", (long) getpid());

	g_free(md5);

	old_mask = umask(0077);
	if (o_jpeg_thumbs.int_value == 1)
	{
		//At least we don't need extensions being '.jpg'
		gdk_pixbuf_save(thumb, to->str, "jpeg", NULL,
				"quality", "77",
				NULL);
	}
	else
	{
		gdk_pixbuf_save(thumb, to->str, "png", NULL,
				"tEXt::Thumb::Image::Width", swidth,
				"tEXt::Thumb::Image::Height", sheight,
				"tEXt::Thumb::Size", ssize,
				"tEXt::Thumb::MTime", smtime,
				"tEXt::Thumb::URI", uri,
				"tEXt::Software", PROJECT,
				NULL);
	}
	umask(old_mask);

	/* We create the file ###.png.ROX-Filer-PID and rename it to avoid
	 * a race condition if two programs create the same thumb at
	 * once.
	 */
	{
		gchar *final;

		final = g_strndup(to->str, name_len);
		if (rename(to->str, final))
			g_warning("Failed to rename '%s' to '%s': %s",
				  to->str, final, g_strerror(errno));
		g_free(final);
	}

	g_object_unref(thumb);
	g_string_free(to, TRUE);
	g_free(swidth);
	g_free(sheight);
	g_free(ssize);
	g_free(smtime);
	g_free(uri);
}

static gchar *thumbnail_path(const char *path)
{
	gchar *uri, *md5;
	GString *to;
	gchar *ans;

	uri = g_filename_to_uri(path, NULL, NULL);
	if(!uri)
	       uri = g_strconcat("file://", path, NULL);
	md5 = md5_hash(uri);

	to = g_string_new(home_dir);
	g_string_append(to, "/.cache");
	mkdir(to->str, 0700);
	g_string_append(to, "/thumbnails/");
	mkdir(to->str, 0700);

	g_string_append(to, thumb_dir);
	g_string_append(to, "/");

	mkdir(to->str, 0700);
	g_string_append(to, md5);
	g_string_append(to, o_jpeg_thumbs.int_value ? ".jpg" : ".png");

	g_free(md5);
	g_free(uri);

	ans=to->str;
	g_string_free(to, FALSE);

	return ans;
}

/* Return a program to create thumbnails for files of this type.
 * NULL to try to make it ourself (using gdk).
 * g_free the result.
 */
static gchar *thumbnail_program(MIME_type *type)
{
	gchar *leaf;
	gchar *path;

	if (!type)
		return NULL;

	leaf = g_strconcat(type->media_type, "_", type->subtype, NULL);
	path = choices_find_xdg_path_load(leaf, "MIME-thumb", SITE);
	g_free(leaf);
	if (path)
	{
		return path;
	}

	path = choices_find_xdg_path_load(type->media_type, "MIME-thumb",
					  SITE);

	return path;
}

/* Load path and create the thumbnail
 * file. Parent will notice when we die.
 */
static void create_thumbnail(const gchar *path, MIME_type *type)
{
	GdkPixbuf *image=NULL;

        if(strcmp(type->subtype, "jpeg")==0)
            image=extract_tiff_thumbnail(path);

	if(!image)
            image = rox_pixbuf_new_from_file_at_scale(path,
			thumb_size, thumb_size, TRUE, NULL);

	if (image)
	{
		save_thumbnail(path, image);
		g_object_unref(image);
	}
}

char *pixmap_make_thumb_path(const char *path)
{
	char *thumb_path, *md5, *uri;

	uri = g_filename_to_uri(path, NULL, NULL);
	if (!uri)
	        uri = g_strconcat("file://", path, NULL);
	md5 = md5_hash(uri);
	g_free(uri);

	thumb_path = g_strdup_printf(
			"%s/.cache/thumbnails/%s/%s.%s",
			home_dir, thumb_dir, md5, o_jpeg_thumbs.int_value ? "jpg" : "png");
	g_free(md5);

	return thumb_path; /* This return is used unlink! Be carefull */
}

static void make_dir_thumb(const gchar *path)
{
	gchar *dir = g_path_get_dirname(path);
	gchar *dir_thumb_path = pixmap_make_thumb_path(dir);
	GdkPixbuf *image = gdk_pixbuf_new_from_file(dir_thumb_path, NULL);
	if (image)
	{
		g_object_unref(image);
	}
	else
	{
		unlink(dir_thumb_path); //////////////////////////

		char *thumb_path = pixmap_make_thumb_path(path);
		char *rel_path = get_relative_path(dir_thumb_path, thumb_path);

		if (symlink(rel_path, dir_thumb_path) == 0)
			dir_force_update_path(dir, TRUE);

		g_free(rel_path);
		g_free(thumb_path);
	}
	g_free(dir_thumb_path);
	g_free(dir);
}

static void ordered_update(ChildThumbnail *info)
{
	static GSList *done_stack = NULL;
	done_stack = g_slist_prepend(done_stack, info);

	if (next_order < info->order) return;

	GSList *n = done_stack;
	while (n)
	{
		ChildThumbnail *li = n->data;
		if (li->order > next_order)
		{
			n = n->next;
			continue;
		}

		if (!li->callback)
			dir_force_update_path(li->path, TRUE);
		make_dir_thumb(li->path);

		g_free(li->path);
		g_free(li);

		next_order++;
		n = done_stack =
			g_slist_delete_link(done_stack, n);
	}
}
static void thumbnail_done(ChildThumbnail *info)
{
	if (info->timeout)
		g_source_remove(info->timeout);

	GdkPixbuf *thumb = get_thumbnail_for(info->path, FALSE);
	if (thumb)
	{
		g_object_unref(thumb);
		g_fscache_remove(thumb_cache, info->path);
	}
	else
		g_fscache_insert(pixmap_cache, info->path, NULL, TRUE);

	info->callback(info->data, thumb ? info->path : NULL);

	ordered_update(info);
}


/* Check if we have an up-to-date thumbnail for this image.
 * If so, return it. Otherwise, returns NULL.
 */
static GdkPixbuf *get_thumbnail_for(const char *pathname, gboolean forcheck)
{
	GdkPixbuf *thumb = NULL;
	char *thumb_path, *path, *pic_path = NULL;
	const char *pic_uri, *ssize, *smtime;
	struct stat info, thumbinfo;
	time_t ttime, now;

	path = pathdup(pathname);

	thumb_path = pixmap_make_thumb_path(path);

	thumb = gdk_pixbuf_new_from_file(thumb_path, NULL);
	if (!thumb)
	{
		if (forcheck
				&& !mc_lstat(thumb_path, &thumbinfo)
				&& S_ISLNK(thumbinfo.st_mode)
				&& mc_stat(thumb_path, &thumbinfo)
		)
			unlink(thumb_path);

		goto out;
	}

	/* Note that these don't need freeing... */
	pic_uri = gdk_pixbuf_get_option(thumb, "tEXt::Thumb::URI");
	if (pic_uri)
	{
		pic_path = g_filename_from_uri(pic_uri, NULL, NULL);

		if (mc_stat(pic_path, &info) != 0)
			goto err;

		smtime = gdk_pixbuf_get_option(thumb, "tEXt::Thumb::MTime");
		if (!smtime)
			goto err;
		ttime=(time_t) atol(smtime);
		time(&now);
		if (info.st_mtime != ttime && now>ttime+PIXMAP_THUMB_TOO_OLD_TIME)
			goto err;

		/* This is optional, so don't flag an error if it is missing */
		ssize = gdk_pixbuf_get_option(thumb, "tEXt::Thumb::Size");
		if (ssize && info.st_size < atol(ssize))
			goto err;
	}
	else
	{ //for jpeg
		if (mc_lstat(thumb_path, &thumbinfo) != 0 ||
			mc_lstat(path, &info) != 0
			)
			goto err;

		if (forcheck && (
					   info.st_ctime == thumbinfo.st_ctime
					|| info.st_ctime == thumbinfo.st_ctime - 1))
					   //we asume generater doesn't take more than 1 sec
		{ //maybe thumb is old in a sec.
			time(&now);
			if (now > thumbinfo.st_ctime)
				goto err;
		}
		else if (info.st_ctime > thumbinfo.st_ctime)
			goto err;
	}

	goto out;
err:
	g_object_unref(thumb);
	unlink(thumb_path);
	thumb = NULL;

out:
	g_free(pic_path);
	g_free(path);
	g_free(thumb_path);
	return thumb;
}

/* Load the image 'path' and return a pointer to the resulting
 * MaskedPixmap. NULL on failure.
 * Doesn't check for thumbnails (this is for small icons).
 */
static MaskedPixmap *image_from_file(const char *path)
{
	GdkPixbuf	*pixbuf;
	MaskedPixmap	*image;
	GError		*error = NULL;

	pixbuf = gdk_pixbuf_new_from_file(path, &error);
	if (!pixbuf)
	{
		g_warning("%s\n", error->message);
		g_error_free(error);
		return NULL;
	}

	image = masked_pixmap_new(pixbuf);

	g_object_unref(pixbuf);

	return image;
}

/* Load this icon named by this .desktop file from the current theme.
 * NULL on failure.
 */
MaskedPixmap *pixmap_from_desktop_file(const char *path)
{
	GError *error = NULL;
	MaskedPixmap *image = NULL;
	char *icon = NULL;

	icon = get_value_from_desktop_file(path,
					"Desktop Entry", "Icon", &error);

//	GKeyFile *key_file = g_key_file_new();
//	if (g_key_file_load_from_file (key_file, path, G_KEY_FILE_NONE, &error))
//		icon = g_key_file_get_string(key_file, "Desktop Entry", "Icon", &error);
//	g_key_file_unref(key_file);

	if (error)
	{
		g_warning("Failed to parse .desktop file '%s':\n%s",
				path, error->message);
		goto err;
	}
	if (!icon)
		goto err;

	if (icon[0] == '/')
		image = image_from_file(icon);
	else
	{
		GdkPixbuf *pixbuf;
		int tmp_fd;
		char *extension;

		/* For some unknown reason, some icon names have extensions.
		 * Remove them.
		 */
		extension = strrchr(icon, '.');
		if (extension && (strcmp(extension, ".png") == 0
						|| strcmp(extension, ".xpm") == 0
						|| strcmp(extension, ".svg") == 0))
		{
			*extension = '\0';
		}

		/* SVG reader is very noisy, so redirect stderr to stdout */
		tmp_fd = dup(2);
		dup2(1, 2);
		pixbuf = theme_load_icon(icon, thumb_size, 0, NULL);
		dup2(tmp_fd, 2);
		close(tmp_fd);

		if (pixbuf == NULL)
			goto err;	/* Might just not be in the theme */

		image = masked_pixmap_new(pixbuf);
		g_object_unref(pixbuf);
	}
err:
	if (error != NULL)
		g_error_free(error);
	if (icon != NULL)
		g_free(icon);
	return image;
}

/* Scale src down to fit in max_w, max_h and return the new pixbuf.
 * If src is small enough, then ref it and return that.
 */
GdkPixbuf *scale_pixbuf(GdkPixbuf *src, int max_w, int max_h)
{
	int	w, h;

	w = gdk_pixbuf_get_width(src);
	h = gdk_pixbuf_get_height(src);

	if (w <= max_w && h <= max_h)
	{
		g_object_ref(src);
		return src;
	}
	else
	{
		float scale_x = ((float) w) / max_w;
		float scale_y = ((float) h) / max_h;
		float scale = MAX(scale_x, scale_y);
		int dest_w = w / scale;
		int dest_h = h / scale;

		return gdk_pixbuf_scale_simple(src,
						MAX(dest_w, 1),
						MAX(dest_h, 1),
						GDK_INTERP_BILINEAR);
	}
}

/* Return a pointer to the (static) bad image. The ref counter will ensure
 * that the image is never freed.
 */
static MaskedPixmap *get_bad_image(void)
{
	GdkPixbuf *bad;
	MaskedPixmap *mp;

	bad = gdk_pixbuf_new_from_xpm_data(bad_xpm);
	mp = masked_pixmap_new(bad);
	g_object_unref(bad);

	return mp;
}

/* Called now and then to clear out old pixmaps */
static gint purge_pixmaps(gpointer data)
{
	g_fscache_purge(pixmap_cache, PIXMAP_PURGE_TIME);
	return TRUE;
}
static gint purge_thumbs(gpointer data)
{
	g_fscache_purge(thumb_cache, o_purge_time.int_value);

	g_timeout_add(MIN(60000, o_purge_time.int_value * 300 + 2000), purge_thumbs, NULL);
	return FALSE;
}

static gpointer parent_class;

static void masked_pixmap_finialize(GObject *object)
{
	MaskedPixmap *mp = (MaskedPixmap *) object;

	if (mp->src_pixbuf)
	{
		g_object_unref(mp->src_pixbuf);
		mp->src_pixbuf = NULL;
	}

	if (mp->pixbuf)
	{
		g_object_unref(mp->pixbuf);
		mp->pixbuf = NULL;
	}

	if (mp->sm_pixbuf)
	{
		g_object_unref(mp->sm_pixbuf);
		mp->sm_pixbuf = NULL;
	}

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void masked_pixmap_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	object->finalize = masked_pixmap_finialize;
}

static void masked_pixmap_init(GTypeInstance *object, gpointer gclass)
{
	MaskedPixmap *mp = (MaskedPixmap *) object;

	mp->src_pixbuf = NULL;

	mp->huge_width = -1;
	mp->huge_height = -1;

	mp->pixbuf = NULL;
	mp->width = -1;
	mp->height = -1;

	mp->sm_pixbuf = NULL;
	mp->sm_width = -1;
	mp->sm_height = -1;
}

static GType masked_pixmap_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (MaskedPixmapClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			masked_pixmap_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(MaskedPixmap),
			0,			/* n_preallocs */
			masked_pixmap_init
		};

		type = g_type_register_static(G_TYPE_OBJECT, "MaskedPixmap",
					      &info, 0);
	}

	return type;
}

MaskedPixmap *masked_pixmap_new(GdkPixbuf *src)
{
	MaskedPixmap *mp;
	GdkPixbuf	*normal_pixbuf;

	g_return_val_if_fail(src != NULL, NULL);

	normal_pixbuf = scale_pixbuf(src, ICON_WIDTH, ICON_HEIGHT);
	g_return_val_if_fail(normal_pixbuf != NULL, NULL);

	mp = g_object_new(masked_pixmap_get_type(), NULL);

	mp->huge_width = gdk_pixbuf_get_width(src);
	mp->huge_height = gdk_pixbuf_get_height(src);
	if (mp->huge_width <= thumb_size && mp->huge_height <= thumb_size)
	{
		g_object_ref(src);
		mp->src_pixbuf = src;
	}
	else
	{
		mp->src_pixbuf = scale_pixbuf(src, thumb_size, thumb_size);
		mp->huge_width = gdk_pixbuf_get_width(mp->src_pixbuf);
		mp->huge_height = gdk_pixbuf_get_height(mp->src_pixbuf);
	}

	mp->pixbuf = normal_pixbuf;
	mp->width = gdk_pixbuf_get_width(normal_pixbuf);
	mp->height = gdk_pixbuf_get_height(normal_pixbuf);

	return mp;
}

/* Load all the standard pixmaps. Also sets the default window icon. */
static GdkPixbuf *winicon;
static void load_default_pixmaps(void)
{
	GError *error = NULL;

	im_error = mp_from_stock(GTK_STOCK_DIALOG_WARNING,
				 GTK_ICON_SIZE_DIALOG);
	im_unknown = mp_from_stock(GTK_STOCK_DIALOG_QUESTION,
				   GTK_ICON_SIZE_DIALOG);

	im_dirs = load_pixmap("dirs");
	im_appdir = load_pixmap("application");

	winicon = gtk_icon_theme_load_icon(
		gtk_icon_theme_get_default(), "rox", thumb_size, 0, NULL);

	if (!winicon)
		winicon = gdk_pixbuf_new_from_file(
			make_path(app_dir, ".DirIcon"), &error);
	if (winicon)
		gtk_window_set_default_icon(winicon);
	else
	{
		g_warning("%s\n", error->message);
		g_error_free(error);
	}
}

/* Also purges memory cache */
static void purge_disk_cache(GtkWidget *button, gpointer data)
{
	char *path;
	GList *list = NULL;
	DIR *dir;
	struct dirent *ent;

	g_fscache_purge(thumb_cache, 0);

	path = g_strconcat(home_dir, "/.cache/thumbnails/", thumb_dir, "/", NULL);

	dir = opendir(path);
	if (!dir)
	{
		report_error(_("Can't delete thumbnails in %s:\n%s"),
				path, g_strerror(errno));
		goto out;
	}

	time_t checktime = o_purge_days.int_value ?
		time(0) - (o_purge_days.int_value * 3600 * 24): 0;
	struct stat info;

	while ((ent = readdir(dir)))
	{
		if (ent->d_name[0] == '.')
			continue;

		if (o_purge_days.int_value
				&& !mc_lstat(make_path(path, ent->d_name), &info)
				&& info.st_atime > checktime)
			continue;

		list = g_list_prepend(list,
				      g_strconcat(path, ent->d_name, NULL));
	}

	closedir(dir);

	if (list)
	{
		action_delete(list);
		destroy_glist(&list);
	}
	else
		info_message(_("There are no thumbnails to delete"));
out:
	g_free(path);
}

static GList *thumbs_purge_cache(Option *option, xmlNode *node, guchar *label)
{
	GtkWidget *button, *align;

	g_return_val_if_fail(option == NULL, NULL);

	align = gtk_alignment_new(0, 0.5, 0, 0);
	button = button_new_mixed(GTK_STOCK_CLEAR,
				  _("Purge thumbnails disk cache"));
	gtk_container_add(GTK_CONTAINER(align), button);
	g_signal_connect(button, "clicked", G_CALLBACK(purge_disk_cache), NULL);

	return g_list_append(NULL, align);
}

/* Exif reading.
 * Based on Thierry Bousch's public domain exifdump.py.
 */

#define JPEG_FORMAT        0x201
#define JPEG_FORMAT_LENGTH 0x202

/*
 * Extract n-byte integer in Motorola (big-endian) format
 */
static inline long long s2n_motorola(const unsigned char *p, int len)
{
    long long a=0;
    int i;

    for(i=0; i<len; i++)
        a=(a<<8) | (int)(p[i]);

    return a;
}

/*
 * Extract n-byte integer in Intel (little-endian) format
 */
static inline long long s2n_intel(const unsigned char *p, int len)
{
    long long a=0;
    int i;

    for(i=0; i<len; i++)
        a=a | (((int) p[i]) << (i*8));

    return a;
}

/*
 * Extract n-byte integer from data
 */
static int s2n(const unsigned char *dat, int off, int len, char format)
{
    const unsigned char *p=dat+off;

    switch(format) {
    case 'I':
        return s2n_intel(p, len);

    case 'M':
        return s2n_motorola(p, len);
    }

    return 0;
}

/*
 * Load header of JPEG/Exif file and attempt to extract the embedded
 * thumbnail.  Return NULL on failure.
 */
static GdkPixbuf *extract_tiff_thumbnail(const gchar *path)
{
    FILE *in;
    unsigned char header[256];
    int i, n;
    int length;
    unsigned char *data;
    char format;
    int ifd, entries;
    int thumb=0, tlength=0;
    GdkPixbuf *buf=NULL;

    in=fopen(path, "rb");
    if(!in) {
        return NULL;
    }

    /* Check for Exif format */
    n=fread(header, 1, 12, in);
    if(n!=12 || strncmp((char *) header, "\377\330\377\341", 4)!=0 ||
       strncmp((char *)header+6, "Exif", 4)!=0) {
        fclose(in);
        return NULL;
    }

    /* Read header */
    length=header[4]*256+header[5];
    data=g_new(unsigned char, length);
    n=fread(data, 1, length, in);
    fclose(in);   /* File no longer needed */
    if(n!=length) {
        g_free(data);
        return NULL;
    }

    /* Big or little endian (as 'M' or 'I') */
    format=data[0];

    /* Skip over main section */
    ifd=s2n(data, 4, 4, format);
    entries=s2n(data, ifd, 2, format);

    /* Second section contains data on thumbnail */
    ifd=s2n(data, ifd+2+12*entries, 4, format);
    entries=s2n(data, ifd, 2, format);

    /* Loop over the entries */
    for(i=0; i<entries; i++) {
        int entry=ifd+2+12*i;
        int tag=s2n(data, entry, 2, format);
        int type=s2n(data, entry+2, 2, format);
        int offset=entry+8;

        if(type==4) {
            int val=(int) s2n(data, offset, 4, format);

            /* Only interested in two entries, the location of the thumbnail
               and its size */
            switch(tag) {
            case JPEG_FORMAT: thumb=val; break;
            case JPEG_FORMAT_LENGTH: tlength=val; break;
            }
        }
    }

    if(thumb && tlength) {
        GError *err=NULL;
        GdkPixbufLoader *loader;

        /* Don't read outside the header (some files have incorrect data) */
        if(thumb+tlength>length)
            tlength=length-thumb;

        loader=gdk_pixbuf_loader_new();
        gdk_pixbuf_loader_write(loader, data+thumb, tlength, &err);
        if(err) {
            g_error_free(err);
            return NULL;
        }

        gdk_pixbuf_loader_close(loader, &err);
        if(err) {
            g_error_free(err);
            return NULL;
        }

        buf=gdk_pixbuf_loader_get_pixbuf(loader);
        g_object_ref(buf);      /* Ref the image before we unref the loader */
        g_object_unref(loader);
    }

    g_free(data);

    return buf;
}


static cairo_status_t suf_to_bufcb(void *p,
		const unsigned char *data, unsigned int len)
{
	g_memory_input_stream_add_data((GMemoryInputStream *)p,
			g_memdup(data, len), len, g_free);
	return CAIRO_STATUS_SUCCESS;
}
static GdkPixbuf *suf_to_buf(cairo_surface_t *suf)
{
	GInputStream *st = g_memory_input_stream_new();
	cairo_surface_write_to_png_stream(suf, suf_to_bufcb, st);
	GdkPixbuf *ret = gdk_pixbuf_new_from_stream(st, NULL, NULL);
	g_object_unref(st);
	return ret;
}
GdkPixbuf *pixmap_make_lined(GdkPixbuf *src, GdkColor *colour)
{
	if (!(src = src ?: winicon)) return NULL;

	int height = gdk_pixbuf_get_height(src);
	int width  = gdk_pixbuf_get_width(src);

	cairo_surface_t *suf = cairo_image_surface_create(
			CAIRO_FORMAT_ARGB32, width, height);

	cairo_t *cr = cairo_create(suf);

	gdk_cairo_set_source_pixbuf(cr, src, 0, 0);
	cairo_paint(cr);

	gdouble base = height / 14.0;
	gdk_cairo_set_source_color(cr, colour);
	cairo_set_line_width(cr, base * 2);
	cairo_move_to(cr, 0    , height - base * 1.2);
	cairo_line_to(cr, width, height - base * 1.2);
	cairo_stroke(cr);

	cairo_set_line_width(cr, base);
	cairo_move_to(cr, 0    , height - base * 3.4);
	cairo_line_to(cr, width, height - base * 3.4);

	cairo_stroke(cr);

//	GdkPixbuf *ret = gdk_pixbuf_get_from_surface(suf);
	GdkPixbuf *ret = suf_to_buf(suf);

	cairo_destroy(cr);
	cairo_surface_destroy(suf);

	return ret;
}
