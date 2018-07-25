/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
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

/* dir.c - directory scanning and caching */

/* How it works:
 *
 * A Directory contains a list DirItems, each having a name and some details
 * (size, image, owner, etc).
 *
 * There is a list of file names that need to be rechecked. While this
 * list is non-empty, items are taken from the list in an idle callback
 * and checked. Missing items are removed from the Directory, new items are
 * added and existing items are updated if they've changed.
 *
 * When a whole directory is to be rescanned:
 *
 * - A list of all filenames in the directory is fetched, without any
 *   of the extra details.
 * - This list is compared to the current DirItems, removing any that are now
 *   missing.
 * - Each window onto the directory is asked which items it will actually
 *   display, and the union of these sets is the new recheck list.
 *
 * This system is designed to get the number of items and their names quickly,
 * so that the auto-sizer can make a good guess. It also prevents checking
 * hidden files if they're not going to be displayed.
 *
 * To get the Directory object, use dir_cache, which will automatically
 * trigger a rescan if needed.
 *
 * To get notified when the Directory changes, use the dir_attach() and
 * dir_detach() functions.
 */

#include "config.h"

#include <gtk/gtk.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "global.h"

#include "dir.h"
#include "diritem.h"
#include "support.h"
#include "dir.h"
#include "filer.h"
#include "fscache.h"
#include "mount.h"
#include "pixmaps.h"
#include "type.h"
#include "main.h"
#include "options.h"

/* For debugging. Can't detach when this is non-zero. */
static int in_callback = 0;

GFSCache *dir_cache = NULL;

static Option o_purge_dir_cache;
static Option o_close_dir_when_missing;

/* Static prototypes */
static void update(Directory *dir, gchar *pathname, gpointer data);
static void set_idle_callback(Directory *dir);
static DirItem *insert_item(Directory *dir, const guchar *leafname, gboolean examine_now);
static void dir_recheck(Directory *dir,
			const guchar *path, const guchar *leafname);
static GPtrArray *hash_to_array(GHashTable *hash);
static void dir_force_update_item(Directory *dir,
		const gchar *leaf, gboolean thumb);
static void dir_scan(Directory *dir);


void dir_init(void)
{
	option_add_int(&o_purge_dir_cache, "purge_dir_cache", FALSE);
	option_add_int(&o_close_dir_when_missing, "close_dir_when_missing", FALSE);

	dir_cache = g_fscache_new((GFSLoadFunc) dir_new,
				(GFSUpdateFunc) update, NULL);
}


static gint rescan_timeout_cb(gpointer data)
{
	Directory *dir = (Directory *) data;

	dir->rescan_timeout = -1;
	if (!dir->scanning && dir->needs_update)
		dir_scan(dir);

	return FALSE;
}

static void rescan_soon(Directory *dir)
{
	dir->needs_update = TRUE;
	if (dir->rescan_timeout != -1) return;
	dir->rescan_timeout = g_timeout_add(300, rescan_timeout_cb, dir);
}
static void monitorcb(GFileMonitor *m, GFile *f,
		GFile *o, GFileMonitorEvent e, Directory *dir)
{
	//don't rescan untile G_FILE_MONITOR_EVENT_CHANGES_DONE_HINT
	if (e != G_FILE_MONITOR_EVENT_CHANGED)
		rescan_soon(dir);
}

/* Periodically calls callback to notify about changes to the contents
 * of the directory.
 * Before this function returns, it calls the callback once to add all
 * the items currently in the directory (unless the dir is empty).
 * It then calls callback(DIR_QUEUE_INTERESTING) to find out which items the
 * caller cares about.
 * If we are not scanning, it also calls callback(DIR_END_SCAN).
 */
void dir_attach(Directory *dir, DirCallback callback, gpointer data)
{
	DirUser	*user;
	GPtrArray *items;

	g_return_if_fail(dir != NULL);
	g_return_if_fail(callback != NULL);

	user = g_new(DirUser, 1);
	user->callback = callback;
	user->data = data;

	if (!dir->users)
	{
		GFile *gf = g_file_new_for_path(dir->pathname);
		dir->monitor = g_file_monitor_directory(gf,
				G_FILE_MONITOR_WATCH_MOUNTS, //doesn't work?
				NULL, NULL);
		g_object_unref(gf);

		g_signal_connect(dir->monitor, "changed", G_CALLBACK(monitorcb), dir);
	}

	dir->users = g_list_prepend(dir->users, user);

	g_object_ref(dir);

	items = hash_to_array(dir->known_items);
	if (items->len)
		callback(dir, DIR_ADD, items, data);
	g_ptr_array_free(items, TRUE);

	if ((dir->needs_update || dir->error) && !dir->scanning)
	{
		dir->needs_update = FALSE;
		dir_scan(dir);
	}
	else
		callback(dir, DIR_QUEUE_INTERESTING, NULL, data);

	/* May start scanning if noone was watching before */
	set_idle_callback(dir);

	if (!dir->scanning)
		callback(dir, DIR_END_SCAN, NULL, data);
}

/* Undo the effect of dir_attach */
void dir_detach(Directory *dir, DirCallback callback, gpointer data)
{
	DirUser	*user;
	GList	*list;

	g_return_if_fail(dir != NULL);
	g_return_if_fail(callback != NULL);
	g_return_if_fail(in_callback == 0);

	for (list = dir->users; list; list = list->next)
	{
		user = (DirUser *) list->data;
		if (user->callback == callback && user->data == data)
		{
			g_free(user);
			dir->users = g_list_remove(dir->users, user);
			g_object_unref(dir);

			/* May stop scanning if noone's watching */
			set_idle_callback(dir);

			if (!dir->users)
				g_clear_object(&dir->monitor);

			if (o_purge_dir_cache.int_value && !dir->users)
				g_fscache_remove(dir_cache, dir->pathname);

			return;
		}
	}

	g_warning("dir_detach: Callback/data pair not attached!\n");
}

void dir_update(Directory *dir, gchar *pathname)
{
	update(dir, pathname, NULL);
}

/* Rescan this directory */
void refresh_dirs(const char *path)
{
	g_fscache_update(dir_cache, path);
}

/* When something has happened to a particular object, call this
 * and all appropriate changes will be made.
 */
void dir_check_this(const guchar *path)
{
	guchar	*real_path;
	guchar	*dir_path;
	Directory *dir;
	gchar	*base;

	dir_path = g_path_get_dirname(path);
	real_path = pathdup(dir_path);
	g_free(dir_path);

	dir = g_fscache_lookup_full(dir_cache, real_path,
					FSCACHE_LOOKUP_PEEK, NULL);
	if (dir)
	{
		base = g_path_get_basename(path);
		dir_recheck(dir, real_path, base);
		g_free(base);
		g_object_unref(dir);
	}

	g_free(real_path);
}

/* Used when we fork an action child, otherwise we can't delete or unmount
 * any directory which we're watching via dnotify!  inotify does not have
 * this problem
 */
void dir_drop_all_notifies(void)
{
}

/* Tell watchers that this item has changed, but don't rescan.
 * (used when thumbnail has been created for an item. also an user icon on a sym_path)
 */
void dir_force_update_path(const gchar *path, gboolean icon)
{
	gchar	*dir_path;
	Directory *dir;
	gchar 	*base;

	g_return_if_fail(path[0] == '/');

	dir_path = g_path_get_dirname(path);

	dir = g_fscache_lookup_full(dir_cache, dir_path, FSCACHE_LOOKUP_PEEK,
			NULL);
	if (dir)
	{
		base = g_path_get_basename(path);
		dir_force_update_item(dir, base, icon);
		g_free(base);
		g_object_unref(dir);
	}

	g_free(dir_path);
}

/* Ensure that 'leafname' is up-to-date. Returns the new/updated
 * DirItem, or NULL if the file no longer exists.
 */
DirItem *dir_update_item(Directory *dir, const gchar *leafname)
{
	DirItem *item;

	time(&diritem_recent_time);
	item = insert_item(dir, leafname, TRUE);
	dir_merge_new(dir);

	return item;
}

/* Add item to the recheck_list if it's marked as needing it.
 * Item must have ITEM_FLAG_NEED_RESCAN_QUEUE.
 * Items on the list will get checked later in an idle callback.
 */
void dir_queue_recheck(Directory *dir, DirItem *item)
{
	item->flags &= ~ITEM_FLAG_NEED_RESCAN_QUEUE;
	g_mutex_lock(&dir->mutex);
	g_queue_push_head(dir->recheck_list, g_strdup(item->leafname));
	g_mutex_unlock(&dir->mutex);
}

static void free_recheck_list(Directory *dir)
{
	g_queue_free_full(dir->recheck_list, g_free);
	dir->recheck_list = NULL;
}

static void tousers(Directory *dir, DirAction action, GPtrArray *items)
{
	in_callback++;
	for (GList *next = dir->users; next; next = next->next)
	{
		DirUser *user = (void *) next->data;
		user->callback(dir, action, items, user->data);
	}
	in_callback--;
}

/* If scanning state has changed then notify all filer windows */
static void dir_set_scanning(Directory *dir, gboolean scanning)
{
	if (scanning == dir->scanning)
		return;

	dir->scanning = scanning;
	tousers(dir, scanning ? DIR_START_SCAN : DIR_END_SCAN, NULL);

#if 0
	/* Useful for profiling */
	if (!scanning)
	{
		g_print("Done\n");
		exit(0);
	}
#endif
}

/* Notify everyone that the error status of the directory has changed */
static void dir_error_changed(Directory *dir)
{
	tousers(dir, DIR_ERROR_CHANGED, NULL);
}

static void stop_scan_t(Directory *dir)
{
	if (dir->t_scan)
	{
		dir->in_scan_thread = FALSE;
		g_thread_join(dir->t_scan);
		dir->t_scan = NULL;
	}
}

static void stop_scan(gpointer key, gpointer data, gpointer user_data)
{
	GFSCacheData *fsdata = (GFSCacheData *) data;
	Directory *dir = (Directory *) fsdata->data;

	stop_scan_t(dir);
	dir_set_scanning(dir, FALSE);
}

void dir_stop(void)
{
	g_hash_table_foreach(dir_cache->inode_to_stats, stop_scan, NULL);
}

static const guchar *make_path_to_buf(GString *buffer, const char *dir, const char *leaf)
{
	g_string_assign(buffer, dir);

	if (dir[0] != '/' || dir[1] != '\0')
		g_string_append_c(buffer, '/');	/* For anything except "/" */

	g_string_append(buffer, leaf);

	return buffer->str;
}

static gint notify_timeout(gpointer data)
{
	Directory	*dir = (Directory *) data;

	dir_merge_new(dir);

	dir->notify_active = 0;
	g_object_unref(dir);

	return FALSE;
}

/* Call dir_merge_new() after a while. */
static void delayed_notify(Directory *dir, gboolean mainthread)
{
	if (!mainthread)
	{
		dir->req_notify = TRUE;
		return;
	}

	if (dir->notify_active)
		return;

	g_object_ref(dir);

	if (dir->notify_time < DIR_NOTIFY_TIME)
		dir->notify_time += DIR_NOTIFY_TIME / 4;

	dir->notify_active = g_timeout_add(dir->notify_time, notify_timeout, dir);
}

/* This is called in the background when there are items on the
 * dir->recheck_list to process.
 */
static gboolean do_recheck(gpointer data)
{
	Directory *dir = (Directory *) data;
	guchar	*leaf;

	g_return_val_if_fail(dir != NULL, FALSE);
	g_return_val_if_fail(dir->recheck_list != NULL, FALSE);

	if (!g_queue_is_empty(dir->recheck_list))
	{
		g_mutex_lock(&dir->mutex);
		leaf = g_queue_pop_tail(dir->recheck_list);
		g_mutex_unlock(&dir->mutex);

		DirItem *item = insert_item(dir, leaf, FALSE);
		if (item && item->flags & ITEM_FLAG_DIR_NEED_EXAMINE)
			g_queue_push_head(dir->examine_list, item);

		g_free(leaf);

		if (g_queue_is_empty(dir->recheck_list))
			dir->req_scan_off = TRUE;

		return TRUE;
	}

	if (!g_queue_is_empty(dir->examine_list))
	{
		DirItem *item = g_queue_pop_tail(dir->examine_list);

		if (item->flags & ITEM_FLAG_DIR_NEED_EXAMINE)
		{
			g_mutex_lock(&dir->mutex);
			if (diritem_examine_dir(
						make_path_to_buf(dir->strbuf, dir->pathname, item->leafname), item))
			{
				g_mutex_lock(&dir->mergem);
				g_ptr_array_add(dir->up_items, item);
				g_mutex_unlock(&dir->mergem);
				delayed_notify(dir, FALSE);
			}
			g_mutex_unlock(&dir->mutex);
		}

		if (!g_queue_is_empty(dir->examine_list))
			return TRUE;
	}

	return FALSE;
}

static void dir_rescan_later(Directory *dir)
{
	if (dir->rescan_timeout != -1)
		g_source_remove(dir->rescan_timeout);

	dir->rescan_timeout = g_timeout_add(
			(g_get_monotonic_time() - dir->last_scan_time) / 1000 * 4 + 600,
			rescan_timeout_cb, dir);
}

static GMutex callbackm;
static gboolean recheck_callback(gpointer data)
{
	Directory *dir = (Directory *) data;

	//waiting for attach until dir->idle_callback is set
	g_thread_yield();
	g_mutex_lock(&callbackm);
	g_source_remove(dir->idle_callback);
	dir->idle_callback = 0;
	g_mutex_unlock(&callbackm);

	GThread *t = dir->t_scan;
	g_object_unref(dir);

	if (!t) return FALSE; //cancelled

	if (dir->req_scan_off)
	{
		dir->req_scan_off = FALSE;

		if (dir->notify_active)
		{
			g_source_remove(dir->notify_active);
			notify_timeout(dir);
		}
		if (dir->req_notify)
		{
			dir->req_notify = FALSE;
			dir_merge_new(dir);
		}

		dir_set_scanning(dir, FALSE);
		dir->notify_time = 0;
	}

	if (!dir->in_scan_thread)
	{
		g_thread_join(dir->t_scan);
		dir->t_scan = NULL;

		if (!g_queue_is_empty(dir->recheck_list) ||
			!g_queue_is_empty(dir->examine_list)
			)
		{
			while (do_recheck(data)) {}
		}

		dir_set_scanning(dir, FALSE);
	}

	if (dir->req_notify)
	{
		dir->req_notify = FALSE;
		delayed_notify(dir, TRUE);
	}

	if (!dir->in_scan_thread && dir->needs_update)
		dir_rescan_later(dir);

	return FALSE;
}

static void attach_callback(Directory *dir)
{
	g_mutex_lock(&callbackm);
	if (!dir->idle_callback)
	{
		g_object_ref(dir);
		GSource *src = g_idle_source_new();
		g_source_set_callback(src, recheck_callback, dir, NULL);
		dir->idle_callback = g_source_attach(src, NULL);
		g_source_unref(src);
	}
	g_mutex_unlock(&callbackm);
	g_thread_yield();
}

static gpointer scan_thread(gpointer data)
{
	Directory *dir = (Directory *) data;

	gboolean ret = TRUE;

	if (g_queue_is_empty(dir->recheck_list))
		dir->req_scan_off = TRUE;

	while (ret)
	{
		if (!dir->in_scan_thread) break;

		ret = do_recheck(data);

		if (dir->req_notify || dir->req_scan_off)
			attach_callback(dir);
	}

	dir->notify_time = 0;
	dir->in_scan_thread = FALSE;
	attach_callback(dir);

	return NULL;
}

static GPtrArray *swap_ptra(GPtrArray *src){
	GPtrArray *ret = g_ptr_array_sized_new(src->len);

	g_free(ret->pdata);
	ret->pdata = g_memdup(src->pdata, sizeof(gpointer) * src->len);
	ret->len = src->len;

	g_ptr_array_set_size(src, 0);

	return ret;
}

/* Add all the new items to the items array.
 * Notify everyone who is watching us.
 */
void dir_merge_new(Directory *dir)
{
	g_mutex_lock(&dir->mergem);

	GPtrArray *new = swap_ptra(dir->new_items);
	GPtrArray *up = swap_ptra(dir->up_items);
	GPtrArray *gone = swap_ptra(dir->gone_items);

	g_mutex_unlock(&dir->mergem);
	g_thread_yield();

	GList	  *list;
	guint	  i, j;

	in_callback++;

	if (gone->len && new->len)
		for (i = 0; i < new->len; i++)
			for (j = gone->len; j--;)
				if (i < new->len && new->pdata[i] == gone->pdata[j])
				{
					j = gone->len;
					g_ptr_array_remove_index_fast(new, i);
				}

	for (list = dir->users; list; list = list->next)
	{
		DirUser *user = (DirUser *) list->data;

		if (up->len)
			user->callback(dir, DIR_UPDATE, up, user->data);
		if (gone->len)
			user->callback(dir, DIR_REMOVE, gone, user->data);
		if (new->len)
			user->callback(dir, DIR_ADD, new, user->data);
	}

	in_callback--;

	for (i = 0; i < gone->len; i++)
	{
		DirItem	*item = (DirItem *) gone->pdata[i];
		diritem_free(item);
	}

	g_ptr_array_free(new, TRUE);
	g_ptr_array_free(up, TRUE);
	g_ptr_array_free(gone, TRUE);
}


/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

static void free_items_array(GPtrArray *array)
{
	guint	i;

	for (i = 0; i < array->len; i++)
	{
		DirItem	*item = (DirItem *) array->pdata[i];

		diritem_free(item);
	}

	g_ptr_array_free(array, TRUE);
}

static gboolean compare_items(DirItem  *item, DirItem  *old)
{
	if (item->lstat_errno == old->lstat_errno
	 && item->base_type == old->base_type
	 && item->flags == old->flags
	 && item->size == old->size
	 && item->mode == old->mode
	 && item->atime == old->atime
	 && item->ctime == old->ctime
	 && item->mtime == old->mtime
	 && item->uid == old->uid
	 && item->gid == old->gid
	 && item->mime_type == old->mime_type
	 && (old->_image == NULL || di_image(item) == old->_image)
	 && (item->label == NULL || (
			   item->label->red   == old->label->red
			&& item->label->green == old->label->green
			&& item->label->blue  == old->label->blue)))
		return TRUE;
	return FALSE;
}

/* Stat this item and add, update or remove it.
 * Returns the new/updated item, if any.
 * (leafname may be from the current DirItem item)
 * Ensure diritem_recent_time is reasonably up-to-date before calling this.
 */
static DirItem *insert_item(Directory *dir, const guchar *leafname, gboolean examine_now)
{
	const gchar *full_path;
	DirItem     *item;

	if (leafname[0] == '.' && (leafname[1] == '\n' ||
		(leafname[1] == '.' && leafname[2] == '\n')))
		return NULL;

	//this called from multi threads e.g. dir_check_this
	g_mutex_lock(&dir->mutex);
	full_path = make_path_to_buf(dir->strbuf, dir->pathname, leafname);

	item = g_hash_table_lookup(dir->known_items, leafname);

	if (item)
	{
		DirItem  old = {};
		gboolean do_compare = FALSE;	/* (old is filled in) */

		if (item->base_type != TYPE_UNKNOWN)
		{
			/* Preserve the old details so we can compare */
			old = *item;
			old.flags &= ~ITEM_FLAG_NEED_RESCAN_QUEUE;
			do_compare = TRUE;
		}
		diritem_restat(full_path, item, &dir->stat_info, examine_now);

		if (item->base_type == TYPE_ERROR && item->lstat_errno == ENOENT)
		{
			/* Item has been deleted */
			if (g_hash_table_remove(dir->known_items, item->leafname))
			{
				g_mutex_lock(&dir->mergem);
				g_ptr_array_add(dir->gone_items, item);
				g_mutex_unlock(&dir->mergem);
			}

			item = NULL;
		}
		else
		{
			if (item->flags & ITEM_FLAG_DIR_NEED_EXAMINE)
				old.flags |= ITEM_FLAG_DIR_NEED_EXAMINE;

			if (do_compare && compare_items(item, &old))
				goto out;

			g_mutex_lock(&dir->mergem);
			g_ptr_array_add(dir->up_items, item);
			g_mutex_unlock(&dir->mergem);
		}
	}
	else
	{
		item = diritem_new(leafname);
		diritem_restat(full_path, item, &dir->stat_info, examine_now);
		if (item->base_type == TYPE_ERROR && item->lstat_errno == ENOENT)
		{
			diritem_free(item);
			item = NULL;
			goto out;
		}

		if (g_hash_table_insert(dir->known_items, item->leafname, item))
		{
			g_mutex_lock(&dir->mergem);
			g_ptr_array_add(dir->new_items, item);
			g_mutex_unlock(&dir->mergem);
		}
	}

	delayed_notify(dir, examine_now);
out:
	g_mutex_unlock(&dir->mutex);
	g_thread_yield();
	return item;
}

static void update(Directory *dir, gchar *pathname, gpointer data)
{
	g_free(dir->pathname);
	dir->pathname = pathdup(pathname);

	if (dir->scanning)
		dir->needs_update = TRUE;
	else
		dir_scan(dir);
}

/* If there is work to do, set the idle callback.
 * Otherwise, stop scanning and unset the idle callback.
 */
static void set_idle_callback(Directory *dir)
{
	if (dir->users &&
			(!g_queue_is_empty(dir->recheck_list) ||
			 !g_queue_is_empty(dir->examine_list)))
	{
		/* Work to do, and someone's watching */

		if (dir->t_scan)
			return;

		time(&diritem_recent_time);
		dir_set_scanning(dir, TRUE);

		dir->req_scan_off = FALSE;
		dir->in_scan_thread = TRUE;
		dir->req_notify = FALSE;
		dir->t_scan = g_thread_new("rescan_t", scan_thread, dir);
	}
	else
	{
		dir_set_scanning(dir, FALSE);
		stop_scan_t(dir);
	}
}

/* See dir_force_update_path() */
static void dir_force_update_item(Directory *dir,
		const gchar *leaf, gboolean icon)
{
	DirItem *item = g_hash_table_lookup(dir->known_items, leaf);
	if (!item) return;

	GPtrArray *items = g_ptr_array_new();
	g_ptr_array_add(items, item);

	tousers(dir, icon ? DIR_UPDATE_ICON : DIR_UPDATE, items);

	g_ptr_array_free(items, TRUE);
}

static void dir_recheck(Directory *dir,
			const guchar *path, const guchar *leafname)
{
	guchar *old = dir->pathname;

	dir->pathname = g_strdup(path);
	g_free(old);

	time(&diritem_recent_time);
	insert_item(dir, leafname, TRUE);
}

static void to_array(gpointer key, gpointer value, gpointer data)
{
	GPtrArray *array = (GPtrArray *) data;

	g_ptr_array_add(array, value);
}

/* Convert a hash table to an unsorted GPtrArray.
 * g_ptr_array_free() the result.
 */
static GPtrArray *hash_to_array(GHashTable *hash)
{
	GPtrArray *array;

	array = g_ptr_array_sized_new(g_hash_table_size(hash));

	g_hash_table_foreach(hash, to_array, array);

	return array;
}

static gpointer parent_class;

/* Note: dir_cache is never purged, so this shouldn't get called */
static void dir_finialize(GObject *object)
{
	GPtrArray *items;
	Directory *dir = (Directory *) object;

	g_return_if_fail(dir->users == NULL);

	//g_print("[ dir finalize ]\n");

	free_recheck_list(dir);
	g_queue_free(dir->examine_list);
	set_idle_callback(dir);
	if (dir->rescan_timeout != -1)
		g_source_remove(dir->rescan_timeout);

	dir_merge_new(dir);	/* Ensures new, up and gone are empty */

	g_ptr_array_free(dir->up_items, TRUE);
	g_ptr_array_free(dir->new_items, TRUE);
	g_ptr_array_free(dir->gone_items, TRUE);

	items = hash_to_array(dir->known_items);
	free_items_array(items);
	g_hash_table_destroy(dir->known_items);

	g_string_free(dir->strbuf, TRUE);
	g_mutex_clear(&dir->mutex);
	g_mutex_clear(&dir->mergem);

	g_free(dir->error);
	g_free(dir->pathname);

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void directory_class_init(gpointer gclass, gpointer data)
{
	GObjectClass *object = (GObjectClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	object->finalize = dir_finialize;
}

static void directory_init(GTypeInstance *object, gpointer gclass)
{
	Directory *dir = (Directory *) object;

	g_mutex_init(&dir->mutex);
	g_mutex_init(&dir->mergem);
	dir->strbuf = g_string_new(NULL);

	dir->known_items = g_hash_table_new(g_str_hash, g_str_equal);
	dir->recheck_list = g_queue_new();
	dir->examine_list = g_queue_new();
	dir->idle_callback = 0;
	dir->t_scan = NULL;
	dir->req_scan_off = FALSE;
	dir->in_scan_thread = FALSE;
	dir->req_notify = FALSE;
	dir->scanning = FALSE;
	dir->have_scanned = FALSE;

	dir->users = NULL;
	dir->needs_update = TRUE;
	dir->notify_active = 0;
	dir->notify_time = 1;
	dir->pathname = NULL;
	dir->error = NULL;
	dir->rescan_timeout = -1;
	dir->monitor = NULL;

	dir->new_items = g_ptr_array_new();
	dir->up_items = g_ptr_array_new();
	dir->gone_items = g_ptr_array_new();
}

static GType dir_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (DirectoryClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			directory_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(Directory),
			0,			/* n_preallocs */
			directory_init
		};

		type = g_type_register_static(G_TYPE_OBJECT, "Directory",
					      &info, 0);
	}

	return type;
}

Directory *dir_new(const char *pathname)
{
	Directory *dir;

	dir = g_object_new(dir_get_type(), NULL);

	dir->pathname = g_strdup(pathname);

	return dir;
}

static gboolean check_delete(gpointer key, gpointer value, gpointer data)
{
	DirItem	*item = (DirItem *) value;
	Directory *dir = (Directory *) data;
	if (item->flags & ITEM_FLAG_NOT_DELETE)
		item->flags &= ~ITEM_FLAG_NOT_DELETE;
	else
	{
		g_ptr_array_add(dir->gone_items, item);
		return TRUE;
	}
	return FALSE;
}

/* Get the names of all files in the directory.
 * Remove any DirItems that are no longer listed.
 * Replace the recheck_list with the items found.
 */
static void dir_scan(Directory *dir)
{
	g_return_if_fail(dir != NULL);

	stop_scan_t(dir);

	dir->last_scan_time = g_get_monotonic_time();

	const char *pathname = dir->pathname;
	gboolean isupdate = dir->needs_update && !dir->error;
	dir->needs_update = FALSE;
	mount_update(FALSE);

	if (dir->error)
	{
		null_g_free(&dir->error);
		dir_error_changed(dir);
	}

	/* Saves statting the parent for each item... */
	if (mc_stat(pathname, &dir->stat_info))
	{
		if (o_close_dir_when_missing.int_value && isupdate)
			g_idle_add((GSourceFunc)filer_close_recursive, g_strdup(dir->pathname));
		else
		{
			dir->error = g_strdup_printf(_("Can't stat directory: %s"),
					g_strerror(errno));
			dir_error_changed(dir);
		}
		return;		/* Report on attach */
	}

	DIR *d = mc_opendir(pathname);
	if (!d)
	{
		dir->error = g_strdup_printf(_("Can't open directory: %s"),
				g_strerror(errno));
		dir_error_changed(dir);
		return;		/* Report on attach */
	}

	dir_set_scanning(dir, TRUE);
	gdk_flush();

	if (dir->have_scanned)
	{
		free_recheck_list(dir);
		dir->recheck_list = g_queue_new();
		g_queue_clear(dir->examine_list);
	}

	struct dirent *ent;
	while ((ent = mc_readdir(d)))
	{
		if (ent->d_name[0] == '.')
		{
			if (ent->d_name[1] == '\0')
				continue;		/* Ignore '.' */
			if (ent->d_name[1] == '.' && ent->d_name[2] == '\0')
				continue;		/* Ignore '..' */
		}

		DirItem *old;
		if (dir->have_scanned &&
				(old = g_hash_table_lookup(dir->known_items, ent->d_name)))
		{
			/* ITEM_FLAG_NEED_RESCAN_QUEUE is cleared when the item is added
			 * to the rescan list.
			 */
			old->flags |= ITEM_FLAG_NEED_RESCAN_QUEUE | ITEM_FLAG_NOT_DELETE;

		}
		else
		{
			DirItem *new;

			new = diritem_new(ent->d_name);
			g_ptr_array_add(dir->new_items, new);
			g_hash_table_insert(dir->known_items, new->leafname, new);
		}
	}
	mc_closedir(d);

	if (dir->have_scanned)
		/* Remove all items and add to gone list */
		g_hash_table_foreach_remove(dir->known_items, check_delete, dir);

	dir_merge_new(dir);

	/* Ask everyone which items they need to display, and add them to
	 * the recheck list. Typically, this means we don't waste time
	 * scanning hidden items.
	 */
	tousers(dir, DIR_QUEUE_INTERESTING, NULL);

	dir->have_scanned = TRUE;

	set_idle_callback(dir);

	dir_check_this(dir->pathname);
}
