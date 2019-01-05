/*
 * ROX-Filer, filer for the ROX desktop project
 * Thomas Leonard, <tal197@users.sourceforge.net>
 */


#ifndef _DIRITEM_H
#define _DIRITEM_H

#include <sys/types.h>

extern time_t diritem_recent_time;

typedef enum
{
	ITEM_FLAG_SYMLINK 	= 0x01,	/* Is a symlink */
	ITEM_FLAG_APPDIR  	= 0x02,	/* Contains an AppRun */
	ITEM_FLAG_MOUNT_POINT  	= 0x04,	/* Is mounted or in fstab */
	ITEM_FLAG_MOUNTED  	= 0x08,	/* Is mounted */
	ITEM_FLAG_EXEC_FILE  	= 0x20,	/* File, and has an X bit set (or is a .desktop)*/
	ITEM_FLAG_NOT_DELETE	= 0x40, /* Not Delete */
	ITEM_FLAG_RECENT	= 0x80, /* [MC]-time is around now */

	/* DirItems are created with this flag set. Restatting or queuing an
	 * item in this state clears the flag. This is to prevent an item
	 * being added to the queue more than once at a time.
	 */
	ITEM_FLAG_NEED_RESCAN_QUEUE = 0x100,
	ITEM_FLAG_IN_RESCAN_QUEUE   = 0x1000,
	ITEM_FLAG_NEED_EXAMINE = 0x200,
	ITEM_FLAG_IN_EXAMINE   = 0x2000,
	ITEM_FLAG_GONE = 0x4000,

	ITEM_FLAG_CAPS      = 0x400,
	ITEM_FLAG_HAS_XATTR = 0x800, /* Has extended attributes set */
} ItemFlags;

struct _DirItem
{
	char		*leafname;
	char		*collatekey; /* Preprocessed for sorting */
	int		base_type;
	int		flags;
	int		lstat_errno;	/* 0 if details are valid */
	mode_t		mode;
	off_t		size;
	time_t		atime, ctime, mtime;
	MaskedPixmap	*_image;	/* NULL => leafname only so far */
	MIME_type	*mime_type;
	GdkColor	*label;
	uid_t		uid;
	gid_t		gid;
};

void diritem_init(void);
DirItem *diritem_new(const guchar *leafname);
void diritem_restat(const guchar *path, DirItem *item, struct stat *parent, gboolean examine_now);
MaskedPixmap *_diritem_get_image(DirItem *item, gboolean mainthread);
void diritem_free(DirItem *item);
gboolean diritem_examine_dir(const guchar *path, DirItem *item);

static inline MaskedPixmap *di_image(DirItem *item)
{
	return _diritem_get_image(item, TRUE);
}

#endif /* _DIRITEM_H */
