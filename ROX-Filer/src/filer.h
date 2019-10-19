/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _FILER_H
#define _FILER_H

enum {
	RESIZE_STYLE = 0,
	RESIZE_ALWAYS = 1,
	RESIZE_NEVER = 2,
};

typedef enum
{
	FILER_NEEDS_RESCAN	= 0x01, /* Call may_rescan after scanning */
	FILER_UPDATING		= 0x02, /* (scanning) items may already exist */
	FILER_CREATE_THUMBS	= 0x04, /* Create thumbs when scan ends */
} FilerFlags;

/* Numbers used in options */
typedef enum
{
	VIEW_TYPE_COLLECTION = 0,	/* Icons view */
	VIEW_TYPE_DETAILS = 1		/* TreeView details list */
} ViewType;

/* Filter types */
typedef enum
{
	FILER_SHOW_ALL,           /* Show all files, modified by show_hidden */
	FILER_SHOW_GLOB,          /* Show files that match a glob pattern */
} FilterType;

/* What to do when all a mount point's windows are closed */
typedef enum {
	UNMOUNT_PROMPT_ASK = GPOINTER_TO_INT(NULL),
	UNMOUNT_PROMPT_NO_CHANGE,
	UNMOUNT_PROMPT_UNMOUNT,
	UNMOUNT_PROMPT_EJECT
} UnmountPrompt;

/* iter's next method has just returned the clicked item... */
typedef void (*TargetFunc)(FilerWindow *filer_window,
			   ViewIter *iter,
			   gpointer data);

struct _FilerWindow
{
	GtkWidget	*window;
	GtkBox		*toplevel_vbox, *view_hbox;
	gboolean	scanning;	/* State of the 'scanning' indicator */
	gchar		*sym_path;		/* Path the user sees */
	gchar		*real_path;		/* realpath(sym_path) */
	ViewIface	*view;
	ViewType	view_type;
	gboolean	temp_item_selected;
	gboolean	show_hidden;
	gboolean	dirs_only;
	gboolean	files_only;
	gboolean	filter_directories;
	FilerFlags	flags;
	SortType	sort_type;
	GtkSortType	sort_order;

	DetailsType	details_type;
	DisplayStyle	display_style;
	DisplayStyle	display_style_wanted;

	Directory	*directory;

	char		*auto_select;	/* If it we find while scanning */

	GtkWidget	*message;	/* The 'Running as ...' message */

	GtkWidget	*minibuffer_area;	/* The hbox to show/hide */
	GtkWidget	*minibuffer_label;	/* The operation name */
	GtkWidget	*minibuffer;		/* The text entry */
	int		mini_cursor_base;	/* XXX */
	MiniType	mini_type;

	FilterType      filter;
	gchar           *filter_string;  /* Glob or regexp pattern */
	gchar           *temp_filter_string;  /* regexp pattern */
	gchar           *regexp;         /* Compiled regexp pattern */
	/* TRUE if hidden files are shown because the minibuffer leafname
	 * starts with a dot.
	 */
	gboolean 	temp_show_hidden;

	TargetFunc	target_cb;
	gpointer	target_data;

	GtkWidget	*toolbar;
	GtkWidget	*toolbar_text;
	GtkWidget	*scrollbar;
	GtkLabel	*toolbar_size_text;
	GtkLabel	*toolbar_settings_text;

	GtkStateType	selection_state;	/* for drawing selection */

	FilerWindow *right_link;
	FilerWindow *left_link;
	guint right_link_idle;
	guint accept_timeout;

	guint pointer_idle;

	gboolean	show_thumbs;
	GQueue		*thumb_queue;		/* paths to thumbnail */
	GtkWidget	*thumb_bar;
	gint64		thumb_bar_time;
	int		max_thumbs;		/* total for this batch */
	int		trying_thumbs;

	gint		auto_scroll;		/* Timer */

	char		*window_id;		/* For remote control */

	/* dir settings */
	gint		reqx;
	gint		reqy;
	/* used by set positon */
	gint		req_width;
	gint		req_height;

	gfloat		icon_scale; /* temporary scale */
	GdkColor	*dir_colour;
	MaskedPixmap *dir_icon;

	gboolean	under_init;
	gboolean	first_scan;
	gboolean	new_win_first_scan;
	gboolean	onlyicon;
	gboolean	req_sort;
	gboolean	may_resize;
	gboolean	presented;

	gint		resize_drag_width; //window width
	gfloat		name_scale_start;
	gint		name_scale_itemw;
	gfloat		name_scale; /* temporary scale */


	/* for checking user resize */
	gint	configured;
	gint	last_width;
	gint	last_height;
};

extern FilerWindow 	*window_with_focus;
extern GList		*all_filer_windows;
extern GHashTable	*child_to_filer;
extern Option		o_filer_auto_resize, o_unique_filer_windows;
extern Option		o_filer_size_limit;
extern Option		o_filer_width_limit;
extern Option		o_fast_font_calc;
extern Option		o_window_link;
extern Option		o_scroll_speed;
extern gint 		fw_font_height;
extern gint 		fw_font_widths[0x7f];
extern gint 		fw_font_widthsb[0x7f];
extern gint 		fw_mono_height;
extern gint 		fw_mono_width;
extern GdkCursor *busy_cursor;

/* Prototypes */
void filer_init(void);
FilerWindow *filer_opendir(const char *path, FilerWindow *src_win,
		const gchar *wm_class, gboolean winlnk);
gboolean filer_update_dir(FilerWindow *filer_window, gboolean warning);
void filer_update_all(void);
void filer_resize_all(gboolean all);
void filer_autosize(FilerWindow *fw);
DirItem *filer_selected_item(FilerWindow *filer_window);
void change_to_parent(FilerWindow *filer_window);
void full_refresh(void);
void filer_openitem(FilerWindow *filer_window, ViewIter *iter, OpenFlags flags);
void filer_check_mounted(const char *real_path);
gboolean filer_close_recursive(char *path); //eats path
void filer_change_to(FilerWindow *filer_window,
			const char *path, const char *from);
gboolean filer_exists(FilerWindow *filer_window);
FilerWindow *filer_get_by_id(const char *id);
void filer_set_id(FilerWindow *, const char *id);
void filer_open_parent(FilerWindow *filer_window);
void filer_detach_rescan(FilerWindow *filer_window);
void filer_target_mode(FilerWindow	*filer_window,
			TargetFunc	fn,
			gpointer	data,
			const char	*reason);
GList *filer_selected_items(FilerWindow *filer_window);
void filer_create_thumb(FilerWindow *filer_window, const gchar *pathname);
void filer_cancel_thumbnails(FilerWindow *filer_window);
void filer_set_title(FilerWindow *filer_window);
void filer_create_thumbs(FilerWindow *filer_window, GPtrArray *items);
void filer_add_tip_details(FilerWindow *filer_window,
			   GString *tip, DirItem *item);
void filer_selection_changed(FilerWindow *filer_window, gint time);
void filer_lost_selection(FilerWindow *filer_window, guint time);
void filer_window_set_size(FilerWindow *filer_window, int w, int h, gboolean ntauto);
gboolean filer_window_delete(GtkWidget *window,
			     GdkEvent *unused,
			     FilerWindow *filer_window);
void filer_set_view_type(FilerWindow *filer_window, ViewType type);
void filer_window_toggle_cursor_item_selected(FilerWindow *filer_window);
void filer_perform_action(FilerWindow *filer_window, GdkEventButton *event);
gint filer_motion_notify(FilerWindow *filer_window, GdkEventMotion *event);
gint filer_key_press_event(GtkWidget *widget, GdkEventKey *event,
			   FilerWindow *filer_window);
void filer_set_autoscroll(FilerWindow *filer_window, gboolean auto_scroll);
void filer_refresh(FilerWindow *filer_window);
void filer_refresh_thumbs(FilerWindow *filer_window);

gboolean filer_match_filter(FilerWindow *filer_window, DirItem *item);
gboolean filer_set_filter(FilerWindow *filer_window,
			  FilterType type, const gchar *filter_string);
void filer_set_filter_directories(FilerWindow *fwin, gboolean filter_directories);
void filer_set_hidden(FilerWindow *fwin, gboolean hidden);
void filer_next_selected(FilerWindow *filer_window, int dir);
void filer_save_settings(FilerWindow *fwin, gboolean parent);
void filer_clear_settings(FilerWindow *fwin);
void filer_copy_settings(FilerWindow *src, FilerWindow *dest);
void filer_link(FilerWindow *left, FilerWindow *right);
void filer_cut_links(FilerWindow *fw, gint side);
void filer_dir_link_next(FilerWindow *fw, GdkScrollDirection dir, gboolean bottom);
void filer_send_event_to_view(FilerWindow *fw, GdkEvent *event);

UnmountPrompt filer_get_unmount_action(const char *path);
void filer_set_unmount_action(const char *path, UnmountPrompt action);
FilerWindow *find_filer_window(const char *sym_path, FilerWindow *diff);

#endif /* _FILER_H */
