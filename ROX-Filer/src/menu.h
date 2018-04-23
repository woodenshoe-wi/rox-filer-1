/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _MENU_H
#define _MENU_H

/* 'action's for menu_rox_help */
enum {HELP_ABOUT, HELP_DIR, HELP_MANUAL};

typedef enum menu_icon_style {
  MIS_NONE, MIS_SMALL, MIS_LARGE,
  MIS_HUGE_UNUSED,
  MIS_CURRENT, /* As per current filer window */
  MIS_DEFAULT
} MenuIconStyle;

extern GtkAccelGroup	*filer_keys;

void menu_init(void);
MenuIconStyle get_menu_icon_style(void);

typedef void (*MenuCB)();
typedef void (*MenuCB1)(
		gpointer callback_data, guint callback_action, GtkWidget *widget);

GtkWidget *menu_add_item(gchar *label, MenuCB cb, guint action);
GtkWidget *menu_add_stock(
		gchar *label, MenuCB cb, guint action, const gchar *stock_id);
void menu_add_separator(void);
GtkWidget *menu_start(gchar *label, GtkWidget *parent);

void menu_set_items_shaded(GtkWidget *menu, gboolean shaded, int from, int n);
void position_menu(GtkMenu *menu, gint *x, gint *y,
		   gboolean  *push_in, gpointer data);
GtkWidget *menu_make_image(DirItem *ditem, MenuIconStyle style);
void show_popup_menu(GtkWidget *menu, GdkEvent *event, int item);

gboolean ensure_filer_menu(void);
void show_filer_menu(FilerWindow *filer_window,
		     GdkEvent *event,
		     ViewIter *item);
void menu_popdown(void);
MIME_type *menu_selection_type(FilerWindow *fw);
GList *menu_sendto_for_type(GList *paths, MIME_type *type);

/* For "New" toolbar button */
void show_new_directory(FilerWindow *filer_window);
void show_new_file(FilerWindow *filer_window);
void show_menu_new(FilerWindow *filer_window);

/* Public menu handlers */
void menu_rox_help(gpointer data, guint action, GtkWidget *widget);
void menu_show_options(gpointer data, guint action, GtkWidget *widget);
void open_home(gpointer data, guint action, GtkWidget *widget);
void menu_show_shift_action(GtkWidget *menu_item, DirItem *item, gboolean next);

#endif /* _MENU_H */
