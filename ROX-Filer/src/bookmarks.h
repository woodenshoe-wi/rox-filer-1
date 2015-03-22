/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _BOOKMARKS_H
#define _BOOKMARKS_H

void bookmarks_show_menu(FilerWindow *filer_window, GtkWidget *widget);
void bookmarks_edit(void);
void bookmarks_add_history(const gchar *path);
void bookmarks_add_uri(const EscapedPath *uri);
gchar *bookmarks_get_recent(void);
gchar *bookmarks_get_top(void);

#endif /* _BOOKMARKS_H */
