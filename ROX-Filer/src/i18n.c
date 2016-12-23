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

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#ifdef HAVE_LIBINTL_H
#include <libintl.h>
#endif

#include "global.h"

#include "support.h"
#include "choices.h"
#include "options.h"
#include "i18n.h"
#include "gui_support.h"
#include "main.h"

#define MESSAGE _("Note that you must save your choices "		\
		  "and restart the filer for the new language "		\
		  "setting to take full effect.")

/* Two/five-char country_territory code, or NULL */
char *current_lang = NULL;

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/


/* Set things up for internationalisation */
void i18n_init(void)
{
	const char *lang;

	setlocale(LC_ALL, "");

#ifdef HAVE_LIBINTL_H
	gchar *path = g_strdup_printf("%s/Messages", app_dir);
	bindtextdomain("ROX-Filer", path);
	bind_textdomain_codeset("ROX-Filer", "UTF-8");
	g_free(path);
#endif

	/* This is a hang-over from when we did translations ourselves.
	 * Maybe we can get this info from the gettext library?
	 */
	lang = getenv("LANG");
	if (lang)
	{
		const char *end;

		/* Extract the language code from the locale name.
		 * language[_territory][.codeset][@modifier]
		 */

		end = strchr(lang, '.');
		if (!end)
			end = strchr(lang, '@');
		if (end)
			current_lang = g_strndup(lang, end - lang);
		else
			current_lang = g_strdup(lang);
	}
}

