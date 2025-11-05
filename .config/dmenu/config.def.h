/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
static int fuzzy  = 1;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching */
static int centered = 0;                    /* -c option; centers dmenu on screen */
static int min_width = 500;                    /* minimum width when centered */
static const float menu_height_ratio = 4.0f;  /* This is the ratio used in the original calculation */
static const unsigned int alpha = 0xff;     /* Amount of opacity. 0xff is opaque             */
static int horizpadbar = 3;                 /* horizontal padding */
static int vertpadbar = 3;                  /* vertical padding */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"SpaceMono Nerd Font Mono:size=10"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */
static const char *colors[SchemeLast][2] = {
	/*     fg         bg       */
	[SchemeNorm] = {"#4d4d4d", "#000000"},
	[SchemeSel] = {"#000000", "#ffd400"},
	[SchemeSelHighlight] = { "#000000", "#a28f5c" },
	[SchemeNormHighlight] = { "#ffc84f", "#4d4d4d" },
	[SchemeOut] = {"#000000", "#f2c94c"},
	[SchemeBorder] = { "#4d4d4d", NULL },
};

static const unsigned int alphas[SchemeLast][2] = {
	[SchemeNorm] = { OPAQUE, alpha },
	[SchemeSel] = { OPAQUE, alpha },
	[SchemeOut] = { OPAQUE, alpha },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 0;
/* -h option; minimum height of a menu line */
static unsigned int lineheight = 0;
static unsigned int min_lineheight = 8;

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";

/* Size of the window border */
static unsigned int border_width = 1;
