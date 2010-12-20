/*
   $Header: /cvs/src/tdl/list.c,v 1.20.2.1 2004/01/07 00:09:05 richard Exp $
  
   tdl - A console program for managing to-do lists
   Copyright (C) 2001,2002,2003,2004,2005  Richard P. Curnow

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
   */

#include <time.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include "tdl.h"

struct list_options {
  unsigned monochrome:1;
  unsigned show_all:1;
  unsigned show_postponed:1;
  unsigned verbose:1;
  unsigned set_depth:1;
  int depth;
};

#define INDENT_TAB 3

/*{{{ Colour definitions */
static void load_colours_from_env( void );
void colours_init(struct list_options *options);

typedef const char* colour_t;

static struct {
  colour_t *priority;
  colour_t header;
  colour_t normal;
  colour_t days;
  colour_t narrow;
  colour_t kids;
  colour_t is_done;
  colour_t is_ignored;
  colour_t is_postponed;
  colour_t is_deferred;
} colours;

static char *priority_text[] = {
  "UNKNOWN!", "verylow", "low", "normal", "high", "urgent"
};

/*}}}*/
void do_indent(int indent)/*{{{*/
{
  int i;
  for (i=0; i<indent; i++) putchar(' ');
}
/*}}}*/
void do_bullet_indent(int indent)/*{{{*/
{
  int i;
  int n;
  n = indent - 2;
  for (i=0; i<indent; i++) putchar((i == n) ? '-' : ' ');
}
/*}}}*/
static void print_timestamp(int timestamp, char *leader, int indent)/*{{{*/
{
  char buffer[32];
  time_t now, timestamp2;
  long diff, days_ago, days_ahead;
  
  now = time(NULL);
  diff = now - timestamp;
  days_ago = (diff + ((diff > 0) ? 43200 : -43200)) / 86400;
  timestamp2 = (time_t) timestamp;
  strftime(buffer, sizeof(buffer), "%a %d %b %Y %H:%M", 
           localtime(&timestamp2));
  do_indent(indent+2);
  if (days_ago < 0) {
    days_ahead = - days_ago;
    printf("%s%s:%s %s %s(%ld day%s ahead)%s\n", 
	   colours.header, leader, colours.normal, 
	   buffer, 
	   colours.days, days_ahead, (days_ahead == 1) ? "" : "s", colours.normal);
  } else {
    printf("%s%s:%s %s (%ld day%s ago)\n", colours.header, leader, colours.normal, buffer, days_ago, (days_ago == 1) ? "" : "s");
  }
}
/*}}}*/
static void count_kids(struct links *x, int *n_kids, int *n_done_kids, int *n_open_kids)/*{{{*/
{
  int nk, ndk;
  struct node *y;

  nk = ndk = 0;
  for (y = x->next;
       y != (struct node *) x;
       y = y->chain.next) {
    if (y->done) ndk++;
    nk++;
  }

  if (n_kids) *n_kids = nk;
  if (n_done_kids) *n_done_kids = ndk;
  if (n_open_kids) *n_open_kids = (nk - ndk);
  return;
}
/*}}}*/
static void print_details(struct node *y, int indent, int summarise_kids, const struct list_options *options, char *index_buffer, time_t now)/*{{{*/
{
  int is_done;
  int is_ignored;
  int is_postponed;
  int is_deferred;
  char *p;
  int n_kids, n_open_kids;
  int show_state;
  char *narrow_prefix;
  int index_buffer_len;

  is_done = (y->done > 0);
  is_ignored = (y->done == IGNORED_TIME);
  is_postponed = (y->arrived == POSTPONED_TIME);
  is_deferred = (y->arrived > now);
  if (!options->show_all && is_done) return;

  do_indent(indent);
  count_kids(&y->kids, &n_kids, NULL, &n_open_kids);
  show_state = options->show_all || options->show_postponed;
  index_buffer_len = strlen(index_buffer);
  narrow_prefix = get_narrow_prefix();

  if (narrow_prefix) {
    printf("%s%s%s%s", colours.narrow, narrow_prefix, index_buffer_len ? "." : "", colours.normal);
  }
  
  printf("%s%s%s", colours.header, index_buffer, colours.normal);

  if (summarise_kids && (n_kids > 0)) {
    printf(" %s[%d/%d]%s", colours.kids, n_open_kids, n_kids, colours.normal);
  }

  if (show_state && !options->verbose) {
    if (is_ignored) {
      printf(" %s(IGNORED)%s", colours.is_ignored, colours.normal);
    } else if (is_done) {
      printf(" %s(DONE)%s", colours.is_done, colours.normal);
    } else if (is_postponed) {
      printf(" %s(POSTPONED)%s", colours.is_postponed, colours.normal);
    } else if (is_deferred) {
      printf(" %s(DEFERRED)%s", colours.is_deferred, colours.normal);
    }
    printf(" : ");
  } else {
    printf(" ");
  }

  printf("%s", ( is_done ? colours.is_done : 
		 ( is_postponed ? colours.is_postponed : 
		   colours.priority[y->priority])));

#if 0
  if (summarise_kids && (n_kids > 0)) {
      printf("%s%s %s[%d/%d]%s %s%s", 
	     colours.header, index_buffer, 
	     CYAN, n_open_kids, n_kids, colours.normal,
             (show_state && !options->verbose && is_ignored) ? BLUE "(IGNORED) " NORMAL :
             (show_state && !options->verbose && is_done) ? CYAN "(DONE) " NORMAL :
             (show_state && !options->verbose && is_postponed) ? MAGENTA "(POSTPONED) " :
             (show_state && !options->verbose && (y->arrived > now)) ? MAGENTA "(DEFERRED) " : "",
             is_done ? CYAN : is_postponed ? MAGENTA : colours.priority[y->priority]);
  } else {
      printf("%s%s%s %s%s", colours.header, index_buffer, colours.normal,
             (show_state && !options->verbose && is_ignored) ? BLUE "(IGNORED) " NORMAL :
             (show_state && !options->verbose && is_done) ? CYAN "(DONE) " NORMAL :
             (show_state && !options->verbose && is_postponed) ? MAGENTA "(POSTPONED) " :
             (show_state && !options->verbose && (y->arrived > now)) ? MAGENTA "(DEFERRED) " : "",
             is_done ? CYAN : is_postponed ? MAGENTA : colours.priority[y->priority]);
  }
#endif
  for (p = y->text; *p; p++) {
    putchar(*p);
    if (*p == '\n') {
      do_indent(indent + 5);
    }
  }
  printf("%s", colours.normal);
  printf("\n");

  if (options->verbose) {
    print_timestamp(y->arrived, "Arrived", indent);
    do_indent(indent + 2);
    printf("%sPriority: %s%s%s\n",
	   colours.header, colours.priority[y->priority], 
	   priority_text[y->priority], colours.normal);
    if (y->required_by > 0) print_timestamp(y->required_by, "Required by", indent);
    if (y->done > 0) print_timestamp(y->done, "Completed", indent);
    printf("\n");
  }

}
/*}}}*/
static void list_chain(struct links *x, int indent, int depth, const struct list_options *options, char *index_buffer, enum Priority prio, time_t now, unsigned char *hits)/*{{{*/
{
  struct node *y;
  int idx, is_done, is_deferred, is_postponed;
  int show_node;
  char component_buffer[8];
  char new_index_buffer[64];
  
  for (y = x->next, idx = 1;
       y != (struct node *) x;
       y = y->chain.next, ++idx) {
    
    is_done = (y->done > 0);
    is_postponed = (y->arrived == POSTPONED_TIME);
    is_deferred = (y->arrived > now);
    show_node = options->show_all 
             || (options->show_postponed && !is_done)
             || (!is_deferred && !is_postponed);
    if (!show_node) continue;

    sprintf(component_buffer, "%d", idx);
    strcpy(new_index_buffer, index_buffer);
    if (strlen(new_index_buffer) > 0) {
      strcat(new_index_buffer, ".");
    }
    strcat(new_index_buffer, component_buffer);

    if (y->priority >= prio) {
      int summarise_kids = (options->set_depth && (options->depth == depth));
      if (hits[y->iscratch]) {
        print_details(y, indent, summarise_kids, options, new_index_buffer, now);
      }
    }

    /* Maybe list children regardless of priority assigned to parent. */
    if (!options->set_depth || (depth < options->depth)) {
      list_chain(&y->kids, indent + INDENT_TAB, depth + 1, options, new_index_buffer, prio, now, hits);
    }

  }
  return;
}
/*}}}*/
static void allocate_indices(struct links *x, int *idx)/*{{{*/
{
  struct node *y;
  for (y = x->next;
       y != (struct node *) x;
       y = y->chain.next) {

    y->iscratch = *idx;
    ++*idx;
    allocate_indices(&y->kids, idx);
  }
}
/*}}}*/
static void search_node(struct links *x, int max_errors, unsigned long *vecs, unsigned long hitvec, unsigned char *hits)/*{{{*/
{
  struct node *y;
  char *p;
  char *token;
  int got_hit;
  unsigned long r0, r1, r2, r3, nr0, nr1, nr2;

  for (y = x->next;
       y != (struct node *) x;
       y = y->chain.next) {

    token = y->text;

    switch (max_errors) {
      /* optimise common cases for few errors to allow optimizer to keep bitmaps
       * in registers */
      case 0:/*{{{*/
        r0 = ~0;
        got_hit = 0;
        for(p=token; *p; p++) {
          int idx = (unsigned int) *(unsigned char *)p;
          r0 = (r0<<1) | vecs[idx];
          if (~(r0 | hitvec)) {
            got_hit = 1;
            break;
          }
        }
        break;
        /*}}}*/
      case 1:/*{{{*/
        r0 = ~0;
        r1 = r0<<1;
        got_hit = 0;
        for(p=token; *p; p++) {
          int idx = (unsigned int) *(unsigned char *)p;
          nr0 = (r0<<1) | vecs[idx];
          r1  = ((r1<<1) | vecs[idx]) & ((r0 & nr0) << 1) & r0;
          r0  = nr0;
          if (~((r0 & r1) | hitvec)) {
            got_hit = 1;
            break;
          }
        }
        break;
  /*}}}*/
      case 2:/*{{{*/
        r0 = ~0;
        r1 = r0<<1;
        r2 = r1<<1;
        got_hit = 0;
        for(p=token; *p; p++) {
          int idx = (unsigned int) *(unsigned char *)p;
          nr0 =  (r0<<1) | vecs[idx];
          nr1 = ((r1<<1) | vecs[idx]) & ((r0 & nr0) << 1) & r0;
          r2  = ((r2<<1) | vecs[idx]) & ((r1 & nr1) << 1) & r1;
          r0  = nr0;
          r1  = nr1;
          if (~((r0 & r1& r2) | hitvec)) {
            got_hit = 1;
            break;
          }
        }
        break;
  /*}}}*/
      case 3:/*{{{*/
        r0 = ~0;
        r1 = r0<<1;
        r2 = r1<<1;
        r3 = r2<<1;
        got_hit = 0;
        for(p=token; *p; p++) {
          int idx = (unsigned int) *(unsigned char *)p;
          nr0 =  (r0<<1) | vecs[idx];
          nr1 = ((r1<<1) | vecs[idx]) & ((r0 & nr0) << 1) & r0;
          nr2 = ((r2<<1) | vecs[idx]) & ((r1 & nr1) << 1) & r1;
          r3  = ((r3<<1) | vecs[idx]) & ((r2 & nr2) << 1) & r2;
          r0  = nr0;
          r1  = nr1;
          r2  = nr2;
          if (~((r0 & r1 & r2 & r3) | hitvec)) {
            got_hit = 1;
            break;
          }
        }
        break;
        /*}}}*/
      default:
        assert(0); /* not allowed */
        break;
    }
    if (got_hit) {
      hits[y->iscratch] = 1;
    }
    search_node(&y->kids, max_errors, vecs, hitvec, hits);
  }
}
/*}}}*/
static void merge_search_condition(unsigned char *hits, int n_nodes, char *cond)/*{{{*/
{
  /* See "Fast text searching with errors, Sun Wu and Udi Manber, TR 91-11,
     University of Arizona.  I have been informed that this algorithm is NOT
     patented.  This implementation of it is entirely the work of Richard P.
     Curnow - I haven't looked at any related source (webglimpse, agrep etc) in
     writing this.
  */

  int max_errors;
  char *slash;
  char *substring;
  unsigned long a[256];
  unsigned long hit;
  int len, i;
  char *p;
  unsigned char *hit0;

  slash = strchr(cond, '/');
  if (!slash) {
    max_errors = 0;
    substring = cond;
  } else {
    substring = new_string(cond);
    substring[slash-cond] = '\0';
    max_errors = atoi(slash+1);
    if (max_errors > 3) {
      fprintf(stderr, "Can only match with up to 3 errors, ignoring patterh <%s>\n", cond);
      goto get_out;
    }
  }

  len = strlen(substring);
  if (len < 1 || len > 31) {
    fprintf(stderr, "Pattern must be between 1 and 31 characters\n");
    goto get_out;
  }
  
  /* Set array 'a' to all -1 values */
  memset(a, 0xff, 256 * sizeof(unsigned long));
  for (p=substring, i=0; *p; p++, i++) {
    unsigned char pc;
    pc = *(unsigned char *) p;
    a[(unsigned int) pc] &= ~(1UL << i);
    /* Make search case insensitive */
    if (isupper(pc)) {
      a[tolower((unsigned int) pc)] &= ~(1UL << i);
    }
    if (islower(pc)) {
      a[toupper((unsigned int) pc)] &= ~(1UL << i);
    }
  }
  hit = ~(1UL << (len-1));

  hit0 = new_array(unsigned char, n_nodes);
  memset(hit0, 0, n_nodes);
  
  /* Now scan each node against this match criterion */
  search_node(&top, max_errors, a, hit, hit0);
  for (i=0; i<n_nodes; i++) {
    hits[i] &= hit0[i];
  }
  free(hit0);

get_out:
  if (substring != cond) {
    free(substring);
  }
  return;
}
/*}}}*/
int process_list(char **x)/*{{{*/
{
  struct list_options options;
  int options_done = 0;
  int any_paths = 0;
  char index_buffer[256];
  char *y;
  enum Priority prio = PRI_NORMAL, prio_to_use, node_prio;
  int prio_set = 0;
  time_t now = time(NULL);
  
  unsigned char *hits;
  int node_index, n_nodes;

  options.monochrome = 0;
  options.show_all = 0;
  options.show_postponed = 0;
  options.verbose = 0;
  options.set_depth = 0;

  if ( (getenv("TDL_LIST_MONOCHROME") != NULL) ||
       (isatty(STDOUT_FILENO) == 0) ) {
    options.monochrome = 1;
  }
  
  /* Initialisation to support searching */
  node_index = 0;
  allocate_indices(&top, &node_index);
  n_nodes = node_index;

  hits = n_nodes ? new_array(unsigned char, n_nodes) : NULL;

  /* all nodes match until proven otherwise */
  memset(hits, 1, n_nodes);
  
  while ((y = *x) != 0) {
    /* An argument starting '1' or '+1' or '+-1' (or '-1' after '--') is
     * treated as the path of the top node to show */
    if (isdigit(y[0]) ||
        (y[0] == '.') ||
        (options_done && (y[0] == '-') && isdigit(y[1])) ||
        ((y[0] == '+') &&
         (isdigit(y[1]) ||
          ((y[1] == '-' && isdigit(y[2])))))) {
      
      struct node *n = lookup_node(y, 0, NULL);
      int summarise_kids;

      if (!n) return -1;
      
      any_paths = 1;
      index_buffer[0] = '\0';
      strcat(index_buffer, y);
      summarise_kids = (options.set_depth && (options.depth==0));
      if (hits[n->iscratch]) {
        print_details(n, 0, summarise_kids, &options, index_buffer, now);
      }
      if (!options.set_depth || (options.depth > 0)) {
        node_prio = n->priority;

        /* If the priority has been set on the cmd line, always use that.
         * Otherwise, use the priority from the specified node, _except_ when
         * that is higher than normal, in which case use normal. */
        prio_to_use = (prio_set) ? prio : ((node_prio > prio) ? prio : node_prio);
        list_chain(&n->kids, INDENT_TAB, 0, &options, index_buffer, prio_to_use, now, hits);
      }
    } else if ((y[0] == '-') && (y[1] == '-')) {
      options_done = 1;
    } else if (y[0] == '-') {
      while (*++y) {
        switch (*y) {
          case 'v':
            options.verbose = 1;
            break;
          case 'a':
            options.show_all = 1;
            break;
          case 'm':
            options.monochrome = 1;
            break;
          case 'p':
            options.show_postponed = 1;
            break;
          case '1': case '2': case '3':
          case '4': case '5': case '6': 
          case '7': case '8': case '9':
            options.set_depth = 1;
            options.depth = (*y) - '1';
            break;
          default:
            fprintf(stderr, "Unrecognized option : -%c\n", *y);
            break;
        }
      }
    } else if (y[0] == '/') {
      /* search expression */
      merge_search_condition(hits, n_nodes, y+1);
       
    } else {
      int error;
      prio = parse_priority(y, &error);
      if (error < 0) return error;
      prio_set = 1;
    }

    x++;
  }
  
  colours_init(&options);

  if (!any_paths) {
    struct node *narrow_top = get_narrow_top();
    if (narrow_top) {
      index_buffer[0] = 0;
      if (hits[narrow_top->iscratch]) {
        int summarise_kids = (options.set_depth && (options.depth==0));
        print_details(narrow_top, 0, summarise_kids, &options, index_buffer, now);
      }
      if (!options.set_depth || (options.depth > 0)) {
        list_chain(&narrow_top->kids, 0, 1, &options, index_buffer, prio, now, hits);
      }
    } else {
      index_buffer[0] = 0;
      list_chain(&top, 0, 0, &options, index_buffer, prio, now, hits);
    }
  }

  if (hits) free(hits);
  
  return 0;
}
/*}}}*/

static const colour_t RED = "[31m[1m";
static const colour_t GREEN = "[32m";
static const colour_t YELLOW = "[33m[1m";
static const colour_t BLUE = "[34m";
static const colour_t MAGENTA = "[35m";
static const colour_t CYAN = "[36m";
static const colour_t NORMAL = "[0m";
/*static const colour_t DIM = "[37m[2m";*/
/*static const colour_t DIMCYAN = "[36m[2m";*/

void colours_init(struct list_options *options)/*{{{*/
{
  /* Table to map priority levels to colours */
  static colour_t priority_colours[] =
    { NULL, NULL, NULL, NULL, NULL, NULL };

  colours.priority = priority_colours;

  if ( options->monochrome ) {
    static const colour_t NO_COLOUR = "";
    priority_colours[PRI_UNKNOWN] = NO_COLOUR;
    priority_colours[PRI_VERYLOW] = NO_COLOUR;
    priority_colours[PRI_LOW]     = NO_COLOUR;
    priority_colours[PRI_NORMAL]  = NO_COLOUR;
    priority_colours[PRI_HIGH]    = NO_COLOUR;
    priority_colours[PRI_URGENT]  = NO_COLOUR;

    colours.header   = NO_COLOUR;
    colours.normal   = NO_COLOUR;    
  } else {
    priority_colours[PRI_UNKNOWN] = NORMAL;
    priority_colours[PRI_VERYLOW] = BLUE;
    priority_colours[PRI_LOW]     = CYAN;
    priority_colours[PRI_NORMAL]  = NORMAL;
    priority_colours[PRI_HIGH]    = YELLOW;
    priority_colours[PRI_URGENT]  = RED;

    colours.normal   = NORMAL;
    
    colours.header   = GREEN;
    colours.days = MAGENTA;
    colours.narrow = BLUE;
    colours.kids = CYAN;
    colours.is_done = CYAN;
    colours.is_ignored = BLUE;
    colours.is_postponed = MAGENTA;
    colours.is_deferred = MAGENTA;

    load_colours_from_env();      
  } 
}
/*}}}*/

static const char * const COLOUR_ENV_VAR = "TDL_LIST_COLOURS";

char *parse_colour_text(const char *text, size_t length) { /*{{{*/

  /* Sanity check */
  if ( length == 0 )
    return NULL;

  if ( isalpha (*text) ) {
    /* Look for some popular colour names */
    if ( strncasecmp ("red", text, length) == 0 ) {
      return strdup(RED);
    } else if ( strncasecmp ("blue", text, length) == 0 ) {
      return strdup(BLUE);
    } else if ( strncasecmp ("green", text, length) == 0 ) {
      return strdup(GREEN);
    } else if ( strncasecmp ("yellow", text, length) == 0 ) {
      return strdup(YELLOW);
    } else if ( strncasecmp ("magenta", text, length) == 0 ) {
      return strdup(MAGENTA);
    } else if ( strncasecmp ("cyan", text, length) == 0 ) {
      return strdup(CYAN);
    } else {
      fprintf(stderr, "In environment '%s' unknown colour '%.*s'\n",
	      COLOUR_ENV_VAR, length, text);
      return NULL;
    }
  } else {
    /* Parse a colour specifier that looks like this : 
     *    [0-9][0-9]?(;[0-9][0-9]?)?
     * For exampe:
     *    "0", "00", "00;0", "00;00" 
     * are all valid.
     * Result for each of the above would be in the form:
     *    "<esc>[0m", "<esc>[00m", 
     *	  "<esc>[00m<esc>[0m", "<esc>[00m<esc>[00m"
     */
    char *colour_spec = (char*)malloc(sizeof(char) * 11);
    if ( colour_spec == NULL )
      return NULL;
    
    char *ptr = colour_spec;

    int had_semicolon = 0;
    int digits_in_block = 0;
    int digit_blocks = 0;
      
    *ptr++ = '';
    *ptr++ = '[';

    const char *end = text + length;
    const char *src = text;

    do {
      /* If we have a digit, and we've not had either too many in a
	 row, or too many groups of digits in total, then copy this
	 into the colour specifier and increment the counters. */
      if ( isdigit (*src) && 
	   (digits_in_block < 2) && 
	   (digit_blocks < 2)) 
      {
	if ( digits_in_block == 0 )
	  digit_blocks++;
	digits_in_block++;
	*ptr++ = *src++;
      } 
      /* If we have a semicolon, and it's our only one, and we've had
	 some digits already then start the second part of the colour
	 specifier. */
      else if ( !had_semicolon && 
		  (*src == ';') && 
		  (digits_in_block > 0) ) 
      {
	src++;   /* Skip the ";" */
	*ptr++ = 'm';
	*ptr++ = '';
	*ptr++ = '[';
	digits_in_block = 0;
	had_semicolon = 1; /* Only one ";" allowed */
      } 
      /* Everything else is a broken colour definition */
      else 
      {
	fprintf (stderr, "In environment '%s', invalid colour '%.*s'\n",
		 COLOUR_ENV_VAR, length, text);
	free (colour_spec);
	return NULL;
      }
    } while ( src < end );
    *ptr++ = 'm';
    *ptr++ = '\0';

    return colour_spec;
  }

  return NULL;
}
/*}}}*/

static void load_colours_from_env( void ) { /*{{{*/
  const char* env_colours = getenv(COLOUR_ENV_VAR);
  if ( env_colours == 0 ) 
    return;

  const char *c = env_colours;
  
  while (*c != '\0') {

    /* Parse "<ATTR>=<colour-spec>:" We allow the final ':' to be
     * missing.  If we get confused, don't understand an <ATTR> or
     * <colour-spec> then we will scan ahead to the next ':' and try
     * to pick up from there.
     */

    char *attr_name = 0;   // Start of attr name
    size_t attr_len = 0;      // Length of the attr
  
    char *colour_text = 0; // Start of colour-spec
    size_t colour_len = 0;    // Length of colour-specxs

    const char *tmp = c;

    /* Move to first non alpha character */
    while ( isalpha(*c) || *c == '_' ) 
      c++;

    attr_len = c - tmp;

    /* Now create a NULL terminated copy of the attr name */
    attr_name = strndup(tmp, attr_len);
    if ( attr_name == NULL )
      return;

    /* Check we're now at an '=' and skip it */
    if ( *c != '=' ) {
      fprintf(stderr, "In environment '%s', '%s' not followed by '='\n", 
	      COLOUR_ENV_VAR, attr_name);
      free(attr_name);

      /* Move forward until either end of string or ':' */
      while ( (*c != ':') && (*c != '\0') )
	c++;
      
      /* We're done if we're at the end of the string */
      if ( *c == '\0' )
	return;
      
      /* Skip the ':' */
      c++;

      continue;
    }
    c++;
    
    /* Now to gather the colour spec */
    tmp = c;
    while ( (*c != ':') && (*c != '\0') )
      c++;
    
    colour_len = c - tmp;  

    /* Create a null terminated copy of colour spec */
    colour_text = parse_colour_text(tmp, colour_len);

    /* parse colour text can return NULL if it doesn't understand
       the colour, or if it's out of memory. In both cases we 
       don't try to set any colour here, then continue. */

    if ( colour_text != NULL ) {
      /* Now find the right attribute to apply the colour to */
      if ( strcasecmp ("HEADER", attr_name) == 0 ) {
	colours.header = colour_text;
      } else if ( strcasecmp ("KIDS", attr_name) == 0 ) {
	colours.kids = colour_text;
      } else if ( strcasecmp ("DAYS", attr_name) == 0 ) {
	colours.days = colour_text;
      } else if ( strcasecmp ("NARROW", attr_name) == 0 ) {
	colours.narrow = colour_text;
      } else if ( strcasecmp ("IS_DONE", attr_name) == 0 ) {
	colours.is_done = colour_text;
      } else if ( strcasecmp ("IS_IGNORED", attr_name) == 0 ) {
	colours.is_ignored = colour_text;
      } else if ( strcasecmp ("IS_POSTPONED", attr_name) == 0 ) {
	colours.is_postponed = colour_text;
      } else if ( strcasecmp ("IS_DEFERRED", attr_name) == 0 ) {
	colours.is_deferred = colour_text;
      } else if ( strcasecmp ("PRI_URGENT", attr_name) == 0 ) {
	colours.priority[PRI_URGENT] = colour_text;
      } else if ( strcasecmp ("PRI_HIGH", attr_name) == 0 ) {
	colours.priority[PRI_HIGH] = colour_text;
      } else if ( strcasecmp ("PRI_LOW", attr_name) == 0 ) {
	colours.priority[PRI_LOW] = colour_text;
      } else if ( strcasecmp ("PRI_VLOW", attr_name) == 0 ) {
	colours.priority[PRI_VERYLOW] = colour_text;
      } else {
	fprintf(stderr, "In environment '%s', '%s' unknown\n", 
		COLOUR_ENV_VAR, attr_name);
	free (colour_text);
      }
    }

    /* Now release memory */
    free (attr_name);

    /* Skip the ':' */ 
    if ( *c == ':' )
      c++;
  }
}
/*}}}*/
