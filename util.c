/*
   $Header: /cvs/src/tdl/util.c,v 1.2 2001/08/21 22:43:25 richard Exp $
  
   tdl - A console program for managing to-do lists
   Copyright (C) 2001  Richard P. Curnow

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

#include "tdl.h"

int count_args(char **x)/*{{{*/
{
  int n = 0;
  while (*x) {
    n++;
    x++;
  }
  return n;
}
/*}}}*/
struct node *lookup_node(char *path, int allow_zero_index, struct node **parent)/*{{{*/
{
  char *p = path;
  int n, nc, idx, tidx, aidx, ncomp, comp10;
  int direction;
  struct links *x = &top;
  struct node *y = NULL;

  ncomp = 1;
  if (parent) *parent = NULL;

  while (*p) {
    n = sscanf(p, "%d%n", &idx, &nc);
    if (n != 1) {
      fprintf(stderr, "Bad path expression found, starting [%s]\n", p);
      exit(1);
    }
    p += nc;
    
    if (idx > 0) {
      direction = 1;
      aidx = idx;
    } else if (idx < 0) {
      direction = 0;
      aidx = -idx;
    } else {
      if (allow_zero_index) {
        if (*p) {
          fprintf(stderr, "Zero index only allowed as last component\n");
          exit(1);
        } else {
          /* This is a special cheat to allow inserting entries at
             the start or end of a chain for the 'above' and
             'below' commands */
          return (struct node *) x;
        }
      } else {
        fprintf(stderr, "Zero in index not allowed\n");
        exit(1);
      }
    }
      
    if (x->next == (struct node *) x) {
      fprintf(stderr, "Path [%s] doesn't exist - tree not that deep\n", path);
      exit(1);
    }

    comp10 = ncomp % 10;

    for (y = direction ? x->next : x->prev, tidx = aidx; --tidx;) {
      
      y = direction ? y->chain.next : y->chain.prev;

      if (y == (struct node *) x) {
        fprintf(stderr, "Can't find entry %d for %d%s component of path %s\n",
                idx, ncomp,
                (comp10 == 1) ? "st" :
                (comp10 == 2) ? "nd" : 
                (comp10 == 3) ? "rd" : "th",
                path);
        exit(1);
      }
    }

    if (*p == '.') {
      p++;
      x = &y->kids;
      if (parent) *parent = y;
    }

    ncomp++;
  }

  return y;
}
/*}}}*/
enum Priority parse_priority(char *priority)/*{{{*/
{
  enum Priority result;
  if (!strcmp(priority, "urgent") || (atoi(priority) >= PRI_URGENT)) {
    result = PRI_URGENT;
  } else if (!strcmp(priority, "high") || (atoi(priority) == PRI_HIGH)) {
    result = PRI_HIGH;
  } else if (!strcmp(priority, "normal") || (atoi(priority) == PRI_NORMAL)) {
    result = PRI_NORMAL;
  } else if (!strcmp(priority, "low") || (atoi(priority) == PRI_LOW)) {
    result = PRI_LOW;
  } else if (!strcmp(priority, "verylow") || (atoi(priority) <= PRI_VERYLOW)) {
    result = PRI_VERYLOW;
  } else {
    fprintf(stderr, "Can't parse new priority '%s'\n", priority);
    exit(1);
  }
  
  return result;
}
/*}}}*/
void clear_flags(struct links *x)/*{{{*/
{
  struct node *y;
  for (y = x->next; y != (struct node *) x; y = y->chain.next) {
    y->flag = 0;
    if (has_kids(y)) {
      clear_flags(&y->kids);
    }
  }
}
/*}}}*/
int has_kids(struct node *x)/*{{{*/
{
  return (x->kids.next != (struct node *) &x->kids);
}
/*}}}*/
struct node *new_node(void)/*{{{*/
{
  struct node *result = new (struct node);
  result->text = NULL;
  result->priority = PRI_NORMAL;
  result->arrived = result->required_by = result->done = 0U;
  result->kids.next = result->kids.prev = (struct node *) &result->kids;
  result->chain.next = result->chain.prev = (struct node *) &result->chain;
  return result;
}
/*}}}*/
void free_node(struct node *x)/*{{{*/
{
  /* FIXME : To be written */


}
/*}}}*/
void append_node(struct node *n, struct links *l)/*{{{*/
{
  n->chain.next = l->next;
  n->chain.prev = (struct node *) l;
  l->next->chain.prev = n;
  l->next = n;
}
/*}}}*/
void prepend_node(struct node *n, struct links *l)/*{{{*/
{
  n->chain.prev = l->prev;
  n->chain.next = (struct node *) l;
  l->prev->chain.next = n;
  l->prev = n;
}
/*}}}*/
void prepend_child(struct node *child, struct node *parent)/*{{{*/
{
  child->parent = parent;
  if (parent) {
    prepend_node(child, &parent->kids);
  } else {
    prepend_node(child, &top);
  }
}
/*}}}*/
