#  $Header: /cvs/src/tdl/Attic/Makefile,v 1.1 2001/08/19 22:12:10 richard Exp $
#  
#  tdl - A console program for managing to-do lists
#  Copyright (C) 2001  Richard P. Curnow
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA

prefix=/usr/local
bindir=$(prefix)/bin
man1dir=$(prefix)/man/man1

CC=gcc
#CFLAGS=-g -Wall
CFLAGS=-O2 -Wall
OBJ = main.o io.o

tdl : $(OBJ)
	$(CC) $(CFLAGS) -o tdl $(OBJ)


%.o : %.c
	$(CC) $(CFLAGS) -c $<

%.s : %.c
	$(CC) $(CFLAGS) -S $<

clean:
	rm -f tdl *.o core

install:
	[ -d $(prefix) ] || mkdir $(prefix)
	[ -d $(bindir) ] || mkdir $(bindir)
	[ -d $(man1dir) ] || mkdir $(man1dir)
	cp tdl $(bindir)/tdl
	chown root $(bindir)/tdl
	chmod 555 $(bindir)/tdl
	ln -s $(bindir)/tdl $(bindir)/tdla
	ln -s $(bindir)/tdl $(bindir)/tdll
	ln -s $(bindir)/tdl $(bindir)/tdld
	cp tdl.1 $(man1dir)/tdl.1
	chown root $(man1dir)/tdl.1
	chmod 444 $(man1dir)/tdl.1

