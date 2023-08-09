# Makefile for CS152 Autumn 2022 Lab 7

.PHONY: clean

adrbook: cmd.h cmd.c shell.h shell.c adrbook.h bst.c vcard.c adrbook.c
	clang -g -o adrbook -Wall vcard.c bst.c shell.c cmd.c adrbook.c -lm

# type 'make clean' to remove the executable and debug information
clean:
	rm -rf a.out adrbook *.dSYM
