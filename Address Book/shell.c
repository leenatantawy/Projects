#include <stdio.h>
#include <stdlib.h>
#include "shell.h"

/* ========================= */
/* === interface actions === */
/* ========================= */

void help_text()
{
  char p[] = 
    "c [char] // show all cnets beginning with character\n"
    "h        // help\n"
    "l [cnet] // look up a vcard by cnet\n"
    "r [file] // read a file into address book\n"
    "s        // show statistics about address book\n"
    "q        // quit\n\n";
  printf("%s",p);
}

void usage()
{
  char u[] = "usage: adrbook [file]";
  fprintf(stderr,"%s\n",u);
  exit(1);
}

void shell_prompt()
{
  printf("adrbook (h for help) > ");
}

void greet()
{
  printf("***** Welcome to adrbook! *****\n\n");
  help_text();
}

/* shell_error: this is for user errors (as opposed to bugs in code) */
/* (gives all such errors a uniform appearance) */
void shell_error(char *s)
{
  fprintf(stderr,"===> ERROR: %s <===\n",s);
}

/* shell_error_arg: this is for user errors (as opposed to bugs)
 * - this version takes a string arg for added flexibility
 */
void shell_error_arg(char *fmt, char *s)
{
  fprintf(stderr,"===> ERROR: ");
  fprintf(stderr,fmt,s);
  fprintf(stderr," <===\n");
}
