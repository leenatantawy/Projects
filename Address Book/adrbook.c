#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "shell.h"
#include "cmd.h"
#include "adrbook.h"

#define BUFFER_SIZE 256

/* =================================== */
/* === execution of shell commands === */
/* =================================== */

/* === actions for each command === */

/* do_stats: print statistics of given address book */
void do_stats(bst *address_book)
{
  printf("Statistics of current address book:\n");
  printf("* entries in address book:      %d\n", 
	 bst_num_entries(address_book));
  printf("* height of binary search tree: %d\n\n", 
	 bst_height(address_book));
}

/* do_read: build address book from given file if possible;
 * return NULL if file doesn't exist
 * note: this is brittle; has little to no error handling
 */
bst *do_read(char *infile)
{
  vcard *c;
  char buf0[BUFFER_SIZE], buf1[BUFFER_SIZE], 
       buf2[BUFFER_SIZE], buf3[BUFFER_SIZE], 
       buf4[BUFFER_SIZE];
  bst *address_book = NULL;
  FILE *f = fopen(infile,"r");
  if (f==NULL) {
    shell_error_arg("File not found: \"%s\".",infile);
    return NULL;
  }
  while (!feof(f)) {
    /* this particular file format is baked in */
    fscanf(f,"%s\t%s\t%s\t%s\t%s\n",buf0,buf1,buf2,buf3,buf4);    
    c = vcard_new(buf0,buf1,buf2,buf3,buf4);
    if (address_book) {
      int i = bst_insert(address_book,c);
      if (i==0) /* c isn't pointed to by tree; free it */
	vcard_free(c);
    }
    else
      address_book = bst_singleton(c);
  }
  fclose(f);
  if (address_book) {
    printf("Read file %s.\n", infile);
    do_stats(address_book);
  }
  return address_book;
}

/* do_char: execute the CHAR command: display all vcards
 * whose cnet starts with c
 */
void do_char(char c,bst *address_book)
{
  unsigned int z = bst_c(stdout,address_book,c);
  printf("\n(found %u CNETs starting with '%c')\n\n",z,c);
}

/* do_lookup: look for the given CNET, report on findings */
void do_lookup(char *cnet, bst *address_book)
{
  int n_comparisons = 0;
  vcard *c = bst_search(address_book, cnet, &n_comparisons);  
  if (c)
    vcard_show(stdout,c);   
  else
    printf("cnet \"%s\" not found in current address book.\n", cnet); 
  printf("\n(%d comparisons made in search.)\n\n", n_comparisons);
}

/* do_cmd: execute the given command
 * note: address_book is of type bst** so it can be modified in READ
 */
void do_cmd(cmd *c, bst **address_book, int *quit)
{
  bst *tmp; /* for reading in new address book if needed */
  switch (c->name) {
  /* available commands are QUIT, STATS, CHAR, LOOKUP, HELP, READ */
  case QUIT:   *quit=1; break;
  case STATS:  do_stats(*address_book); break;
  case CHAR:   do_char(c->arg[0],*address_book); break;
  case LOOKUP: do_lookup(c->arg,*address_book);  break;    
  case HELP:   help_text(); break;
  case READ:
    tmp = do_read(c->arg);
    if (tmp) {
      bst_free(*address_book);
      *address_book = tmp;
    }
    else {
      shell_error_arg("could not read file %s, address book unchanged",c->arg);
      putchar('\n');
    }
    break;
  default:
    /* this shouldn't happen, ever */
    fprintf(stderr,"BUG (do_cmd): bad tag in cmd (%d)\n",c->name);
    exit(1);
  }    
}

/* ===================================== */
/* === administer the adrbook shell  === */
/* ===================================== */

/* trim_newline: duplicate the string and replace the newline 
 * at the end with '\0' if it exists. 
 */
char *trim_newline(char *s)
{
  int n = strlen(s);
  char *t = strdup(s);
  if (s[n-1]=='\n')
    t[n-1] = '\0';
  return t;
}

int main(int argc, char *argv[])
{
  int quit=0;
  bst *address_book=NULL;
  char *cmd_string;
  char user_input[BUFFER_SIZE];
  memset(user_input,'\0',BUFFER_SIZE);

  /* if too many args given, print usage and exit */
  if (argc>2)
    usage();

  /* say hello and load file, if one is named */
  greet();
  if (argc==2)
    /* if a file name given, read it in */
    address_book=do_read(argv[1]);

  while (!quit) {
    shell_prompt();
    fgets(user_input, BUFFER_SIZE, stdin);
    cmd_string = trim_newline(user_input);
    putchar('\n');
    if (!strcmp(cmd_string,""))
      continue;
    cmd *c = cmd_from_string(cmd_string);
    if (!c) {
      shell_error_arg("unrecognized or malformed command: \"%s\"", cmd_string);
      putchar('\n');
      help_text();
    } else
      do_cmd(c,&address_book,&quit);
    if (cmd_string)
      free(cmd_string);
    cmd_free(c);
  }

  /* clean up and exit */
  bst_free(address_book);
  printf("Bye!\n");
  return 0;
}

/* A note to coders who have read this code with interest:
 * one typically doesn't reinvent the wheel to write a shell
 * from scratch like you see here. For more information, please
 * investigate the Command Line Editor Library (libedit). -ams
 */
