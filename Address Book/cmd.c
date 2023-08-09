#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "shell.h"
#include "cmd.h"

/* === command constructors  === */

/* cmd_new: make a new heap-allocated command with arg set to NULL */
cmd *cmd_new(enum cmd_name name) 
{
  cmd *c = (cmd*)malloc(sizeof(cmd));
  if (c==NULL) {
    fprintf(stderr,"error (cmd_tag): malloc failed\n");
    exit(1);
  }
  c->name=name;
  c->arg=NULL;
  return c;
}

/* cmd_read: make a new heap-allocated READ command */
/* note: copy filename (deep copy) */
cmd *cmd_read(char *filename)
{
  if (!strlen(filename)) {
    shell_error("no filename given with read command");
    return NULL;
  }
  cmd *c = cmd_new(READ);
  c->arg = strdup(filename);
  return c;
}

/* cmd_char: make a new heap-allocated CHAR command */
/* note: copy "character" (actually length 1 string) (deep copy) */
cmd *cmd_char(char *ch)
{
  unsigned int len = strlen(ch);
  if (len==0) {
    shell_error("no character given with char command");
    return NULL;
  } else if (len>1) {
    shell_error("more than one character given with char command");
    return NULL;
  }
  cmd *c = cmd_new(CHAR);
  c->arg = strdup(ch);
  return c;
}

/* cmd_lookup: make a new heap-allocated LOOKUP command */
/* note: copy cnet (deep copy) */
cmd *cmd_lookup(char *cnet)
{
  if (!strlen(cnet)) {
    shell_error("no cnet provided for lookup command");
    return NULL;
  }
  cmd *c = cmd_new(LOOKUP);
  c->arg = strdup(cnet);
  return c;
}

/* === command free === */

/* cmd_free: free command struct and its string, if there is one */
void cmd_free(cmd *c)
{
  if (c) {
    if (c->arg)
      free(c->arg);
    free(c);
  }
}

/* === command display (for debugging, logging) === */

/* cmd_name_tos: return string constant for command name */
/* note: for debugging only; not currently used in shell */
char *cmd_name_tos(cmd *c)
{
  switch (c->name) {
  case QUIT:   return "QUIT";
  case STATS:  return "STATS";
  case READ:   return "READ";
  case CHAR:   return "CHAR";
  case LOOKUP: return "LOOKUP";
  case HELP:   return "HELP";
  }
  /* this should never happen, ever */
  fprintf(stderr,"BUG (cmd_name_tos): unrecognized command name %d\n",c->name);
  exit(1);
}

/* cmd_show: print command to FILE* (typically stderr) */
/* note: for debugging only; not currently used in shell */
void cmd_show(FILE *f, cmd *c)
{
  /* note: cmd_name_tos result does not need to be freed
   * since that function returns pointers to string constants
   */
  if (c->arg)
    fprintf(f,"%s %s\n",cmd_name_tos(c),c->arg);
  else
    fprintf(f,"%s\n",cmd_name_tos(c));
}

/* === command parsing === */

/* cmd_from_string: build a cmd (as defined above) from a string;
 * return NULL if the parse fails
 */
cmd *cmd_from_string(char *s)
{
  char cmdbuf[2];
  char argbuf[99];
  memset(cmdbuf,0,2);
  memset(argbuf,0,99);
  sscanf(s,"%s %s",cmdbuf,argbuf);
  if (strlen(cmdbuf)==1) {
    switch (cmdbuf[0]) {
    case 'q' : return cmd_new(QUIT);
    case 's' : return cmd_new(STATS);
    case 'r' : return cmd_read(argbuf);
    case 'c' : return cmd_char(argbuf);
    case 'l' : return cmd_lookup(argbuf);
    case 'h' : return cmd_new(HELP);
    }
  } 
  /* if we get this far, we couldn't parse the command string */
  return NULL;
}
