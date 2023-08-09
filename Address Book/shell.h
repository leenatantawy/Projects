#ifndef _SHELL_H
#define _SHELL_H

/* ========================= */
/* === interface actions === */
/* ========================= */

void help_text();

void usage();

void shell_prompt();

void greet();

/* shell_error: this is for user errors (as opposed to bugs in code) */
/* (gives all such errors a uniform appearance) */
void shell_error(char *s);

/* shell_error_arg: this is for user errors (as opposed to bugs)
 * - this version takes a string arg for added flexibility
 */
void shell_error_arg(char *fmt, char *s);

#endif /* _SHELL_H */
