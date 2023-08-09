#include <stdlib.h>
#include <stdio.h>
#include "adrbook.h"

const char *_vcard_todo_format = "TODO [vcard]: %s\nhalting\n";

/* vcard_new : allocate a new vcard, copy all strings, return new object
 * note: this is a "deep copy" as opposed to a "shallow copy"; the string 
 *   arguments are not to be shared by the newly allocated vcard
 */
vcard *vcard_new(char *cnet, char *email, char *fname, char *lname, char *tel)
{
    
    vcard* vcardnew = (vcard*)malloc(sizeof(vcard));
    vcardnew -> cnet = strdup(cnet);
    vcardnew -> email = strdup(email);
    vcardnew -> fname = strdup(fname);
    vcardnew -> lname = strdup(lname);
    vcardnew -> tel = strdup(tel);

    return vcardnew;

    /*fprintf(stderr,_vcard_todo_format,"vcard_new");*/
    
}

/* vcard_free : free vcard and the strings it points to
 */
void vcard_free(vcard *c)
{
    free(c->cnet);
    free(c->email);
    free(c->fname);
    free(c->lname);
    free(c->tel);
    free(c);

    /*fprintf(stderr,_vcard_todo_format,"vcard_free");*/
    
}

/* vcard_show : display contents of vcard
 * note: f is the destination of the output, e.g. the screen;
 * our code calls this with stdout, which is where printfs are sent;
 * simply use fprintf rather than printf in this function, and pass in f
 * as its first parameter
 */
void vcard_show(FILE *f, vcard *c)
{
  printf("cnet %s\n", c->cnet);
  printf("email %s\n", c->email);
  printf("fname %s\n", c->fname);
  printf("lname %s\n", c->lname);
  printf("tel %s\n", c->tel);
  /*fprintf(stderr,_vcard_todo_format,"vcard_show");*/
}
