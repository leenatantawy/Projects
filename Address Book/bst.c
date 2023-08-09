#include <stdlib.h>
#include <stdio.h>
#include "adrbook.h"

const char *_bst_todo_format = "TODO [bst]: %s\nhalting\n";

bst *bst_singleton(vcard *c)
{
    bst* res = (bst*)malloc(sizeof(bst));
    res-> left = NULL;
    res-> right = NULL;
    res-> c = c;
    return res;
}

int bst_insert(bst *t, vcard *c)
{   
    if (t == NULL){
        return 0;
    }else if (t->c == NULL){
        fprintf(stderr, "empty BST");
        exit(1);
    }else if (strcmp((t->c->cnet),(c->cnet)) > 0){
        if (t->left == NULL){
            t->left = bst_singleton(c);
            return 1;
        }else{
            return bst_insert(t -> left, c);
        }
    }else if (strcmp((t->c->cnet),(c->cnet)) < 0){
        if (t -> right == NULL){
            t -> right = bst_singleton(c);
            return 1;
        }else{
            return bst_insert(t -> right, c);
        }
    }else{
        return 0;
    }
}

unsigned int bst_num_entries(bst *t)
{
    if(t == NULL){
        return 0;
    }else{
         return 1 + bst_num_entries(t->left) + bst_num_entries(t->right);
    }
}

unsigned int bst_height(bst *t)
{
  if (t == NULL){
    return 0;
  } else{
     int bstleft = bst_height(t -> left);
     int bstright = bst_height(t -> right);
     if (bstright > bstleft){
        return 1 + bstright;
    }else{
        return 1 + bstleft;
    }
 } 
}


vcard *bst_search(bst *t, char *cnet, int *n_comparisons)
{
  
  if (t == NULL){
    return NULL;
  } else if (strcmp((t->c->cnet), cnet) > 0){
    (*n_comparisons)++;
    return bst_search(t->left, cnet, n_comparisons);
  } else if (strcmp((t->c->cnet),cnet) == 0) {
    return t-> c;
  }else if (strcmp((t->c->cnet), cnet) < 0){
    (*n_comparisons)++;
    return bst_search(t->right, cnet, n_comparisons);
  }else{
    return NULL;
}
}
/* note: f is the destination of the output, e.g. the screen;
 * our code calls this with stdout, which is where printfs are sent;
 * simply use fprintf rather than printf in this function, and pass in f
 * as its first parameter
 */
unsigned int bst_c(FILE *f, bst *t, char c){ 
  char cmpr = t->c->cnet[0];
  if( t == NULL){
    return 0;
  }else if (cmpr < c){
    return bst_c(f, t->left, c);
  } else if (cmpr > c){
    return bst_c(f, t-> right, c);
  }else {
    fprintf(f,"%s",t->c->cnet);
    return 1 + bst_c(f, t-> right, c) + bst_c(f, t-> left, c);
  }
}

void bst_free(bst *t)
{
    if (t == NULL){
        free(t);
    }else{
        vcard_free(t -> c);
        bst_free(t -> left);
        bst_free(t -> right);
    }
}
