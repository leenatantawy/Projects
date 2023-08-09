#include <stdio.h>
#include <stdlib.h>
#include "pos.h"

pos make_pos(unsigned int r, unsigned int c){
    struct pos newpos;
    newpos.r = r;
    newpos.c = c;
    return newpos;
}

