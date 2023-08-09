#include <stdio.h>
#include <stdlib.h>
#include "board.h"




board* board_new(unsigned int width, unsigned int height, enum type type){

       board* boardnew = (board*)malloc(sizeof(board));
       boardnew -> width = width;
       boardnew -> height = height;
       boardnew -> type = type;
       unsigned int length = 1;
       unsigned int bits = width * height;

       switch(type){
        case MATRIX:
            if (width == 0 | height == 0){
                fprintf(stderr, "board dimensions can not be 0, n x 0 and 0 x n/n");
                return NULL;
                /* exit(1) */
            } else{
                enum cell** matrixnew = (cell**)malloc(height * sizeof(cell*));
                unsigned int x, y;
                for (x = 0; x < height; x++){
                      matrixnew[x] = (cell*)malloc(width * sizeof(cell));
                    for (y= 0; y < width; y++){
                        matrixnew[x][y] = EMPTY;
                    }
                }
                boardnew -> u.matrix = matrixnew;
            }       
            return boardnew;

        case BITS:
            if (bits % 32 == 0){
                length = (bits/32);
            }else{
                length = (bits/32) + 1;
            }
            unsigned int* bitsnew = (unsigned int*)malloc(length * sizeof(unsigned int));
            for (int i = 0; i < length; i ++){
                bitsnew[i] = 0;
            }
            boardnew -> u.bits = bitsnew;
            return boardnew;
     }
}

void board_free(board* b){
    unsigned int height = b -> height;
    unsigned int x;
    switch (b -> type){
        case BITS:
            free(b -> u.bits);
        case MATRIX:
            for (x = 0; x < height; x++){
                    free (b -> u.matrix[x]);
            }
            free(b -> u.matrix);
    }
    free(b);
}

void board_show(board* b){
        if (b-> type == MATRIX){
            int x = 0;
            unsigned int i = 0;
            int y = 0;
            unsigned int j = 0;
            unsigned int height = b -> height;
            unsigned int width = b -> width;
            char s;
    
            for (y = 0; y < height + 2; y++){
                if (y == 0){
                    for (x = 0; x <= width; x++){
                        if (x == 0){
                            printf("  ");
                        }else { 
                            if (x <= 10){
                                printf("%d", x - 1);
                            }else if ((x-1) > 9 && (x-1) <= 36){
                                s = x + 54;
                                printf("%c", s);
                                s = 0;
                            }else if ((x-1) > 36 && (x-1) <= 62){
                                s = x + 60;
                                printf("%c", s);
                                s = 0;
                            }else{
                                s = 63;
                                printf("%c", s);
                            }    
                        }   
                    }
                } else if (y == 1){
                    printf("\n");
                }else{
                    i = y-2;
                    if (i <= 9){
                        printf("%d", i);
                    }else if ( i > 9 && i <= 36){
                        s = i + 55;
                        printf("%c", s);
                        s = 0;
                    }else if (i > 36 && i <= 62){
                        s = i + 61;
                        printf("%c", s);
                        s = 0;
                    }else{
                        s = 63;
                        printf("%c", s);
                    }
                    printf(" ");
                    for (j = 0; j < width; j++){
                        enum cell color = b -> u.matrix[y-2][j];
                        if (color == EMPTY){
                            printf(".");
                        } else if (color == WHITE){
                            printf("o");
                        } else{
                            printf("*");
                        }
                    }
                }
                printf("\n");
              }
        }else{
            int x = 0;
            unsigned int i = 0;
            int y = 0;
            unsigned int j = 0;
            unsigned int height = b -> height;
            unsigned int width = b -> width;
            char s;

            for (y = 0; y < height + 2; y++){
                if (y == 0){
                    for (x = 0; x <= width; x++){
                        if (x == 0){
                            printf("  ");
                        }else {
                            if (x <= 10){
                                printf("%d", x - 1);
                            }else if ((x-1) > 9 && (x-1) <= 36){
                                s = x + 54;
                                printf("%c", s);
                                s = 0;
                            }else if ((x-1) > 36 && (x-1) <= 62){
                                s = x + 60;
                                printf("%c", s);
                                s = 0;
                            }else{
                                s = 63;
                                printf("%c", s);
                            }
                        }
                    }
                } else if (y == 1){
                    printf("\n");
                }else{
                    i = y-2;
                    if (i <= 9){
                        printf("%d", i);
                    }else if ( i > 9 && i <= 36){
                        s = i + 55;
                        printf("%c", s);
                        s = 0;
                    }else if (i > 36 && i <= 62){
                        s = i + 61;
                        printf("%c", s);
                        s = 0;
                    }else{
                        s = 63;
                        printf("%c", s);
                    }
                    printf(" ");
                    for (j = 0; j < width; j++){
                        enum cell color = board_get(b, make_pos(y-2, j));
                        if (color == EMPTY){
                            printf(".");
                        } else if (color == WHITE){
                            printf("o");
                        } else{
                            printf("*");
                        }
                    }
                }
                printf("\n");
            }

    }

} 
                
          


cell board_get(board* b, pos p){
    unsigned int width = b-> width;
    unsigned int height = b-> height;

    if (b -> type == MATRIX){
            if (p.r > height | p.c > width){
                fprintf(stderr, "position requested is out of bounds.\n");
                exit(1);
            }
            return b -> u.matrix[p.r][p.c];

    }else{
             if (p.r > b-> height | p.c > b-> width){
                fprintf(stderr, "position requested is out of bounds.\n");
                exit(1);
            }else{
   
                int bpos = (2*((p.r)*(width) + (p.c))) % 32;
                    
                int bindex = 0;

                bindex = (2*((p.r)*(width) + (p.c))) / 32;
                
                unsigned int bit_check = (b->u.bits[bindex]) & (0x3 << bpos);
                
                if ((bit_check >> bpos) == 0){
                    return EMPTY;
                }else if ((bit_check >> bpos) == 1){
                    return BLACK;
                }else{
                    return WHITE;
                }   
        }           
    
    }
}

void board_set(board* b, pos p, cell c){
        unsigned int height = b-> height;
        unsigned int width = b -> width;

        if (b -> type == MATRIX){
            if (p.r > (height) | p.c > (width)){
                fprintf(stderr, "position requested is out of bounds.\n");
                exit(1);
            }
            b -> u.matrix[p.r][p.c] = c;
        }else{
            int bpos = (2*((p.r)*(width) + (p.c))) % 32;
            int bindex = 0;

            bindex = (2*((p.r)*(width) + (p.c))) / 32;
            

            b -> u.bits[bindex] = (b->u.bits[bindex]) & ~(0x3 << bpos);

            b -> u.bits[bindex] = (b->u.bits[bindex]) | (c << bpos);
            
    }
}


