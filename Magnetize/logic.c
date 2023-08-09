#include <stdio.h>
#include <stdlib.h>
#include "logic.h"

game* new_game(unsigned int square, unsigned int maglock, unsigned int width, unsigned int height, enum type type){

    game* gamenew = (game*)malloc(sizeof(game));
    if (square > width | square > height){
        fprintf(stderr, "square is bigger than width or height so game is impractical.\n");
        /*exit(1);*/
    }
    gamenew -> square = square;
    gamenew -> maglock = maglock;
    gamenew -> b = board_new(width, height, type);
    gamenew -> player= WHITES_TURN;
    gamenew -> black_rem = 0;
    gamenew -> white_rem = 0;
    return gamenew;
}

void game_free(game* g){
    free(g -> b);
    free(g);
}

bool drop_piece(game* g, unsigned int column){
    unsigned int height = g -> b -> height;
    int i = 0;
    while(i < height){
        struct pos checkpos = make_pos(i, column);
        if ((i == 0) && ((board_get((g->b), checkpos) == BLACK) | (board_get((g->b), checkpos) == WHITE))){
            if (g-> player == BLACKS_TURN){
                g -> player = WHITES_TURN;
            }else{
                g -> player = BLACKS_TURN;
            }
            i = height;
            return false;    
        } else if ((i > 0) && ((board_get((g->b), checkpos) == BLACK) | (board_get((g->b), checkpos) == WHITE))){
            struct pos setpos = make_pos(i - 1, column);
            if (g-> player == BLACKS_TURN){
                board_set((g->b), setpos, BLACK);
                i = height + 1;
                return true;
            }else{
                board_set((g->b), setpos, WHITE);
                i = height + 1;
                return true;
            }
        } else if ((i == height - 1) && (board_get((g->b), checkpos) == EMPTY)){
            if (g-> player == BLACKS_TURN){
                board_set((g->b), checkpos, BLACK);
                i = height + 1;
                return true;
            }else{
                board_set((g->b), checkpos, WHITE);
                i = height + 1;
                return true;
            }
        }
        else if (board_get((g->b), checkpos) == EMPTY){
            i++;
        }
    }   
    return false;
}
     

bool magnetize(game* g){
       printf("printing to show play.c calls magnetize, but magnetize not working\n");
       int black_rem = g -> black_rem;
       int white_rem = g -> white_rem;
       if (g-> player == WHITES_TURN){
            if ( white_rem > 0){
                return false;
            }
            else{
                g -> black_rem = g-> maglock;
            }
       }else if (g -> player == BLACKS_TURN){
            if (black_rem > 0){
                return false;
            }
            else{ 
                g -> white_rem = g -> maglock;
            }
       }
       if (white_rem == 0 && black_rem == 0){
       return false;
       }
       board* shifting = g-> b;
       board* postshift = moveanddrop(g);
       while (doneshift(g, shifting, postshift) == 0){
            doneshift(g, shifting, postshift);
       }


       /* abnormal cases and ending maglock */
       if ( white_rem > 0 && g->black_rem > 0){
            if (white_rem > black_rem){
                g-> black_rem  --;
            }
            else if (black_rem > g -> white_rem){
                g -> white_rem  --;
            }
       }
       if (black_rem > 0){
            g -> black_rem  --;
       }
        g -> player = BLACKS_TURN;
       if (white_rem > 0){
            g -> white_rem --;
       }
        g -> player = WHITES_TURN;
        return true;
}

board* moveanddrop(game* g){
    printf("move and drop\n");
    magnetize_shift(g);
    drop_magnetize(g);
    return (g-> b);
}
                
int doneshift(game*g, board* shifting, board* postshift){
    printf("dontshift\n");
    postshift = moveanddrop(g);
    unsigned int height = g->b-> height;
    unsigned int width = g -> b -> width;
    unsigned int k = 0;
    for (int x = 0; x < height; x++){
        for (int y = 0; y < width; y++){
            if (board_get(shifting, make_pos(x,y)) == board_get(postshift, make_pos(x,y))){
                k = 1;
            } else {
                k = 0; 
                y = width;
                x = height;
            }
        }
    }
    if (k == 1){
        return 1;
    }else if (k == 0){
        shifting = postshift;
        postshift = moveanddrop(g);
        return doneshift(g, shifting, postshift);
    }
    return 1;
}     


bool drop_magnetize(game* g){
    printf("drop_magnetize\n");
    unsigned int height = g -> b -> height;
    unsigned int width = g -> b -> width;
    for (int x = 0; x <= height; x++){
        for (int y = 0; y <= width; y++){
            struct pos checkpos = make_pos(x,y);
            if (board_get((g -> b), checkpos) == WHITE){
                drop_piece(g, y);
            }else if (board_get((g -> b), checkpos) == BLACK){
                drop_piece(g, y);
            }
            printf("drop magnetize");
        }
    }
    return false;
}

int magnetize_shift(game* g){
    printf("magnetize_shift\n");
    unsigned int height = g -> b -> height;
    unsigned int width = g -> b -> width;
    int y = 0;
    int x = 0;
    /* white moves right */
    if (g -> player == WHITES_TURN){
        for (y = 0; y < height; y++){
            for (x = 0; x < width; x++){
                struct pos checkpos = make_pos(x,y);
                if (board_get((g->b), checkpos) == WHITE){
                    printf("shift right");
                    shift_right(g, checkpos);
                }
            }
        }
        return 1;
    }else if (g -> player == BLACKS_TURN){
        for (y = 0; y < height; y++){
            for (x = width - 1; x >= 0; x--){
                struct pos checkpos = make_pos(x,y);
                if (board_get((g->b), checkpos) == WHITE){
                    shift_left(g, checkpos);
                }
            }
        }
        return 0;
    }
    return 0;
}
                     
/* start from the right and then when u get to a black or white piece u set it to the place to thr right of that piece*/

int shift_right(game* g, struct pos checkpos){
    printf("shift_right\n");
    unsigned int width = g -> b -> width;
    unsigned int i = checkpos.c;
    while (i > width){
        struct pos nextpos = make_pos(checkpos.r, i + 1);
        struct pos setpos = make_pos(checkpos.r, i);
        if (board_get((g -> b), nextpos) == WHITE | board_get((g -> b), nextpos) == BLACK){
            board_set((g -> b), checkpos, EMPTY);
            board_set((g -> b), setpos, BLACK);
            i ++;
            return 1;
        } else if ( (i > 0) && (board_get((g -> b), nextpos) == EMPTY)){
            i ++;
        } else if ((i == 1) && (board_get((g -> b), nextpos) == EMPTY)){
            board_set((g -> b), setpos, BLACK);
            i ++;
        }   
    }
    return 0;
}

int shift_left(game* g, struct pos checkpos){
    printf("shift_left\n");
    unsigned int i = checkpos.c;
    while (i > 0){
        struct pos nextpos = make_pos(checkpos.r, i - 1);
        struct pos setpos = make_pos(checkpos.r, i);
        if (board_get((g -> b), nextpos) == WHITE | board_get((g -> b), nextpos) == BLACK){
            board_set((g -> b), checkpos, EMPTY);
            board_set((g -> b), setpos, BLACK);
            i --;
            return 1;
        } else if ( (i > 0) && (board_get((g -> b), nextpos) == EMPTY)){
            i --;
        } else if ((i == 1) && (board_get((g -> b), nextpos) == EMPTY)){
            board_set((g -> b), setpos, BLACK);
            i --;
        }
    }
    return 0;
}
    

outcome game_outcome(game* g){
     unsigned int height = g -> b -> height;
     unsigned int width = g -> b -> width;
     unsigned int black_wins = 0;
     unsigned int white_wins = 0;

   for (int x = 0; x < height - 3; x++){
        for (int y = 0; y < width - 3; y++){
            struct pos checkpos = make_pos(x, y);
            if (check_square(g, checkpos)){
                if (board_get((g -> b), checkpos) == WHITE){
                    white_wins += 1;
                }else if (board_get((g -> b), checkpos) == BLACK){
                    black_wins += 1;
                }
            }
        }
    }
                    
        if ((black_wins > 0) && (white_wins > 0)){
            return DRAW;
        }else if (black_wins > 0){
            return BLACK_WIN;
        }else if (white_wins > 0){
            return WHITE_WIN;
        }else{
            return IN_PROGRESS;
        }
}

bool check_square(game* g, struct pos checkpos){
    unsigned int square = g-> square;
    unsigned int x = checkpos.r;
    unsigned int y = checkpos.c; 
    for (int i = 0; i < square ; i++){
        struct pos horizpos = make_pos(x + i, y);
                if (board_get((g -> b), horizpos) != board_get((g->b), checkpos)){
                    i = square;
                    return false;
                }
                for (int j = 0; j < square; j++){
                    struct pos vertpos = make_pos(x + i, y + j);
                        if (board_get((g ->b), vertpos) != board_get((g->b), checkpos)){
                            j = square;
                            return false;
                        }
                    }
                }
        return true;
}
    
