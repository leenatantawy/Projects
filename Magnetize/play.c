#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "logic.h"

int main(int argc, char* argv[]){

    unsigned int height;
    unsigned int width;
    unsigned int maglock;
    unsigned int square;
    

    for (int i = 1; i < argc; i+=2){
        if (strcmp(argv[i], "-h") == 0){
            height = atoi(argv[i+1]);
        }else if (strcmp(argv[i], "-w") == 0){
            width = atof(argv[i+1]);
        }else if (strcmp(argv[i], "-s") == 0){
            square = atof(argv[i+1]);
        }else if (strcmp(argv[i], "-l") == 0){
            maglock = atof(argv[i+1]);
        }else{
            fprintf(stderr, "not supported character to start game");
        }
    }
        
    enum type type = BITS;

    game* g = new_game(square, maglock, width, height, type);
    
    int x = 0;
    int j = 0;
    while(x == 0){
        board_show(g -> b);

            while(j == 0){
                char ch;
                if (g -> player == WHITES_TURN){
                    printf("WHITE: ");
                } else if (g -> player == BLACKS_TURN){
                    printf("BLACK: ");
                }

                printf("Please enter a character: ");
                scanf("%c%*c", &ch);
            
                if (ch == '!'){
                    magnetize(g);
                    j = 1;
                } else {
                    if (ch >= 48 && ch <= 57){
                        int chi = ch - 48;
                        if ( chi >= width){
                            printf("out of bounds\n");
                            j = 0;
                        }else{
                            if (drop_piece(g, chi) == false){
                                printf("column is full");
                                j = 0;
                            }else{
                                j = 1;
                            }
                        }
                    }else if (ch >= 65 && ch <= 90){    
                        int chi = ch - 55;
                        if (chi >= width){
                            printf("out of bounds\n");
                            j = 0;
                        }else{
                            if (drop_piece(g, chi) == false){
                                printf("column full\n");
                                j = 0;
                            }else{
                                j = 1;
                            }
                            drop_piece(g, chi);
                            j = 1;
                        }
                    }else if (ch >= 97 && ch <= 122){
                        int chi = ch - 61;
                        if (chi >= width){
                            printf("out of bounds\n");
                            j = 0;
                        }else{
                            if (drop_piece(g, chi) == false){
                                printf("column full\n");
                                j = 0;
                            }else{
                                j = 1;
                            }
                        }
                    }else if (ch > 122){
                        printf("column number too high\n");
                        j = 0;
                    }            
                }
            }
            if (game_outcome(g) == 0){
                j = 0;
            }else {
                printf("checkoutcome");
                x = 1;
                printf("%d", x);
            }
        }
        board_show(g -> b);
        int outcome = game_outcome(g);
        if (outcome == 1){
            printf("BLACK WINS");
        } else if  (outcome == 2){
            printf("WHITE WINS");
        } else {
            printf("DRAW");
        }
        exit(1);
 }


            
    

