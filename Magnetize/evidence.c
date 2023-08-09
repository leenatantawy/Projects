#include <stdlib.h>
#include <stdio.h>
#include "logic.h"
#include "board.h"
#include "pos.h"


/* evidence_make_pos: test make_pos */
void evidence_make_pos()
{
    printf("*** testing make_pos\n");
    struct pos testpos = make_pos(1,2);
    printf("expecting (1, 2): (%d, %d)\n", testpos.r, testpos.c);
}

/* evidence_board_new: test board_new */
void evidence_board_new()
{
    unsigned int width = 4;
    unsigned int height = 4;
    enum type type = MATRIX;
    enum type bitstype = BITS;

    printf("*** testing board_new\n");
    board_new(width, height, bitstype);
    board_new(0, height, type);
    board_new(width, 0, type);
    board* testboard = board_new(width, height, type);
    board_show(testboard);

}

   
/* evidence_board_show: test board_show */
void evidence_board_show()
{
    printf("*** testing board_show\n");
    unsigned int width = 4;
    unsigned int height = 4;
    enum type type = BITS;
    board* testboard = board_new(width, height, type);
    board* testboard1 = board_new(36, 36, type);
    board_show(testboard);
    board_show(testboard1);
}

/* evidence_board_get: test board_get */
void evidence_board_get()
{
    printf("*** testing board_get\n");
    struct pos p = make_pos(1,2);
    struct pos p1 = make_pos(1,3);
    board* testboard = board_new(4, 4, BITS);
    board_show(testboard);
    board_set(testboard, p, WHITE);
    board_get(testboard, p);
    board_set(testboard, p1, BLACK);
    board_get(testboard, p1);
    printf(" expecting 2: %d\n", board_get(testboard, p));
    printf(" expecting 1: %d\n", board_get(testboard, p1));
}

/* evidence_board_set: test board_set */
void evidence_board_set()
{
    printf("*** testing board_set\n");
    struct pos p = make_pos(3,1);
    board* testboard = board_new(4, 4, BITS);
    board_set(testboard, p, BLACK);
    board_show(testboard);
    game* g1 = new_game( 2, 5, 4, 4, BITS);
    printf("testing drop piece");
    drop_piece(g1, 4);
    board_show(testboard);
    printf("*** testing game_outcome");
        printf("*** testing game_outcome \n");
    game* g = new_game(3, 5, 5, 5, BITS);
    struct pos p1 = make_pos(0,0);
    struct pos p2 = make_pos(1,0);
    struct pos p3 = make_pos(2,0);
    struct pos p4 = make_pos(0,1);
    struct pos p5 = make_pos(1,1);
    struct pos p6 = make_pos(2,1);
    struct pos p7 = make_pos(3,2);
    struct pos p8 = make_pos(1,2);
    struct pos p9 = make_pos(2,2);
    board_set((g -> b), p1, WHITE);
    board_set((g -> b), p2, WHITE);
    board_set((g -> b), p3, WHITE);
    board_set((g -> b), p4, WHITE);
    board_set((g -> b), p5, WHITE);
    board_set((g -> b), p6, WHITE);
    board_set((g -> b), p7, WHITE);
    board_set((g -> b), p8, WHITE);
    board_set((g -> b), p9, WHITE);
    board_show(g->b);
    printf("expecting 0: %d\n", game_outcome(g));
    struct pos p1_ = make_pos(1,1);
    struct pos p2_ = make_pos(2,1);
    struct pos p3_ = make_pos(3,1);
    struct pos p4_ = make_pos(1,2);
    struct pos p5_ = make_pos(2,2);
    struct pos p6_ = make_pos(3,2);
    struct pos p7_ = make_pos(1,3);
    struct pos p8_ = make_pos(2,3);
    struct pos p9_ = make_pos(3,3);
    board_set((g -> b), p1_, WHITE);
    board_set((g -> b), p2_, WHITE);
    board_set((g -> b), p3_, WHITE);
    board_set((g -> b), p4_, WHITE);
    board_set((g -> b), p5_, WHITE);
    board_set((g -> b), p6_, WHITE);
    board_set((g -> b), p7_, WHITE);
    board_set((g -> b), p8_, WHITE);
    board_set((g -> b), p9_, WHITE);
    board_show(g -> b);
    game* g_1 = new_game(2, 5, 4, 4, BITS);
    struct pos p_1 = make_pos(3,1);
    struct pos p_2 = make_pos(2,1);
    struct pos p_3 = make_pos(3,2);
    struct pos p_4 = make_pos(2,2);
    board_set((g_1 -> b), p_1, WHITE);
    board_set((g_1 -> b), p_2, WHITE);
    board_set((g_1 -> b), p_3, WHITE);
    board_set((g_1 -> b), p_4, WHITE);
    board_show(g_1 -> b);
    printf("expecting 2: %d\n", game_outcome(g_1));
}

/* main: run the evidence functions above */
int main(int argc, char *argv[])
{
    evidence_make_pos();
    evidence_board_new();
    evidence_board_show();
    evidence_board_get();
    evidence_board_set();
    return 0;
}
