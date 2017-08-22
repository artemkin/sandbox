
#include "curses.h"
#include "term.h"
#include "unistd.h"

int main()
{
    // number of available colors
    initscr();
    start_color();
    printw("COLORS: %d\n", tgetnum("Co"));
    printw("PAIRS: %d\n", tgetnum("pa"));
    refresh(); // redraw
    sleep(3);
    endwin();

    return 0;
}
