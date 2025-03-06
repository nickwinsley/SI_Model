#include "sir.h"

extern Node * nodes;
extern GLOBALS g;

void transmit(Node * root, double tcont) {
    unsigned int * nbh = root -> nb;
    for (; *nbh != 0; nbh++) {
        
    }
}

void infect(Node * src, Node * toInfect, double tcont) {
    double * times = toInfect -> t[src -> qr];
    unsigned int ind = search(tcont, times, 0, toInfect -> nc[src -> qr] - 1);
    if (times[ind] >= toInfect -> t_inf) {
        return;
    }
}