#include "sir.h"

extern Node * nodes;
extern GLOBALS g;
extern double beta;
extern Node zero;

void infect(Node * src, Node * toInfect, double tcont) {
    if (toInfect -> t_inf <= tcont) {
        return;
    }
    toInfect -> t_inf = tcont;
    up_heap(toInfect -> heap);
}

void transmit(Node * root, double tcont) {
    unsigned int * nbh = root -> nb;
    for (int i = 0; *(nbh + i) != 0; i++) {

        unsigned int begin = search(tcont, root -> t[*(nbh + i)], 0, root -> nc[*(nbh + i)]);
        unsigned int length = root -> nc[*(nbh + i)] - begin;

        // If time of infection is greater than all other contacts
        if (length < 0) {
            continue;
        }

        // Generate first time of infection
        double uniform = (double)rand() / (double)RAND_MAX;
        unsigned int first_infection = (unsigned int)ceil(log(1 - uniform)/log(1 - beta));
        if (first_infection > length) {
            continue;
        }

        infect(root, nodes + *(nbh + i), root -> t[*(nbh + i)][first_infection + begin]);
    }
}