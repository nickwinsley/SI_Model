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
    if (toInfect -> heap == 0) {
        add_node(toInfect -> qr);
        return;
    }
    up_heap(toInfect -> heap);
}

void transmit(Node * root, double tcont) {
    for (int i = 0; i < root -> deg; i++) {

        unsigned int id = *((root -> nb) + i);

        unsigned int begin = search(tcont, root -> t[id], 0, root -> nc[id] - 1);
        int length = (root -> nc[id]) - begin;

        unsigned int nconn = root -> nc[id];

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

        infect(root, nodes + id, root -> t[id][first_infection + begin - 1]);
    }
}