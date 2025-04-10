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

        int nconn = root -> nc[id];

        unsigned int begin = search(tcont, root -> t[id], 0, nconn - 1);
        int length = nconn - begin;


        // If time of infection is greater than all other contacts
        if (length < 0) {
            continue;
        }

        double p = root -> beta[id];

        // Generate first time of infection
        double uniform = (double)rand() / (double)RAND_MAX;
        unsigned int first_infection = (unsigned int)ceil(log(1 - uniform)/log(1 - p));
        if (first_infection > length) {
            continue;
        }

        // for (int a = begin; a < nconn; a++) {
        //     double prob = root -> prob[id][a];
        //     double uniform = (double)rand() / (double)RAND_MAX;
        //     if (uniform < prob) {
        //         infect(root, nodes + id, root -> t[id][a]);
        //         break;
        //     }
        // }

        infect(root, nodes + id, root -> t[id][first_infection + begin - 1]);
    }
}