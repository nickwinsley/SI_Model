#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <float.h>

#ifndef SIR_H
#define SIR_H

#define END DBL_MAX

typedef struct {
    unsigned int qr;
    unsigned int deg, *nc, heap;
    unsigned int *nb;
    double **t;
    double t_inf;
}Node;

typedef struct {
    unsigned int * heap; // Min-heap sorted by infection time. Values are qr codes of nodes.
    unsigned int nheap; // Size of heap
    unsigned int n_inf; // Reproductive number - number of infected individuals
} GLOBALS

const Node zero = {.qr = 0};
Node * nodes;
GLOBALS g = { .heap = (unsigned int *)malloc(2103*sizeof(unsigned int)), .nheap = 0, .n_inf = 0 };

extern unsigned int search(double, double *, unsigned int, unsigned int);
extern void add_node(unsigned int);

#endif /* SIR_H */
