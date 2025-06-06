#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <float.h>
#include <unistd.h>

#ifndef SIR_H
#define SIR_H

#define END 1000000

typedef struct {
    unsigned int qr; // QR Code
    unsigned int deg; // Degree of node
    unsigned int *nc; // entry i is the number of contacts for node with qr code i
    unsigned int heap; // Position of node in heap (zero if not in heap)
    unsigned int *nb; // entry i is the qr code of the ith neighbour
    double **t; // Array of pointer arrays. Entry i is the array of contact times for node with QR Code i.
    double **prob; // Entry i is the probabilities corresponding to contact times in t
    double t_inf; // Earliest time of infection
    double *beta;
}Node;

typedef struct {
    unsigned int * heap; // Min-heap sorted by infection time. Values are qr codes of nodes.
    unsigned int nheap; // Size of heap
    unsigned int n_inf; // Reproductive number - number of infected individuals
}GLOBALS;

extern Node zero;
extern Node * nodes;
extern GLOBALS g;
extern double beta;

extern unsigned int search(double, double *, unsigned int, unsigned int);
extern void add_node(unsigned int);
extern void up_heap(unsigned int);
extern void del_root();
extern void transmit(Node *, double);

#endif /* SIR_H */
