#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <float.h>

#ifndef SIR_H
#define SIR_H

#define END DBL_MAX

typedef struct {
    unsigned int qr; // QR Code
    unsigned int deg; // Degree of node
    unsigned int *nc; // entry i is the number of contacts for node with qr code i
    unsigned int heap; // Position of node in heap (zero if not in heap)
    unsigned int *nb; // entry i is the qr code of the ith neighbour
    double **t; // Array of pointer arrays. Entry i is the array of contact times for node with QR Code i.
    double t_inf; // Earliest time of infection
}Node;

typedef struct {
    unsigned int * heap; // Min-heap sorted by infection time. Values are qr codes of nodes.
    unsigned int nheap; // Size of heap
    unsigned int n_inf; // Reproductive number - number of infected individuals
} GLOBALS;

const Node zero = {.qr = 0};
Node * nodes;

extern unsigned int search(double, double *, unsigned int, unsigned int);
extern void add_node(unsigned int);

#endif /* SIR_H */
