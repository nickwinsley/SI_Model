#include <stdio.h>
#include <stdlib.h>

#include "sir.h"

extern GLOBALS g;
extern Node * nodes;

// Bisection search for finding contacts after time of infection
unsigned int search(double time, double * times, unsigned int start, unsigned int end) {
    if (time >= times[end]) {
        return end + 1;
    }
    if (end == start + 1) {
        return end;
    }
    unsigned int mid = (end - start) / 2 + start;
    if (times[mid] >= time) {
        return search(time, times, start, mid);
    }
    return search(time, times, mid, end);
}

// Restore min-heap property
void down_heap(unsigned int start) {
    unsigned int left = (start * 2);
    unsigned int right = left + 1;
    unsigned int smallest = start;

    if (left <= g.nheap && nodes[g.heap[left]].t_inf < nodes[g.heap[smallest]].t_inf) {
        smallest = left;
    }

    if (right <= g.nheap && nodes[g.heap[right]].t_inf < nodes[g.heap[smallest]].t_inf) {
        smallest = right;
    }

    if (smallest == start) {
        return;
    }

    unsigned int temp = g.heap[smallest];
    nodes[g.heap[smallest]].heap = start;
    nodes[g.heap[start]].heap = smallest;
    g.heap[smallest] = g.heap[start];
    g.heap[start] = temp;

    down_heap(smallest);
}

// Add node to heap and increment the number of nodes infected
void add_node(unsigned int qr) {
    if (g.nheap >= sizeof(g.heap)/sizeof(unsigned int)) {
        g.heap = (unsigned int *)realloc(g.heap, sizeof(g.heap) + 10 * sizeof(unsigned int));
    }
    g.heap[++g.nheap] = qr;
    g.n_inf++;
    nodes[qr].heap = g.nheap;
    up_heap(g.nheap);
}

// Restore min-heap propoerty
void up_heap(unsigned int start) {
    if (start == 1) {
        return;
    }
    unsigned int parent = start / 2;
    if (nodes[g.heap[parent]].t_inf > nodes[g.heap[start]].t_inf) {
        unsigned int temp = g.heap[start];

        nodes[g.heap[start]].heap = parent;
        nodes[g.heap[parent]].heap = start;

        g.heap[start] = g.heap[parent];
        g.heap[parent] = temp;

        up_heap(parent);
    }
}

// Delete the root node
void del_root() {
    g.heap[1] = g.heap[g.nheap];
    g.heap[g.nheap--] = 0;

    nodes[g.heap[1]].heap = 1;
    down_heap(1);
}

