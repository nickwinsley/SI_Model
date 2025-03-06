#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

extern Node * nodes;
extern Node zero;
extern GLOBALS g;

// Check if node is uninitialized
bool compare(Node * a, Node * b) {
    return a -> qr == b -> qr;
}

// Add contact time and increment number of contacts
void addTime(Node * node1, Node * node2, unsigned int id1, unsigned int id2, double tcontact) {
    node1 -> t[id2][node1 -> nc[id2]] = tcontact;

    node2 -> t[id1][node2 -> nc[id1]] = tcontact;

    node1 -> nc[id2] += 1;
    node2 -> nc[id1] += 1;
}

void addContact(Node * node1, Node * node2, int id1, int id2, double tcontact) {
    addTime(node1, node2, id1, id2, tcontact);
}

void read_nodes(char * nfname) {
    FILE * fstream = fopen(nfname, "r");
    nodes = (Node *)malloc(2103 * sizeof(Node));
    for (int i = 0; i < 2103; i++) {
        memcpy(nodes + i, &zero, sizeof(Node));
    }
    char line[1024];
    fgets(line, 1024, fstream);
    while (fgets(line, 1024, fstream) != NULL) {
        unsigned int id = atoi(strtok(line, ",")); // QR Code of first node
        unsigned int nid = atoi(strtok(NULL, ",")); // QR Code of second node
        unsigned int nb = atoi(strtok(NULL, ",")); // Number of neighbours of first node
        unsigned int nnb = atoi(strtok(NULL, ",")); // Number of neighbours of second node
        unsigned int nconn = atoi(strtok(NULL, ",")); // Number of contacts between first node and second node
        
        // If first node is uninitialized
        if (compare(nodes + id, &zero)) {
            Node tmp = { .heap = 0, .qr = id, .deg = 0, .nb = (unsigned int *)malloc(nb * sizeof(unsigned int)), .nc = (unsigned int *)malloc(2103 * sizeof(unsigned int)), .t = (double **)malloc(2103 * sizeof(double *)), .t_inf = END};
            memcpy(nodes + id, &tmp, sizeof(Node));
            printf("Added node: %d\n", id);
        }

        // If second node is uninitialized
        if (compare(nodes + nid, &zero)) {
            Node tmp = { .heap = 0, .qr = nid, .deg = 0, .nb = (unsigned int *)malloc(nnb * sizeof(unsigned int)), .nc = (unsigned int *)malloc(2103 * sizeof(unsigned int)), .t = (double **)malloc(2103 * sizeof(double *)), .t_inf = END};
            memcpy(nodes + nid, &tmp, sizeof(Node));
            printf("Added node: %d\n", nid);
        }

        // Initialize pointer array of contact times
        double * time = (double *)malloc(nconn * sizeof(double));
        memcpy((nodes + id) -> t + nid, &time, nconn * sizeof(double));
        
        // Initialize pointer array of contact times
        double * time = (double *)malloc(nconn * sizeof(double));
        memcpy((nodes + nid) -> t + id, &time, nconn * sizeof(double));

        if (nconn > 0) {
            (nodes + id) -> nb[(nodes + id) -> deg++] = nid;
            (nodes + nid) -> nb[(nodes + nid) -> deg++] = id;
        }


    }
}

void read_data(char * fname, char * nfname) {
    GLOBALS g = { .heap = (unsigned int *)malloc(336423*sizeof(unsigned int)), .nheap = 0, .n_inf = 0 };
    read_nodes(nfname);
    FILE* fstream = fopen(fname, "r");
    char line[1024];
    fgets(line, 1024, fstream);

    // Read csv file and parse data
    while (fgets(line, 1024, fstream) != NULL) {
        strtok(line, ",");
        unsigned int id = atoi(strtok(NULL, ","));
        unsigned int nid = atoi(strtok(NULL, ","));
        char * date = strtok(NULL, ",");
        int rssi = atoi(strtok(NULL, ","));

        unsigned int month, day, year, hour, minute;
        char time[2];


        int res = sscanf(date, "%u/%u/%u %u:%u %s", &month, &day, &year, &hour, &minute, time);
        if (res != 6) {
            printf("Failed to read contact data. %u %u %u %u %u %s\n", month, day, year, hour, minute, time);
        }

        double tcontact = 0;

        if (year = 20) {
            tcontact = (month - 11) * 730;
        }
        else if (year = 21) {
            tcontact = 730 * 2 + (month - 1)*730;
        }

        tcontact += (day - 1) * 24;
        if (strcmp(time, "PM") == 0) {
            tcontact += 12;
        }
        tcontact += hour;
        tcontact += ((double)minute)/60;

        addContact(nodes + id, nodes + nid, id, nid, tcontact);
    }
}

int main() {
    read_data("Card Data Raw.csv");
}