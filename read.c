#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

extern Node * nodes;
extern GLOBALS g;

// Check if node is uninitialized
bool compare(Node * a, Node * b) {
    return a -> qr == b -> qr;
}

// Add contact time and increment number of contacts
void addTime(Node * node1, Node * node2, unsigned int id1, unsigned int id2, double tcontact) {
    node1 -> t[id2][node1 -> nc[id2]] = tcontact;

    node2 -> t[id1][node2 -> nc[id1]] = tcontact;
}

void addContact(Node * node1, Node * node2, int id1, int id2, double tcontact) {
    addTime(node1, node2, id1, id2, tcontact);
}

// Read number of contacts
void read_conn(char * cfname) {
    FILE * fstream = fopen(cfname, "r");

    char line[1024];
    fgets(line, 1024, fstream);

    while (fgets(line, 1024, fstream) != NULL) {

        strtok(line, ",");
        unsigned int id = atoi(strtok(NULL, ",")); // QR Code of first node
        unsigned int nid = atoi(strtok(NULL, ",")); // QR Code of second node
        unsigned int nconn = atoi(strtok(NULL, ",")); // Number of connections

        // Initialize pointer array of contact times
        nodes[nid].t[id] = (double *)malloc(nconn * sizeof(double));
        
        // Initialize pointer array of contact times
        nodes[id].t[nid] = (double *)malloc(nconn * sizeof(double));

        nodes[id].nc[nid] = nconn;

        nodes[nid].nc[id] = nconn;

        if (nconn > 0) {
            (nodes + id) -> nb[(nodes + id) -> deg++] = nid;
            (nodes + nid) -> nb[(nodes + nid) -> deg++] = id;
        }

    }
}

void read_nodes(char * nfname, char * cfname) {
    FILE * fstream = fopen(nfname, "r");
    nodes = (Node *)malloc(2103 * sizeof(Node));
    for (int i = 0; i < 2103; i++) {
        memcpy(nodes + i, &zero, sizeof(Node));
    }
    char line[1024];
    fgets(line, 1024, fstream);
    while (fgets(line, 1024, fstream) != NULL) {
        strtok(line, ",");
        unsigned int id = atoi(strtok(NULL, ",")); // QR Code
        unsigned int nb = atoi(strtok(NULL, ",")); // Number of neighbours
        
        // If first node is uninitialized
        if (compare(nodes + id, &zero)) {
            Node tmp = { .heap = 0, .qr = id, .deg = 0, .nb = (unsigned int *)malloc(nb * sizeof(unsigned int)), .nc = (unsigned int *)malloc(2103 * sizeof(unsigned int)), .t = (double **)malloc(2103 * sizeof(double *)), .t_inf = END};
            memcpy(nodes + id, &tmp, sizeof(Node));
            printf("Added node: %d\n", id);
        }

        read_conn(cfname);
    }
}

void read_data(char * fname, char * nfname, char * cfname) {
    GLOBALS g1 = { .heap = (unsigned int *)malloc(336423*sizeof(unsigned int)), .nheap = 0, .n_inf = 0 };
    memcpy(&g, &g1, sizeof(GLOBALS));
    read_nodes(nfname, cfname);
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
        if (strcasecmp(time, "PM") == 0) {
            tcontact += 12;
        }
        tcontact += hour;
        tcontact += ((double)minute)/60;

        addContact(nodes + id, nodes + nid, id, nid, tcontact);
    }
}

int main() {
    read_data("Card Data Cleaned.csv", "Neighbours.csv", "Contacts.csv");
}