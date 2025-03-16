#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

Node * nodes;
GLOBALS g;
Node zero = {.qr = 0};
double beta = 0.1;


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


        int res = sscanf(date, "%u/%u/%u %u:%u", &month, &day, &year, &hour, &minute);
        if (res != 5) {
            printf("Failed to read contact data. %u %u %u %u %u\n", month, day, year, hour, minute);
        }

        unsigned int tcontact = 0;

        if (year = 20) {
            tcontact = (month - 11) * 730;
        }
        else if (year = 21) {
            tcontact = 730 * 2 + (month - 1)*730;
        }

        tcontact += (day - 1) * 24;
        tcontact += hour;

        addContact(nodes + id, nodes + nid, id, nid, (double)tcontact);
    }
}

int main(int argc, char * argv[]) {
    if (argc < 2) {
        printf("Incorrect number of command line arguments.");
        return 0;
    }
    read_data("Example_Network.csv", "Neighbours1.csv", "Contacts1.csv");
    srand(atoi(argv[0]));
    unsigned int run = atoi(argv[1]);
    unsigned int starting_node = (rand() % 5) + 1;
    (nodes + starting_node) -> t_inf = 0;
    add_node(starting_node);

    bool accessable = true;

    FILE * outstream;

    if (!access("Results.csv", F_OK)) {
        outstream = (FILE *)fopen("Results.csv", "w");
        for (unsigned int i = 0; i < 2103; i++) {
            if (!compare(nodes + i, &zero)) {
                char buffer[4];
                snprintf(buffer, sizeof(buffer), "%u", i);
                fprintf(outstream, "%s,", buffer);
            }
        }
        fputc('\n', outstream);
    }

    else {
        outstream = (FILE *)fopen("Results.csv", "a");
    }

    
    while (g.nheap > 0) {
        unsigned int next = g.heap[1];
        del_root();
        transmit(nodes + next, (nodes + next) -> t_inf);
    }

    for (unsigned int i = 0; i < 2103; i++) {
        if (!compare(nodes + i, &zero)) {
            char buffer[20];
            snprintf(buffer, sizeof(buffer), "%u", (unsigned int)(nodes + i) -> t_inf);
            fprintf(outstream, "%s,", buffer);
        }
    }
    fputc('\n', outstream);
    return 1;
}