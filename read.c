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

    node1 -> nc[id2] += 1;
    node2 -> nc[id1] += 1;
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

        if (nconn > 0) {
            (nodes + id) -> nb[(nodes + id) -> deg++] = nid;
            (nodes + nid) -> nb[(nodes + nid) -> deg++] = id;
        }

    }

    fclose(fstream);
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
            memset(tmp.nc, 0, 2103 * sizeof(unsigned int));
            memcpy(nodes + id, &tmp, sizeof(Node));
            printf("Added node: %d\n", id);
        }
    }

    fclose(fstream);
    read_conn(cfname);
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

        if (year == 20) {
            tcontact = (month - 11) * 730;
        }
        else if (year == 21) {
            tcontact = 730 * 2 + (month - 1)*730;
        }

        tcontact += (day - 1) * 24;
        tcontact += hour;

        addContact(nodes + id, nodes + nid, id, nid, (double)tcontact);
    }
    fclose(fstream);
}

int main(int argc, char * argv[]) {
    unsigned int run;
    unsigned int seed;
    if (argc < 2) {
        seed = 1;
        run = 1;
        printf("Incorrect number of command line arguments.");
    }
    else {
        seed = atoi(argv[0]);
        run = atoi(argv[1]);
    }
    read_data("Example_Network.csv", "Neighbours1.csv", "Contacts1.csv");
    srand(time(NULL) + 10000 * seed);
    unsigned int starting_node = (rand() % 5) + 1;
    (nodes + starting_node) -> t_inf = 0;
    add_node(starting_node);
    FILE * outstream;

    if (access("Results2.csv", F_OK) != 0) {
        outstream = (FILE *)fopen("Results2.csv", "w");
        printf("Hello\n");
        for (unsigned int i = 0; i < 2103; i++) {
            if (!compare(nodes + i, &zero)) {
                printf("Hello #2\n");
                char buffer[4];
                memset(buffer, 0, 4);
                snprintf(buffer, sizeof(buffer), "%u", i);

                if (fprintf(outstream, "%s,", buffer) < 0) {
                    printf("Error\n");
                }
            }
        }
        fputc('\n', outstream);
    }

    else {
        outstream = (FILE *)fopen("Results2.csv", "a");
    }

    
    while (g.nheap > 0) {
        unsigned int next = g.heap[1];
        del_root();
        transmit(nodes + next, (nodes + next) -> t_inf);

    }

    for (unsigned int i = 0; i < 2103; i++) {
        if (!compare(nodes + i, &zero)) {
            char buffer[20];
            memset(buffer, 0, 20);
            snprintf(buffer, sizeof(buffer), "%.3lf", (nodes + i) -> t_inf);
            fprintf(outstream, "%s,", buffer);

        }
    }

    fputc('\n', outstream);
    fclose(outstream);
    return 1;
}