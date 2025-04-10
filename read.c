#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

Node * nodes;
GLOBALS g;
Node zero = {.qr = 0};
double beta = 0.1;

// Logistic regression to get probability of transmission
double logistic(int class0, int class1, int class2) {
    double exponent = exp(-4 + class0 * 0.1 + class1 * 0.2 + class2 * 0.4);
    return exponent/(1 + exponent);
}

// Check if node is uninitialized
bool compare(Node * a, Node * b) {
    return a -> qr == b -> qr;
}

// Add contact time and increment number of contacts
void addTime(Node * node1, Node * node2, unsigned int id1, unsigned int id2, double tcontact, double prob) {
    node1 -> t[id2][node1 -> nc[id2]] = tcontact;
    node1 -> prob[id2][node1 -> nc[id2]] = prob;

    node2 -> t[id1][node2 -> nc[id1]] = tcontact;
    node2 -> prob[id1][node2 -> nc[id1]] = prob;

    node1 -> nc[id2] += 1;
    node2 -> nc[id1] += 1;

    node1 -> beta[id2] = ((node1 -> beta[id2]) * (node1 -> nc[id2] - 1) + prob)/(node1 -> nc[id2]);
    node2 -> beta[id1] = ((node2 -> beta[id1]) * (node2 -> nc[id1] - 1) + prob)/(node2 -> nc[id1]);
}

void addContact(Node * node1, Node * node2, int id1, int id2, double tcontact, double prob) {
    addTime(node1, node2, id1, id2, tcontact, prob);
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
        nodes[nid].prob[id] = (double *)malloc(nconn * sizeof(double));
        
        // Initialize pointer array of contact times
        nodes[id].t[nid] = (double *)malloc(nconn * sizeof(double));
        nodes[id].prob[nid] = (double *)malloc(nconn * sizeof(double));

        if (nconn > 0) {
            (nodes + id) -> nb[(nodes + id) -> deg++] = nid;
            (nodes + nid) -> nb[(nodes + nid) -> deg++] = id;
        }

    }

    fclose(fstream);
}

void read_nodes(char * nfname, char * cfname) {
    FILE * fstream = fopen(nfname, "r");
    nodes = (Node *)malloc(752 * sizeof(Node));
    for (int i = 0; i < 752; i++) {
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
            Node tmp = { .heap = 0, .qr = id, .deg = 0, .nb = (unsigned int *)malloc(nb * sizeof(unsigned int)), .nc = (unsigned int *)malloc(752 * sizeof(unsigned int)), .t = (double **)malloc(752 * sizeof(double *)), .t_inf = END, .prob = (double **)malloc(752 * sizeof(double *)), .beta = (double *)malloc(752 * sizeof(double))};
            memset(tmp.beta, 0, 752 * sizeof(double));
            memset(tmp.nc, 0, 752 * sizeof(unsigned int));
            memcpy(nodes + id, &tmp, sizeof(Node));
        }
    }

    fclose(fstream);
    read_conn(cfname);
}

void read_data(char * fname, char * nfname, char * cfname) {
    GLOBALS g1 = { .heap = (unsigned int *)malloc(30449*sizeof(unsigned int)), .nheap = 0, .n_inf = 0 };
    memcpy(&g, &g1, sizeof(GLOBALS));
    read_nodes(nfname, cfname);
    FILE* fstream = fopen(fname, "r");
    char line[1024];
    fgets(line, 1024, fstream);

    // Read csv file and parse data
    while (fgets(line, 1024, fstream) != NULL) {

        // Read row
        strtok(line, ",");
        unsigned int id = atoi(strtok(NULL, ","));
        unsigned int nid = atoi(strtok(NULL, ","));
        char * date = strtok(NULL, ",");
        int rssi = atoi(strtok(NULL, ","));
        strtok(NULL, ",");
        strtok(NULL, ",");
        unsigned int class0 = atoi(strtok(NULL, ","));
        unsigned int class1 = atoi(strtok(NULL, ","));
        unsigned int class2 = atoi(strtok(NULL, ","));

        class2 = class2 - class1;
        class1 = class1 - class0;

        // Calculate probability of transmission and time of contact
        double prob = logistic(class0, class1, class2);

        unsigned int month, day, year, hour, minute;

        char time[2];


        int res = sscanf(date, "%u/%u/%u %u:%u", &month, &day, &year, &hour, &minute);
        if (res != 5) {
            //printf("Failed to read contact data. %u %u %u %u %u\n", month, day, year, hour, minute);
            hour = 0;
            minute = 0;
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

        addContact(nodes + id, nodes + nid, id, nid, (double)tcontact, prob);
    }
    fclose(fstream);
}

int main(int argc, char * argv[]) {
    unsigned int run;
    unsigned int seed;

    if (argc < 3) {
        seed = 1;
        run = 1;
        printf("Incorrect number of command line arguments.");
    }
    else {
        seed = atoi(argv[1]);
        run = atoi(argv[2]);
    }

    read_data("Card Data Cleaned.csv", "Neighbours.csv", "Contacts.csv");

    // Set seed for the mersenne twister rng algorithm
    
    srand(9995 * seed);

    // Randomize starting nodes
    unsigned int starting_node = (rand() % 751) + 1;
    (nodes + starting_node) -> t_inf = 0;
    add_node(starting_node);

    unsigned int starting_node2 = (rand() % 751) + 1;
    while (starting_node2 == starting_node) {
        starting_node2 = (rand() % 751) + 1;
    }
    (nodes + starting_node2) -> t_inf = 0;
    add_node(starting_node2);

    unsigned int starting_node3 = (rand() % 751) + 1;
    while (starting_node3 == starting_node || starting_node3 == starting_node2) {
        starting_node3 = (rand() % 751) + 1;
    }
    (nodes + starting_node3) -> t_inf = 0;
    add_node(starting_node3);


    FILE * outstream;

    if (access("Fixed_Probability_Results.csv", F_OK) != 0) {
        outstream = (FILE *)fopen("Fixed_Probability_Results.csv", "w");
        for (unsigned int i = 0; i < 752; i++) {
            if (!compare(nodes + i, &zero)) {
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
        outstream = (FILE *)fopen("Fixed_Probability_Results.csv", "a");
    }

    printf("Started simulation.\n");

    // Run event-based algorithm
    while (g.nheap > 0) {
        unsigned int next = g.heap[1];
        del_root();
        transmit(nodes + next, (nodes + next) -> t_inf);
    }

    printf("Finished simulation.\n");

    for (unsigned int i = 0; i < 752; i++) {
        if (!compare(nodes + i, &zero)) {
            //char buffer[20];
            //memset(buffer, 0, 20);
            //snprintf(buffer, sizeof(buffer), "%.3lf", (nodes + i) -> t_inf);
            if ((nodes + i) -> t_inf == END) {
                fprintf(outstream, "%d,", 0);
                continue;
            }
            fprintf(outstream, "%d,", 1);
        }
    }

    fputc('\n', outstream);
    fclose(outstream);
    printf("%d\n", run);
    return 1;
}