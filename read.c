#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

Node * nodes;
GLOBALS g;
Node zero = {.qr = 0};
double beta = 0.1;
int* res;

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
void read_conn(const char * cfname) {
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

void read_nodes(const char * nfname, const char * cfname) {
    FILE * fstream = fopen(nfname, "r");
    nodes = (Node *)malloc(752 * sizeof(Node));
    if (nodes == NULL) {
        printf("Failed to allocate memory.");
        exit(1);
    }
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
            Node tmp = { .qr = id, .deg = 0, .nc = (unsigned int *)malloc(752 * sizeof(unsigned int)), .heap = 0, .nb = (unsigned int *)malloc(nb * sizeof(unsigned int)), .t = (double **)malloc(752 * sizeof(double *)), .prob = (double **)malloc(752 * sizeof(double *)), .t_inf = END, .beta = (double *)malloc(752 * sizeof(double)) };
            memset(tmp.beta, 0, 752 * sizeof(double));
            memset(tmp.nc, 0, 752 * sizeof(unsigned int));
            memcpy(nodes + id, &tmp, sizeof(Node));
        }
    }

    fclose(fstream);
    read_conn(cfname);
}

void read_data(const char * fname, const char * nfname, const char * cfname) {
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

int mainFunc(int argc, char * argv[], unsigned int start_node) {

    res = malloc(sizeof(int) * 751);
    memset(res, 0, sizeof(int) * 751);

    unsigned int starting_node = start_node;
    (nodes + starting_node) -> t_inf = 0;
    add_node(starting_node);

    printf("Started simulation.\n");

    // Run event-based algorithm
    while (g.nheap > 0) {
        unsigned int next = g.heap[1];
        del_root();
        transmit(nodes + next, (nodes + next) -> t_inf);
    }

    printf("Finished simulation.\n");

    for (unsigned int i = 0; i < 751; i++) {
        if (!compare(nodes + i + 1, &zero)) {
            //char buffer[20];
            //memset(buffer, 0, 20);
            //snprintf(buffer, sizeof(buffer), "%.3lf", (nodes + i) -> t_inf);
            if ((nodes + i + 1) -> t_inf == END) {
                continue;
            }
            
            res[i] = 1;
        }
    }

    return 1;
}

int reset_sim(void) {
    memset(g.heap, 0, sizeof(g.heap));
    for (int i = 1; i < 752; i++) {
        (nodes + i) -> t_inf = END;
        (nodes + i) -> heap = 0;
    }
    free(res);
}

int main(int argc, char * argv[]) {
    unsigned int run;
    unsigned int seed;

    if (argc < 3) {
        seed = 1;
        run = 1;
        printf("Incorrect number of command line arguments.\n");
    }
    else {
        seed = atoi(argv[1]);
        run = atoi(argv[2]);
    }

    read_data("Card Data Cleaned.csv", "Neighbours.csv", "Contacts.csv");

    // Set seed for the mersenne twister rng algorithm
    
    srand(9995);

    int** num_inf = malloc(sizeof(int*) * 751);

    for (int i = 0; i < 752; i++) {
        num_inf[i] = malloc(751 * sizeof(int));
        memset(num_inf[i], 0, sizeof(num_inf[i]));
    }

    // Run 1000 simulations for each starting node
    for (int start_node = 1; start_node < 752; start_node++) {
        for (int i = 0; i < 1000; i++) {
            mainFunc(argc, argv, start_node);
            for (int k = 0; k < 751; k++) {
                num_inf[start_node - 1][k] += res[k];
            }
            reset_sim();
        }
    }

    FILE * outstream;

    outstream = (FILE *)fopen("Results.csv", "w");
    for (unsigned int i = 0; i < 752; i++) {
        for (unsigned int j = 0; j < 752; j++) {
            fprintf(outstream, "%d,", num_inf[i][j]);
        }
        fputc('\n', outstream);
    }

    fclose(outstream);

    return 1;
}