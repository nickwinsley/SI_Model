#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "sir.h"

int main() {
    FILE* fstream = fopen("Example_Network.csv", "r");
    char line[1024];
    fgets(line, 1024, fstream);
    FILE* outstream = fopen("Times.csv", "w");
    char * col1 = "ID1";
    char * col2 = "ID2";
    char * col3 = "tcontact";
    fprintf(outstream, "%s,%s,%s\n", col1, col2, col3);

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

        double tcontact = 0;

        if (year = 20) {
            tcontact = (month - 11) * 730;
        }
        else if (year = 21) {
            tcontact = 730 * 2 + (month - 1)*730;
        }

        tcontact += (day - 1) * 24;
        tcontact += hour;

        fprintf(outstream, "%u,%u,%.3lf\n", id, nid, tcontact);
    }

}