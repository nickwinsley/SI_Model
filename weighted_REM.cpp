#include <map>
#include <set>
#include <string>
#include <string.h>
#include <vector>
#include <utility>
#include <bits/stdc++.h>
#include <unistd.h>

using namespace std;


int getTime(char * datetime) {
    unsigned int year, month, day, hour, minute;

    int res = sscanf(datetime, "%u/%u/%u %2u:%2u:00", &day, &month, &year, &hour, &minute);

    if (res != 5) {
        return -1;
    }

    double tcontact = 0;

    if (year == 20) {
        tcontact = (month - 11) * 730;
    }
    else if (year == 21) {
        tcontact = 730 * 2 + (month - 1)*730;
    }

    tcontact += (day - 1) * 24;
    tcontact += hour;
    tcontact += (double)minute/(double)60;
    return floor(tcontact * 10);
}

void addContact(vector<pair<int, map<int, set<pair<int, double>>>>> &vec, int time, double prob, int id1, int id2, int start, int end) {
    if (end == start + 1) {
        map<int, set<int>> map;
        map[id1] = {{id2, log(prob)}};
        map[id2] = {{id1, log(prob)}};
        if (time >= (vec.begin() + start) -> first) {
            vec.insert(vec.begin() + end, {time, map});
            return;
        }

        vec.insert(vec.begin() + start, {time, map});
        return;
    }

    int mid = (end - start)/2 + start;

    int mid_time = vec[mid].first;

    if (time == mid_time) {
        vec[mid].second[id1].insert({id2, log(prob)});
        vec[mid].second[id2].insert({id1, log(prob)});
        return;
    }
    
    else if (time < mid_time) {
        addContact(vec, time, prob, id1, id2, start, mid);
    }
    else if (time > mid_time) {
        addContact(vec, time, prob, id1, id2, mid, end);
    }
}

int main(int argc, char * argv[]) {

    // Each pair contains a time, and a map of contacts for each node
    vector<pair<int, map<int, set<pair<int, double>>>>> contacts;

    FILE * instream = fopen("Card Data Cleaned.csv", "r");

    char line[1024];
    fgets(line, 1024, instream);
    while (fgets(line, 1024, instream)) {
        strtok(line, ",");

        unsigned int id1 = atoi(strtok(NULL, ","));
        unsigned int id2 = atoi(strtok(NULL, ","));

        char * datetime = strtok(NULL, ",");

        int time = getTime(datetime);

        if (time < 0) {
            continue;
        }

        strtok(NULL, ",");
        strtok(NULL, ",");
        strtok(NULL, ",");

        unsigned int class0 = atoi(strtok(NULL, ","));
        unsigned int class1 = atoi(strtok(NULL, ","));
        unsigned int class2 = atoi(strtok(NULL, ","));

        double prob = logistic(class0, class1, class2);

        if (contacts.size() == 0) {
            map<int, set<pair<int, double>>> map;
            map[id1] = {{id2, log(prob)}};
            map[id2] = {{id1, log(prob)}};
            contacts.push_back({time, map});
            continue;
        }

        else if (time == contacts.front().first) {
            contacts.front().second[id1].insert({id2, log(prob)});
            contacts.front().second[id2].insert({id1, log(prob)});
            continue;
        }

        addContact(contacts, time, prob, id1, id2, 0, contacts.size());
    }

    fclose(instream);

    int target = 1; // atoi(argv[1]);
    set<int> reachable = {target};

    map<int, double> shortest_paths = {};

    int end_time = (--contacts.end()) -> first;

    auto back = contacts.end(); // Pointer for iterating backwards through contacts
    back--;

    // Calculate temporal proximity prestige

    double tpp = 0; // Temporal Proximity Prestige

    double tp = 0; // Temporal Prestige

    // Calculate shortest path distances
    while (back >= contacts.begin()) {
        pair<int, map<int, set<pair<int, double>>> pair1 = *back;
        back--;

        map<int, double> temp = {};

        int time = pair1.first;
        auto first = reachable.begin();
        auto last = reachable.end();
        double tpp_sum = 0;
        while (first != last) {

            int node = *first;
            auto start = pair1.second[node].begin();
            auto end = pair1.second[node].end();
            
            while (start != end) {
                pair<int, double> next = *start;

                bool unreachable = false;

                if (reachable.find(next.first) == reachable.end()) {
                    unreachable = true;
                    reachable.insert(next.first);
                }

                double length = shortest_paths[node] + next.second;

                temp[next.first] = max(length, temp[next.first]);

                start++;
            }
            first++;
        }

        first = reachable.begin();

        while (first != last) {

            int node = *first;
            double len = max(temp[node], shortest_paths[node]);
            shortest_paths[node] = len;
            if (len > 0) {
                tp += 1/(1 - len);
            }
            tpp_sum += 1 - len;
            first++;
        }

        tpp += ((double)reachable.size()/(double)750)/(tpp_sum/(double)reachable.size());
    }


    // tpp /= ind;
    // tp /= ind*750;

    FILE * outstream;

    if (access("tpp.csv", F_OK) != 0) {
        outstream = fopen("tpp.csv", "w");

        char col1[] = "Node";
        char col2[] = "TP";
        char col3[] = "TPP";

        fprintf(outstream, "%s,%s,%s\n", col1, col2, col3);
    }

    else {
        outstream = fopen("tpp.csv", "a");
    }

    fprintf(outstream, "%d,%.2lf,%.2lf\n", target, tp, tpp);

    fclose(outstream);

    return 1;
}