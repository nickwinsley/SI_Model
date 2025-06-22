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

void addContact(vector<pair<int, map<int, set<int>>>> &vec, int time, double prob, int id1, int id2, int start, int end) {
    if (end == start + 1) {
        map<int, set<int>> map;
        map[id1] = {id2};
        map[id2] = {id1};
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
        vec[mid].second[id1].insert(id2);
        vec[mid].second[id2].insert(id1);
        return;
    }
    
    else if (time < mid_time) {
        addContact(vec, time, prob, id1, id2, start, mid);
    }
    else if (time > mid_time) {
        addContact(vec, time, prob, id1, id2, mid, end);
    }
}

pair<double, double> calcCentrality(int target, vector<pair<int, map<int, set<int>>>> contacts) {

    set<int> reachable = {target};
    map<int, int> times = {};
    int end_time = (--contacts.end()) -> first;
    auto back = contacts.end();
    back--;

    // Calculate shortest path distances
    while (back >= contacts.begin()) {
        pair<int, map<int, set<int>>> pair = *back;
        back--;

        int time = pair.first;
        auto first = reachable.begin();
        auto last = reachable.end();
        while (first != last) {

            int node = *first;
            auto start = pair.second[node].begin();
            auto end = pair.second[node].end();
            while (start != end) {

                int next = *start;
                if (reachable.find(next) != reachable.end()) {
                    start++;
                    continue;
                }
                reachable.insert(next);

                times[next] = end_time - time;
                start++;
            }
            first++;
        }
    }

    // Calculate temporal proximity prestige

    double tpp = 0; // Temporal Proximity Prestige

    double tp = 0; // Temporal Prestige

    int ind = 0;

    map<int, int> dist;

    int prev;

    // Iterate through sorted contact times, and calculate tpp for each starting point
    for (auto start = contacts.end() - 1; start >= contacts.begin(); start--) {

        if (start -> first == prev) {
            continue;
        }
        else {
            prev = start -> first;
        }

        ind++;
        dist[end_time - start -> first] = ind;
        // printf("%d\n", end_time - start -> first);
        int tot_dist = 0;
        int n_influence = 0;

        for (int i = 1; i < 752; i++) {

            if (i == target) {
                continue;
            }

            if (reachable.find(i) == reachable.end()) {
                continue;
            }

            if (times[i] > (end_time - start -> first)) {
                continue;
            }
            
            n_influence++;

            int distance = dist[times[i]];

            if (distance == 0) {
                printf("%d, %d\n", i, times[i]);
                continue;
            }

            tot_dist += distance;
            tp += (double)1/(double)distance;
        }

        if (tot_dist == 0 || n_influence == 0) {
            continue;
        }

        tpp += ((double)n_influence/(double)750)/((double)tot_dist/(double)n_influence);
    }
    return {tp, tpp};
}

int main(int argc, char * argv[]) {

    // Each pair contains a time, and a map of contacts for each node
    vector<pair<int, map<int, set<int>>>> contacts;

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

        if (contacts.size() == 0) {
            map<int, set<int>> map;
            map[id1] = {id2};
            map[id2] = {id1};
            contacts.push_back({time, map});
            continue;
        }

        else if (time == contacts.front().first) {
            contacts.front().second[id1].insert(id2);
            contacts.front().second[id2].insert(id1);
            continue;
        }

        double prob = 1;

        addContact(contacts, time, prob, id1, id2, 0, contacts.size());
    }

    fclose(instream);

    vector<pair<double, double>> results;

    for (int i = 1; i < 752; i++) {
        results.push_back(calcCentrality(i, contacts));
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
        outstream = fopen("tpp.csv", "w");
    }

    for (int i = 0; i < 751; i++) {
        pair<double, double> res = results[i];
        fprintf(outstream, "%d,%.2lf,%.2lf\n", i + 1, res.first, res.second);
    }

    fclose(outstream);

    return 1;
}