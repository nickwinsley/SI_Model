import numpy as np
from datetime import datetime
import pandas as pd
from numpy import linalg as LA

class ContactTime:
    def __init__(self, time):
        self.time = time
        self.contacts = {}

    def addContact(self, node1, node2):
        if (not node1 in self.contacts):
            self.contacts[node1] = set()
        if (not node2 in self.contacts):
            self.contacts[node2] = set()
        self.contacts[node1].add(node2)
        self.contacts[node2].add(node1)
    
# Bisection search for correct position to insert contact
def search(list, start, end, t, node1, node2):

    if (end <= start + 1):

        if (end < len(list) and list[end].time == t):
            list[end].addContact(node1, node2)
            return
        
        if (list[start].time == t):
            list[start].addContact(node1, node2)
            return
        
        cont = ContactTime(t)
        cont.addContact(node1, node2)
        list.insert(start + 1, cont)
        return

    mid = (end - start)//2 + start
    if (list[mid].time > t):
        search(list, start, mid, t, node1, node2)
    elif (list[mid].time < t):
        search(list, mid, end, t, node1, node2)
    else:
        # time = t
        list[mid].addContact(node1, node2)


def compute_adjacency_matrix(ctime, n):
    identity = [[0] * n for i in range(n)]

    for i in range(1, n + 1):
        for j in range(1, n + 1):
            if ((i in ctime.contacts) and (j in ctime.contacts[i])):
                identity[i - 1][j - 1] = 1
    
    return identity

def spectral_radius(adjacency_matrix):
    eigen_values = LA.eigvals(adjacency_matrix)
    return np.max(np.abs(eigen_values))

def compute_katz_matrix(ctime, n, alpha):
    identity = [[0] * i + [1] + [0] * (n - i - 1) for i in range(n)]

    for i in range(1, n + 1):
        for j in range(1, n + 1):
            if ((i in ctime.contacts) and (j in ctime.contacts[i])):
                identity[i - 1][j - 1] = alpha

    return identity

        
if (__name__ == '__main__'):
    contacts = []
    df1 = pd.read_csv("Card Data Cleaned.csv")

    for index, row in df1.iterrows():
        time = int(datetime.strptime(row[3].split(" ")[0], '%d/%m/%Y').timestamp())
        times = row[3].split(" ")
        if (len(times) < 2):
            continue
        time = time + int(times[1].split(":")[0])

        node1 = int(row[1])
        node2 = int(row[2])

        if (len(contacts) == 0):
            cont = ContactTime(time)
            cont.addContact(node1, node2)
            contacts.append(cont)

        else:
            search(contacts, 0, len(contacts), time, node1, node2)

    n = 751

    max_spectral_radius = 0

    tempDeg = [0] * n
        
    for i in range(len(contacts)):
        matrix = compute_adjacency_matrix(contacts[i], n)
        for j in range(n):
            tempDeg[j] = (tempDeg[j] * i + sum(matrix[j]))/(i + 1)
        max_spectral_radius = max(max_spectral_radius, spectral_radius(matrix))

    alpha = 1/max_spectral_radius

    prod = [[0] * i + [1] + [0] * (n - i - 1) for i in range(n)]

    for i in range(len(contacts)):
        prod = np.matmul(prod, compute_katz_matrix(contacts[i], n, alpha))

    katz_centrality = sum(prod[0])

    degree_centrality = tempDeg[0]

    df = pd.DataFrame({'Node' : [1], 'Centrality' : [katz_centrality], 'TempDegree' : [degree_centrality]})

    for i in range(1, n):
        df.loc[len(df)] = [i + 1, sum(prod[i]), tempDeg[i]]

    df.to_csv("Katz_Centrality.csv", index = False)