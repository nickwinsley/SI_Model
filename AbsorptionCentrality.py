import pandas as pd
import numpy as np
import copy
from datetime import datetime

# Calculate Absorption centrality for a given network

p = 0.3

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

def compute_transition_matrix(identity, nc, absorbing_node):
    for y in range(len(identity)):
        if (y == absorbing_node):
            continue
        sum1 = sum(nc[y])
        if (sum1 == 0):
            continue
        
        sum2 = 0
        # Compute diagonal element
        for x in range(len(identity[0])):
           identity[y][y] *= (1 - p) ** nc[y][x]
           sum2 += 1 - (1 - p) ** nc[y][x]
        ii = identity[y][y]

        # Compute off-diagonal elements
        for x in range(len(identity[0])):
            if (y == x):
                continue
            # identity[y][x] = 1 - (1 - p)**(nc[y][x])
            identity[y][x] = (1 - ii)*((1 - (1 - p) ** nc[y][x])/(sum2))
    return identity

def calcNC(contact, nc):
    for key in contact.contacts:
        for val in contact.contacts[key]:
            nc[key - 1][val - 1] += 1
    return nc


if (__name__ == '__main__'):
    t = [] # List for storing contact times
    df1 = pd.read_csv("Card Data Cleaned.csv")

    for index, row in df1.iterrows():
        time = int(datetime.strptime(row[3].split(" ")[0], '%d/%m/%Y').timestamp())
        times = row[3].split(" ")
        if (len(times) < 2):
            continue
        time = time + int(times[1].split(":")[0])
        if (len(t) == 0):
            cont = ContactTime(time)
            cont.addContact(int(row[1]), int(row[2]))
            t.append(cont)
        else:
            search(t, 0, len(t), time, int(row[1]), int(row[2]))

    df = pd.DataFrame(columns = ["Node", "Absorption Centrality"])

    for node in range(751):

        n = 751  # Number of rows and columns in the matrix.

        identity = [[0] * i + [1] + [0] * (n - i - 1) for i in range(n)] # Create 5-by-5 identity matrix

        nc = [[0]*n for i in range(n)] # 5-by-5 matrix filled with zeroes

        nc_curr = nc.copy() # Number of contacts betweeen nodes

        prod_curr = [1/751] * 751 # Running product of matrix multiplications

        index = 0

        while (index < len(t)):
            cont = t[index]

            nc_curr = calcNC(cont, copy.deepcopy(nc))
            matrix = compute_transition_matrix(copy.deepcopy(identity), nc_curr, node)

            # Multiply new transition matrix with current product
            prod_curr = np.matmul(prod_curr, matrix)
            nc_curr = copy.deepcopy(nc)

            index += 1
        
        df = pd.concat([pd.DataFrame([[node + 1, round(prod_curr[node], 3)]], columns = df.columns), df], axis = 0)
    
    df.to_csv("AbsorptionCentrality.csv", index = False)
            
