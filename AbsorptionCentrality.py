import pandas as pd
import numpy as np
import copy

# Calculate Absorption centrality for a given network

p = 0.1

class Contact:
    def __init__(self, node1, node2, tcontact):
        self.node1 = node1
        self.node2 = node2
        self.tcontact = tcontact

# Bisection search for correct position to insert contact
def search(list, start, end, t):
    if (end <= start + 1):
        return end
    mid = (end - start)//2 + start
    if (list[mid].tcontact >= t):
        return search(list, start, mid, t)
    elif (list[mid].tcontact < t):
        return search(list, mid, end, t)
    
# Add contact to sorted list of contacts
def addContact(list, contact):
    if (len(list) == 0):
        list.append(contact)
        return
    ind = search(list, 0, len(list), contact.tcontact)
    list.insert(ind, contact)

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
        print(ii)

        # Compute off-diagonal elements
        for x in range(len(identity[0])):
            if (y == x):
                continue
            # identity[y][x] = 1 - (1 - p)**(nc[y][x])
            identity[y][x] = (1 - ii)*((1 - (1 - p)**nc[y][x])/(sum2))
    return identity


if (__name__ == '__main__'):
    t = [] # List for storing contacts
    df1 = pd.read_csv("Times.csv")
    for index, row in df1.iterrows():
        contact = Contact(row[0], row[1], row[2])
        addContact(t, contact)

    print(len(t))

    df = pd.DataFrame(columns = ["Node", "Absorption Centrality"])

    for node in range(5):
        n = 5  # Number of rows and columns in the matrix.

        identity = [[0] * i + [1] + [0] * (n - i - 1) for i in range(n)] # Create 5-by-5 identity matrix

        nc = [[0]*n for i in range(n)] # 5-by-5 matrix filled with zeroes

        nc_curr = nc.copy() # Number of contacts betweeen nodes

        prod_curr = [1/5, 1/5, 1/5, 1/5, 1/5] # Running product of matrix multiplications

        index = 0
        while (index < len(t)):
            cont = t[index]
            time = cont.tcontact
            print(time)
            nc_curr[cont.node1 - 1][cont.node2 - 1] += 1
            nc_curr[cont.node2 - 1][cont.node1 - 1] += 1
            index += 1
            if (index < len(t)):
                while (index < len(t) and t[index].tcontact == time):
                    cont = t[index]
                    nc_curr[cont.node1 - 1][cont.node2 - 1] += 1
                    nc_curr[cont.node2 - 1][cont.node1 - 1] += 1
                    index += 1

            # Multiply new transition matrix with current product
            prod_curr = np.matmul(prod_curr, compute_transition_matrix(copy.deepcopy(identity), nc_curr, node))
            nc_curr = copy.deepcopy(nc)
        
        df = pd.concat([pd.DataFrame([[node + 1, round(prod_curr[node], 3)]], columns = df.columns), df], axis = 0)
    
    df.to_csv("AbsorptionCentrality2.csv", index = False)
            
