import numpy as np
import pandas as pd

# Create csv file with

if (__name__ == '__main__'):
    df = pd.read_csv("Card Data Cleaned.csv")

    qrs1 = np.unique(df['ObservedQRCode'].to_numpy())
    qrs2 = np.unique(df['ObservingQRCode'].to_numpy())
    qrs = np.union1d(qrs1, qrs2)

    nc = pd.DataFrame(index = qrs, columns = qrs).fillna(0)

    print(nc.head())

    for i in range(len(df.index)):
        observed = df['ObservedQRCode'].to_numpy()[i]
        observing = df['ObservingQRCode'].to_numpy()[i]
        nc.loc[observed, observing] += 1
        nc.loc[observing, observed] += 1

    nodes = []
    nb = []
    conn = []

    for colname, col in nc.items():
        nodes.append(colname)
        nb.append((col > 0).sum())
        for i in nc.index:
            if (colname <= i):
                continue
            print(type(nc.loc[i, colname].item()))
            if (nc.loc[i, colname].item() > 0):
                conn.append([i, colname, nc.loc[i, colname].item()])

    ans = pd.DataFrame({'QR Code' : nodes, 'Neighbours' : nb})

    ans.to_csv("Neighbours.csv")

    res = pd.DataFrame(conn, columns = ['Node 1', 'Node 2', 'Num Contacts'])

    res.to_csv("Contacts.csv")


