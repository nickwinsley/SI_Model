import pandas as pd
import numpy as np

# Remove duplicate rows in data

if (__name__ == '__main__'):
    data = pd.read_csv("Card Data Raw.csv")
    print(len(data.index))

    m=pd.concat([pd.DataFrame(np.sort(data[['ObservingQRCode','ObservedQRCode']])), data[['ExposureDateTime']]], axis = 1).duplicated()
    print(len(data[~m].index))
    new_data = data[~m]
    new_data.to_csv("Card Data Cleaned.csv")
