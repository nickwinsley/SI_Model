import pandas as pd
import numpy as np

# Remove duplicate rows in data

if (__name__ == '__main__'):
    data = pd.read_csv("Card Data.csv")
    print(len(data.index))

    data['ExposureDateTime'] = data['ExposureDateTime'].astype(str)
    data['UploadDateTime'] = data['UploadDateTime'].astype(str)

    m=pd.concat([pd.DataFrame(np.sort(data[['ObservingQRCode','ObservedQRCode']])), data[['ExposureDateTime']]], axis = 1).duplicated()
    print(len(data[~m].index))
    new_data = data[~m]
    new_data.to_csv("Card Data Cleaned.csv", index = False)
