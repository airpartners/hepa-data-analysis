"""
Author: Neel Dhulipala
Project: Air Partners

Importing data from CSVs downloaded and processed from the below link. This
includes:
- Revere High
- Shining Star
- City Hall
- Little Folks

Please note that the data will not be imported directly from QuantAQ, although
they can be, since this data has already been imported and cleaned by Megan
Ku's work in dealing with these data.

https://github.com/scott-hersey/EB_AQ_Network/tree/HEPA

While cleaning and filtering these data, Ku opted to separate each dataset into
two unique sets: one containing baseline data and the other containing unusual
indoor plumes. This script recombines the sets. If you have access to the
merged files already, this step can be skipped.
"""

import json
import pandas as pd

# Define dtypes for each csv reading
with open('imports/revere_high_cols.json') as f:
    cols = json.load(f)
    dtypes = cols["dtypes"]
    omit_columns = cols["omit_columns"]
    renames = cols["renames"]


def recombine_filt_and_indoor(filt, indoor_src):
    """
    Recombines filtered and indoor_src files into one dataframe.

    Args:
        filt: (str) path to csv of filter data
        indoor_src: (str) path to csv of indoor source data
    Returns:
        (pandas.DataFrame) dataset of recombined data
    """

    # Read CSV files
    filt = pd.read_csv(filt, dtype=dtypes)
    indoor_src = pd.read_csv(indoor_src, dtype=dtypes)

    # Get indices of all data from indoor source that must be added back to filt
    indoor_null = indoor_src.isnull().unstack()
    has_data = indoor_null[indoor_null == False]["HEPA_on"].index
    
    # For each index, replace each row in filt with corresponding row in indoor_src
    for idx in has_data:
        filt.iloc[idx] = indoor_src.iloc[idx]

    return filt


def clean_df(df):
    """
    Removes unnecessary columns from dataframe and reformats for HEPA pipeline.

    Args:
        df: (pandas.DataFrame) dataset of combined filter and indoor_src data
    Returns:
        (pandas.DataFrame) reformatted version
    """
    ## TODO: Implement this function; involves adding dictionary to JSON
    
    # Delete unneeded columns
    df.drop(omit_columns)

    # Rename remaining columns that must be renamed
    df.rename(columns = renames, inplace = True)

    # Replace hepa_installed floats with booleans
    df["hepa_installed"] = df["hepa_installed"] == 1

    return df
    

if __name__ == "__main__":
    ## TODO: Include arguments in main function that access CSV files for any RH/SS/CH/LF file
    df = recombine_filt_and_indoor("hepa-pckls/RH_principal_filt.csv", "hepa-pckls/RH_principal_indoor_src.csv")
    print(df.head(5))
    
