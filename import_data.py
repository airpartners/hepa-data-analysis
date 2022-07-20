"""
Author: Neel Dhulipala, Andrew DeCandia
Project: Air Partners

Script for importing necessary data for HEPA data research
"""

import json
import pandas as pd
from numpy import nan
import quantaq
from datetime import datetime
import data_analysis.quantaq_pipeline as qp
import re

with open('token.txt', 'r') as f:
    token = f.read()

client = quantaq.QuantAQAPIClient(token)

class DataImporter(object):
    """
    Imports necessary sensor and wind data for analysis.
    """

    def __init__(self):
        self.deployments = pd.read_csv("hepa-pckls/MOF-Curated-Deployments.csv")


    def get_sensor_list(self):
        """
        Gets the list of sensors currently within HEPA deployment dataset.
        """

        # Read the deployments CSV, which includes information about the 
        # names of sensors and when they were deployed
        # deployments = pd.read_csv("hepa-pckls/MOF-Curated-Deployments.csv")
        devices = list(set(self.deployments['Sensors'].to_list()))
        # Remove instances of "Unknown" sensor or NaNs
        devices.remove('Unknown'); devices.remove(nan)
        return devices


    def create_dict(self, device_list):
        """
        Creates dictionary of each sensor including data.
        """
        # Creates empty dictionary
        sensor_dict = {}
        # Create a dictionary for each sensor in the sensor dictionary
        for sn in device_list:
            sensor_dict[sn] = {}
        # Go through CSV dataframe; anytime there is an instance of 
        for index in range(len(self.deployments)):
            sn = self.deployments['Sensors'].iloc[index]
            # If sensor of this CSV row is in the device list:
            if sn in device_list:
                # Define key of inner dictionary with start and end dates
                start = self.deployments['Approximate time and day'].iloc[index]
                end = self.deployments['End of data acquisition'].iloc[index]
                date_tuple = (start, end)
                # Get data for sensor for this time period
                print(f'Downloading data for {sn}')
                df = self._data_month(start, end, sn)
                # Define items of this key as the sensor location (outdoors or indoors) and data
                sensor_dict[sn][date_tuple] = self.deployments['Location'].iloc[index], df
        
        # Create json of dictionary and save it as json file
        sensor_json = json.dumps(sensor_dict, indent=4)
        with open("sensors.json", "w") as outfile:
            outfile.write(sensor_json)

        return sensor_dict


    def get_data_in_timeframe(self, start_date, end_date, sensor_sn):
        """
        Gets data for a specific sensor.
        If data doesn't already exist in a pickle file, data is pulled from QuantAQ API.

        :param sensor_sn: (str) The serial number of the sensor to pull data for
        :returns: A pandas dataframe containing all of the sensor data for the month
        """
        try:
            # convert start and end times to datetime objects
            start_date = self._str_to_datetime(start_date)
            end_date = self._str_to_datetime(end_date)
        except:
            print(f"Deployment dates missing for {sensor_sn}.")
            return pd.DataFrame()
        
        # instantiate handler used to download data
        mod_handler = qp.ModPMHandler(start_date=start_date, end_date=end_date)

        # Check if pckl file exists, pull data otherwise
        try:
            df = mod_handler.load_df(sensor_sn, start_date, end_date)
            print("\r Data pulled from Pickle file", flush=True)
        # Otherwise download it from API
        except:
            # Pull dataframe from API, will return the dataframe and will also pickle the results for you to open and use later
            df = mod_handler.from_api(sensor_sn)
        return df


    def _str_to_datetime(self, date):
        return pd.to_datetime(date).date()


#    def get_PM_data(self):
#        """
#        Collects data from all sensors for the month.
#
#       :returns: A list of all sensors available from QuantAQ API
#        :returns: A dictionary with sensor serial numbers as keys and pandas dataframes containing sensor data as values
#        """
#        # Get list of sensors, where this function returns two pandas.DataFrames of the devices
#        df_sensor_list, _ = self.get_sensor_list()
#        # Simplify output of last function into list of sensors
#        sn_list = list(df_sensor_list.sn)
#        sn_count = len(sn_list)
#        sn_dict = {}
#
#        sensor_count = 1
#        # For every sensor, download DataFrame with data of that sensor and insert it into dictionary
#        for sn in sn_list:
#            # Print out sensor downloading progress
#            print('\rSensor Progress: {0} / {1}\n'.format(sensor_count, sn_count), end='', flush=True)
#            # If sensor data already exists in pickle file, use that
#            df = self.get_data_in_timeframe(sn)
#            # Add new dataframe to dictionary
#            sn_dict[sn] = df
#            sensor_count+=1
#        print('\nDone!')
#        return sn_list, sn_dict

if __name__ == '__main__':
  
  # Convert csv to usable dataframes
  deploy = pd.read_csv("hepa-pckls/MOF-Curated-Deployments.csv")
  
  locations = {}
  d = {"sensor_sn" : [], "location" : [], "sensor_install" : [],
       "hepa_install" : [], "sensor_removal" : []}
  current_location = ""
  
  for row in deploy.itertuples():
    
    # Check for dates by looking for strings that start with a number.
    if re.search(r'^(\d+)', str(row.Address)):
      current_location = row.Address
      d = {"sensor_sn" : [], "location" : [], "sensor_install" : [],
           "hepa_install" : [], "sensor_removal" : []}
      
    # Look to see if this is the last sensor at this location
    if str(row.Sensors) == 'nan':
      df = pd.DataFrame(data=d)
      
      # Filter out sensors lacking deployment info
      df = df.dropna(axis=0, how='all')
      df = df[df.sensor_sn != "Unknown"]
      df = df[df.sensor_install != "Unknown"]
      df = df[df.hepa_install != "Unknown"]
      df = df[df.sensor_removal != "Unknown"]
      locations[current_location] = df
    
    # Add relevant data to dictionary
    d["sensor_sn"].append(row.Sensors)
    d["location"].append(row.Location)
    d["sensor_install"].append(row._5)
    d["hepa_install"].append(row._6)
    d["sensor_removal"].append(row._8)
  
  # Remove locations with no sensor deployment from end of file
  locations = {key:val for key, val in locations.items() if not val.empty}
  
  di = DataImporter()
  final = {}
  
  for address in locations:
    final[address] = []
    df = locations[address]
    
    # Pull outdoor sensor data
    df_out = df[df.location == "Outdoors"]
    # Should always be one row
    for row in df_out.itertuples():
      df_out_data = di.get_data_in_timeframe(row.sensor_install, row.sensor_removal, row.sensor_sn)
      df_out_data = df_out_data.add_prefix("outdoor_")
    
    # Look at indoor sensor data
    df_in = df[df.location == "Indoors"]
    
    for row in df_in.itertuples():
      filter_date = row.hepa_install
      df_in_data = di.get_data_in_timeframe(row.sensor_install, row.sensor_removal, row.sensor_sn)
      df_in_data = df_in_data.add_prefix("indoor_")
      # Combine outdoor and indoor data into pairs
      result = pd.concat([df_out_data, df_in_data], ignore_index=True, axis=1)
      
    
      
        
        
        
    
  
  
  
  
