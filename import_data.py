"""
Author: Neel Dhulipala, Andrew DeCandia
Project: Air Partners

Script for importing necessary data for air quality analysis for static reporting.
"""

from email.errors import HeaderParseError
import sys
import pandas as pd
from numpy import nan
from calendar import monthrange
import quantaq
from quantaq.utils import to_dataframe
from datetime import datetime
import data_analysis.quantaq_pipeline as qp

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
        return sensor_dict


    def _data_month(self, start_date, end_date, sensor_sn):
        """
        Gets data for a specific sensor.
        If data doesn't already exist in a pickle file, data is pulled from QuantAQ API.

        :param sensor_sn: (str) The serial number of the sensor to pull data for
        :returns: A pandas dataframe containing all of the sensor data for the month
        """
        # convert start and end times to datetime objects
        start_date = self._str_to_datetime(start_date)
        end_date = self._str_to_datetime(end_date)
        
        # instantiate handler used to download data
        mod_handler = qp.ModPMHandler(start_date=start_date, end_date=end_date)

        # Check if pckl file exists, pull data otherwise
        try:
            df = mod_handler.load_df(sensor_sn, start_date, end_date)
            print("\r Data pulled from Pickle file", flush=True)
        # Otherwise download it from API
        except:
            try:
                # Pull dataframe from API, will return the dataframe and will also pickle the results for you to open and use later
                df = mod_handler.from_api(sensor_sn)
            except:
                # If there is a request protocol error, create empty dataframe (temp solution)
                df = pd.DataFrame()
        return df


    def _str_to_datetime(self, date):
        try:
            format = '%m/%d/%Y %H:%M:%S'
            date = datetime.strptime(date, format)
        except:
            try:
                format = '%m/%d/%Y %H:%M'
                date = datetime.strptime(date, format)
            except:
                try:
                    format = '%m/%d/%Y'
                    date = datetime.strptime(date, format)
                except:
                    date = datetime.today()
        return date


    def get_PM_data(self):
        """
        Collects data from all sensors for the month.

        :returns: A list of all sensors available from QuantAQ API
        :returns: A dictionary with sensor serial numbers as keys and pandas dataframes containing sensor data as values
        """
        # Get list of sensors, where this function returns two pandas.DataFrames of the devices
        df_sensor_list, _ = self.get_sensor_list()
        # Simplify output of last function into list of sensors
        sn_list = list(df_sensor_list.sn)
        sn_count = len(sn_list)
        sn_dict = {}

        sensor_count = 1
        # For every sensor, download DataFrame with data of that sensor and insert it into dictionary
        for sn in sn_list:
            # Print out sensor downloading progress
            print('\rSensor Progress: {0} / {1}\n'.format(sensor_count, sn_count), end='', flush=True)
            # If sensor data already exists in pickle file, use that
            df = self._data_month(sn)
            # Add new dataframe to dictionary
            sn_dict[sn] = df
            sensor_count+=1
        print('\nDone!')
        return sn_list, sn_dict


if __name__ == '__main__':
    di = DataImporter()
    sn_list = di.get_sensor_list()
    sn_dict = di.create_dict(sn_list)
    print(sn_dict)