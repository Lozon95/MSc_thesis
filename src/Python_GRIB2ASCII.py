"""
This pyhtonfile converts grib-files to .txt-files in ASCII format
The file was created by Loa Andersson during master thesis spring 2025
in order to convert meterological data from Copriconus to a format 
compatible with Delft3D-Flow"""

import pygrib
import pandas as pd
import numpy as np

#File paths
grib_file_path = "D:\\EXAMENSARBETE!!\\Thesis_data\\Coperconus_Januari_2023\\data.grib"
ascii_file_path = "D:\\EXAMENSARBETE!!\\Thesis_data\\Coperconus_Januari_2023\\data_csv\\data.txt"

# Open gribfile
grbs = pygrib.open(grib_file_path)

# Open the ASCII text file and write grib
with open(ascii_file_path, "w") as ascii_file:
    for grb in grbs:
        # Save metadata for each dataset
        data = grb.values
        name = grb.name
        date = grb.date
        time = grb.time
        units = grb.units
        short_name = grb.shortName

        # Write data and metadata to ASCII file
        ascii_file.write(f"# Variable: {name}\n")
        ascii_file.write(f"# Date: {date}\n")
        ascii_file.write(f"# Time: {time}\n")
        ascii_file.write(f"# Units: {units}\n")
        ascii_file.write(f"# Short Name: {short_name}\n")
        np.savetxt(ascii_file, data, fmt="%f")
        # New row between each dataset
        ascii_file.write("\n")  

print(f"Data saved to {ascii_file_path}")


