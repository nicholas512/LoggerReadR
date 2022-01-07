# LoggerReadR
Open datalogger exports effortlessly

# About
Dataloggers for environmental science data can be configured to output text files in any number of flavours. Differences in date format, separator, and metadata can make it hard to easily open datalogger files for further processing. A script that one scientist uses to read their files can fail when trying to open files that were configured differently, even when the data are from a sensor of the same make and model. This makes it harder for scientists to share data and use a common set of tools for quality control and analysis of observational data.

LoggerRead is a library of text-file readers for permafrost-related dataloggers meant to open files effortly as dataframes. Instead of trying to make a single csv-reader that can handle any format, LoggerReadR treats each datalogger separately; this makes it possible to open files automatically with greater confidence that they're being read properly. LoggerReadR can be used as a standalone library for interactive data analysis and exploration, or can be integrated into other programs. Currently, data from the following sensors are supported:

* GeoPrecision (GP5W)
* GeoPrecision (FG2)
* Onset HOBO (HOBOware)

# Installation
You can install 
```R
library(devtools)
install_github("nicholas512/LoggerReadR", ref="main")
```

# Using LoggerReader

## Geoprecision
```R
f <- "your_geoprecision_file.csv"
read_geoprecision(f)
```

If you have any troubles you can be more specific depending which program you used to download the file (i.e. using the FG2 or GP5W client):
```
read_gp5w(f)
read_fg2(f)
```
