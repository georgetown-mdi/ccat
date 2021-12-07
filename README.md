# 2020 Census County Assessment Tool

## Project Description
Georgetown University’s Massive Data Institute in collaboration with The Urban Institute developed the 2020 Census County Assessment Tool, a Tableau-based dashboard that provides county-level context to the 2020 decennial census data products. This tool provides an overview of an area’s estimates against the actual counts from the 2020 decennial census and provides contextual information about issues potentially affecting the census in 2020.

## Repository Contents
The GitHub repository includes raw data, the code to clean and join data, the processed data, and the Tableau workbook created with the output data. The data dictionaries for each output file can be located [here]().

## Usage Notes
The data used in this analysis was aggregated across several data sources outlined in the [data documentation](). Some data needed to be recoded to counties, but county geographies shifted slightly in 2020 from 2019. The data includes state and county identifiers (GEOIDs) from 2019 and 2020, but the tool only visualizes the 2019 geographies. This change is most stark in Alaska, where the Valdez-Cordova Census Area was actually two census areas in 2019: Copper River and Chugach. As a result, there are two extra GEOIDs in the data for Alaska, but the data is compatible with 2020 and 2019 geographies.

## License and Attribution
This work is licensed under a Creative Commons Attribution-NonCommercial 4.0 International License.

## Contact
Please contact the [Massive Data Institute](mailto:mdiresearch@georgetown.edu) with questions.

## Contributors
The 2020 Census County Assessment Tool was developed with funding from the Tableau Foundation and Alfred P. Sloan Foundation.

Contributors to this project included: Amy O’Hara, Claire Bowen, Ron Prevost, Chris Dick, Izzy Youngs, Gabriel Morrison, Sang Doan, Joseph Scariano, and Lahy Amman.