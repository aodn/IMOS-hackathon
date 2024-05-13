# IMOS MLD-GUI code

Natalia Riberio Santos (ribeiron) and Michael Hemming (mphemming) worked on this project during the AODN hackathon 2024. 

## Setup
### Environment

The 'AODN-Hackathon-MLD-GUI' Python environment was used for developing. The packages installed are contained in the 'requirements.txt' file. 

Mamba package installer is used (https://www.anaconda.com/blog/a-faster-conda-for-a-growing-community)

## Data

We used TEMP and PSAL CTD profile data at the location of the Port Hacking PH100 mooring site. The data are stored as pickle files in the 'Data' folder. The code can be adapted for other mooring sites. 

## Code

* 'MLD-chooser.py/ipynb' --> this is the main script that loads in CTD profile data, selects randomly n profiles, and then runs the GUI for MLD estimation
* 'MLD_Depth_GUI_output_analysis.ipynb' --> analysis of CSV containing MLD guesses created by GUI
* 'MLD_GUI_demo.ipnb' --> test code useful when developing the GUI
* 'GetCTDData.py' --> run this code to save CTD profiles as aggregated products per variable
* 'aggregated_profiles.py' -->  the main function that aggregates CTD profiles at a mooring site
* 'PickleStuff.py' --> functions to load / save workspace variables using pickle files
