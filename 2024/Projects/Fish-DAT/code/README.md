# Set up instructions
Downloading Global Fishing Watch (GFW) fishing pressure data uses Python functions included in the `supporting_functions.py`via the `reticulate` package.  
  
Prior to running the scripts in this folder, you will need to install Python and all libraries in your computer. Additionally, you will also need to get a GFW account and an API token. Detailed instructions about how to do this are included in the sections below.  
  
## Downloading Python
There are multiple ways to install Python, but miniconda offers the option to install Python together with some useful libraries, such as `pip`, which we will use to install Python libraries. To download miniconda, click [here](https://docs.anaconda.com/free/miniconda/index.html) to go to their website and download an installer for your operating system (i.e., Windows, Linux, Mac OS).   
  
## Instaling Python libraries
Once you install Python and `pip`, you can install the Python libraries used in the `supporting_functions.py` script. You can do this using the `requirements.txt` file included in this folder via the command line or terminal as follows:  
- In the command line, navigate to the folder containing the `requirement.txt` file. For this example, we will assume this file is in the `C:/Users/username/Fish-Dat/code/` folder  
  
```bash
cd C:/Users/username/Fish-Dat/code/
```
  
- Install the Python libraries  
  
```bash
pip install -r requirements.txt
```

This may take a couple of minutes, but once it is done, you will be able to use the Python functions included in the `supporting_functions.py` script. 
  
## Get GFW account

## Get GFW token


## How to store GFW token as environmental variable
"GFW_token"
usethis::edit_r_environ()