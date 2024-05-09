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

If tou already have an account on [globalfishingwatch.org](https://globalfishingwatch.org) you can skip this step. If you don't have an account, you can create one by clicking [here](https://gateway.api.globalfishingwatch.org/v2/auth/registration).

If you end up using GFW data be sure to check their [terms of use](https://globalfishingwatch.org/our-apis/documentation#terms-of-use), in particular section 3 (Attribution and Citation)
which details how to cite GFW data in your graphics, bibliography, or other products.

## Get GFW token

You will need a token to download GFW data with the code in this package. A token is
a series of characters that identifies your GFW account and allows you to connect to the
GFW API.

To get a token, simply go to the [token request page](https://globalfishingwatch.org/our-apis/tokens)
and generate a new token by giving it an app name (e.g. "Fish-DAT") and a description (e.g. "Token to download GFW data for the Fish-DAT project"). Your token will be created and you will be able to
copy it.

## How to store GFW token as environment variable

The best place to store your token is in the .Renviron file in your home directory. This file is used to store environmental variables that are loaded when you start an R session. To store your token in the .renviron file, you can call `usethis::edit_r_environ()` in the R console. This will open the .Renviron file where you can add the following line:

```
GFW_token = "PASTE YOUR TOKEN HERE"
```

Then save and close the file. After restarting R, you will now be able to access this token as an environment variable with:

```r
token <- Sys.getenv("GFW_token")
```

`download_fishing_effort_gfw.r` automatically loads the "GFW_token" environment variable
in this way, so you don't need to worry about it when running the script.
