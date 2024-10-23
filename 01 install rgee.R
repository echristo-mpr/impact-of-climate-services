# *************************************************
# Project: 51910
# Programmer: Evan Christo
# Researcher: Dheeya Rizmie
#	Date created: 10/23/2024
# Purpose/Description: Install and initialize RGEE
# Inputs: 
# Outputs: 
#   
# Copyright information:
# Copyright (C) Mathematica Policy Research, Inc. This code cannot be 
# copied, distributed or used without the express written permission of
# Mathematica Policy Research, Inc.
# 
# **************************************************/

# I am trying to intall and initialize the RGEE package.
# Install instructions are available here: https://github.com/r-spatial/rgee
# I've also followed a few YouTube tutorials to try and install it, but with no success.
#   1. https://www.youtube.com/watch?v=olNYYynSJfI
#   2. https://www.youtube.com/watch?v=1-k6wNL2hlo&pp=ygUMaW5zdGFsbCByZ2Vl
#   3. https://www.youtube.com/watch?v=_fDhRL_LBdQ
# Any help is appreciated!

# Install packages
install.packages("sf")
install.packages("reticulate")
install.packages("remotes")

# Install RGEE
install_github("r-spatial/rgee")

# Load packages
library(reticulate)
library(rgee)
library(remotes)

# See what version of Python will be used
py_discover_config()

# Verify current Python path
import("sys")$executable

# Create a Python environment with all RGEE dependencies
ee_install()

# ERROR when I run ee_install()
# rgee::ee_install want to store the environment variables: EARTHENGINE_PYTHON 
# and EARTHENGINE_ENV in your .Renviron file to use the Python path:
#   C:\Users\echristo\AppData\Local\Continuum\Anaconda3\envs\rgee/python.exe in future sessions.
# Would you like to continues? [Y/n]:Y
# Error in ee_clean_pyenv(home) : 
#   The directory C:\Users\${USERNAME} does not exist!
#   In addition: Warning message:
#   In file.create(renv) :
#   cannot create file 'C:\Users\${USERNAME}/.Renviron', reason 'No such file or directory'


