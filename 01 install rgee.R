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