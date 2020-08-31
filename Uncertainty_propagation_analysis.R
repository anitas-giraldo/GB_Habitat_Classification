
####        UNCERTAINTY PROPAGATION ANALYSIS      ####

# https://cran.r-project.org/web/packages/spup/vignettes/DEM_v3.html\

#  DEM is only an approximation of the real elevation in the area. It contains errors.
#  We can use the Monte Carlo (MC) method to analyse how the error propagates through spatial operations and models
#  The method thus consists of the following steps:

# 1. Characterise uncertain model inputs with (spatial) pdfs.
# 2.Repeatedly sample from (spatial) pdfs of uncertain inputs.
# 3. Run model with sampled inputs and store model outputs.
# 4. Compute summary statistics of model outputs.

# Note that the above ignores uncertainty in model parameters and model structure

# load packages
library(sp)
library(spup)
library(GGally)
library(gridExtra)
library(purrr)
library(magrittr)

# source code for Slope model
source("examples/Slope.R")

grid.arrange