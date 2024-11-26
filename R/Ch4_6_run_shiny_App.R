
# use install.packages("PACKAGE_NAME") if you don't have the following required packages installed yet

library(shiny) 
library(bslib)
library(shinyWidgets) 
library(shinyFiles)

library(tidyverse)
library(DT)
library(praise)

library(tuneR)
library(seewave)

shiny::runGitHub("Birds-Canada-ARU-2024", "SunnyTseng", subdir = "R")