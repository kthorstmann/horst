# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

devtools::install_github("kthorstmann/horst")
library(horst)


# readme -------------------------------------------------------------




# build --------------------------------------------------------------


# library(roxygen2)
# library(devtools)
# setwd("./horst")
# document()


# ignore -------------------------------------------------------------

# devtools::use_build_ignore("00_hu_head.R")

# install ------------------------------------------------------------
#
# setwd("..")
# install("horst")
# library(horst)

# use functions ------------------------------------------------------

# ?omegaW
# ?simpsonCor
# ?getTable
# ?corHist
# ?matchItems


# # vignette -----------------------------------------------------------
#
# browseVignettes()
# devtools::use_vignette("horst-vignette")
# # test ---------------------------------------------------------------
#
# # devtools::use_testthat()
#
