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

# install.packages("devtools")
# library(devtools)

# devtools::install_github("kthorstmann/horst")
# install.packages("Hmisc")
# library(Hmisc)
# library(psych)
# library(xlsx)
# library(data.table)

# install.packages("roxygen2")



# readme -------------------------------------------------------------

# devtools::use_readme_rmd()


# build --------------------------------------------------------------

# install.packages("roxygen2")
# library(roxygen2)
# library(devtools)
# setwd("./horst")
# document()


# ignore -------------------------------------------------------------

# devtools::use_build_ignore("00_hu_head.R")


# suggests -----------------------------------------------------------

# devtools::use_package("purrr")
# devtools::use_package("stringr")
# devtools::use_package("psych")

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
# ?plotMod
# ?renameCodebook
# ?vlookup

# # vignette -----------------------------------------------------------
#
#
# devtools::use_vignette("horst-vignette")
#
# test -----------------------------------------------------------------
#
# # devtools::use_testthat()
#
