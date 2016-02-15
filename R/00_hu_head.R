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

# library(devtools)
# devtools::install_github("kthorstmann/horst")
# library(Hmisc)
# library(psych)
# library(xlsx)
# library(data.table)

# readme -------------------------------------------------------------

# devtools::use_readme_rmd()
#
# x <- seq(1:5)
#
# for (i in seq_along(data)){
#   print(i)}

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
# ?plotMod



# # vignette -----------------------------------------------------------
#
# browseVignettes()
# devtools::use_vignette("horst-vignette")
# test -----------------------------------------------------------------
#
# # devtools::use_testthat()
#
