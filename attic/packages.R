# If unavailable please install the below packages:
# install.packages("here")
# install.packages("tidyverse")
# install.packages("animation")
# install.packages("png")

# packages
library(here)
library(tidyverse)
library(animation)
library(png)
library(grid)

if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")
