#-------------------------------------------------------------------------
# Date: 2018-05
# Title: fire - gangwondo
# Contens : 1)
# Issue   : 1) 
#-------------------------------------------------------------------------
rm(list=ls())
gc()

#0. LOAD LIBRARY ============================================================

library(ggplot2)
library(dplyr)
require(gridExtra)
require(reshape2)
library(raster)
library(googleway)
library(pracma)  
library(knitr)

#1. READ DATA================================================================
setwd("C:\\Users\\Jihyeon\\Desktop\\공모전 - 화재")
tot_df <- read.csv("data\\tot_df.csv", stringsAsFactors = FALSE)

sapply(tot_df, function(x) round(sum(is.na(x))/length(x),4))
tot_df$month <- substr(tot_df$date, 6, 7)

table(tot_df$month[tot_df$y == 1])








