# load libraries
library(dplyr)
library(ggplot2)
library(boot)
library(knitr)
library(data.table)
library(R2jags)
library(patchwork)
library(investr)
library(IsoplotR)
library(deming)
library(rstan)
library(pracma)
library(MASS)
library(quantreg)
library(gmodels)
library(greekLetters)

# load data in
data_low <- read.csv("./Data/Dataset_S1_Mar8.csv")
data_intermediate <- read.csv("./Data/Dataset_S2_Apr20_V2.csv")
data_high <- read.csv("./Data/Dataset_S3_Apr20_V2.csv")


# functions from BayClump
function_url = c("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Predictions_Bayesian.R",
                 "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Predictions_nonBayesian.R",
                 "https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Calibration_BayesianNonBayesian.R")
sapply(function_url, source)