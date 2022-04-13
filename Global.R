# load libraries
library(dplyr)
library(ggplot2)
library(boot)
library(knitr)
library(data.table)
library(Rmisc)
library(R2jags)
library(patchwork)
library(investr)
library(IsoplotR)
library(deming)
library(rstan)
library(pracma)
library(MASS)
library(quantreg)

# load data in
data_low <- read.csv("./Data/Dataset_S1_Mar8.csv")
data_intermediate <- read.csv("./Data/Dataset_S2_Mar8.csv")
data_high <- read.csv("./Data/Dataset_S3_Mar8.csv")

# load necessary functions
sapply(list.files("Functions", full.names = T), source)