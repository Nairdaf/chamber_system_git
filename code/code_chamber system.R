# This code is to replicate analysis and figures for the paper "Comprehensive industry-relevant black soldier fly bioconversion characterisation by a novel chamber system", Fuhrmann et. al 2024
# Code developed by Adrian Fuhrmann and Moritz Gold

# Load libraries


library(tidyverse)
library(readxl)
library(hms)
library(humidity)
library(aiRthermo)
library(ggplot2)
library(psychrolib)
library(readr)

###load and prepare all csv data from data folder----

#CO2 data
CO2_data <- read_csv("data/cs_pub_runs_CO2.csv")

#scale data
scale_data <- read_csv("data/cs_pub_hive_all.csv")

#data from manual sampling
manual_data <- read_csv("data/cs_pub_runs_manual.csv")

#sensor data
runs_data <- read_csv("data/cs_pub_runs.csv")

#sensor data gathered
runs_g_data <- read_csv("data/cs_pub_runs_gather_clean.csv")


#-exclude---


#---

# process data----
