######################################################
##Syntax:   Life Goals - Job Goals MASTER
##Project:  bwp@ LeOJ LEBEL Article
##Owner: IBG
##Author:    bmc
#R Version: 3.6.1
#R Studio Version 1.2.1335 
######################################################

######################################################

################################
#SETTING UP
################################
#Clear Working Memory and Garbage
rm(list=ls())
gc()

#Load Packages
library(tidyverse)
library(readxl)
library(broom)
library(haven)
library(car)
library(miceadds)
library(ggpubr)
library(gridExtra)
library(sjmisc)
library(sjPlot)
#library(texreg)
library(rstanarm)
library(labelled)
#library(brms)
#library(ordinal)

############
#Enter path
#pfad <- "T:/__Forschung/Bildung und Gesellschaft/Projekte_Aktuell/P_Jugendstudie-2016/6_Dissemination/Artikel_bwp@"

#Names for paths
#Working Directory
pfad <- dirname(rstudioapi::getSourceEditorContext()$path)
#Tables
pfad_tab <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/Tables")
#Plots
plot_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Images")

#Names for Data files
#LeOJ
leoj.name <- "170612_JSLW_Aufbereitet.sav"
#LEBEL
lebel.name <- "Datensatz BL_190626.sav"

##############################
#Sourcing the different tasks


#Functions
##########
source(file.path(pfad, "bwp@_Analysen_FUNCTIONS.R"))


#Recoding and Selecting Variables
##############################
source(file.path(pfad, "bwp@_Analysen_RECODE_LeOJ.R"))
source(file.path(pfad, "bwp@_Analysen_RECODE_LEBEL.R"))

#Bind data of both studies together
full_data <- bind_rows(lebel.ana, leoj.ana)
rm(lebel.ana, leoj.ana) #remove single dfs


#Regressions - Values and Job Goals
##############################
source(file.path(pfad, "bwp@_Analysen_REGRESSIONS.R"))

#Tables
##############################
#source(file.path(pfad, "bwp@_Analysen_DESCRIPTIVES.R"))

#Likert-Plots
##############################
source(file.path(pfad, "bwp@_Analysen_DESCRIPTIVES_PLOTS.R"))

#Plots
##############################
source(file.path(pfad, "bwp@_Analysen_PLOTS.R"))
