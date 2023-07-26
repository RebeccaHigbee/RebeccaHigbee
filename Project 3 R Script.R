##Project 3: Open Pierce County Standards Review


# Working Directory -----------------------------------------------
setwd("M:/Project 2 OPC Asset Valuation Protocol")


# Packages ----------------------------------------------------------------
install.packages("RSocrata")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("tidyverse")


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(RSocrata)
library(dplyr)
library(tidyr)
library(lubridate)


# Data Import -------------------------------------------------------------
aiorig=read.csv("Asset_Inventory.csv", header=T)
aaorig=read.csv("Site_Analytics__Asset_Access.csv", header=T)


# Filter by public datasets only ------------------------------------------
aiorig<-aiorig %>% 
  filter(Audience == "public" & Type == "dataset")

# Clean Datasets to Include Only Necessary Columns ---------------------------
ai <- aiorig %>% 
  select(
    UID, #unique identifier of dataset
    Name, #title of dataset
    RowLab=Row.Label, #Row label
    Tag=Tags, #Values: T/F
    PublishUpdateFreq =Publishing..Update.Frequency, #Values: Daily, Weekly, Monthly, Quarterly, Annually
    License=License, #License
    DataProvided=Attribution, #Where the data came from
    SourceURL=url, #source url
    DataCustodians = Data.Procedure..Data.Custodian , #text, the data custodian's name
    ContactEmail = Contact.Email, #contant email
    Dep = Publishing..Department)

project3<-ai %>%
  mutate(RowLabel = case_when(RowLab == "" ~ "N", TRUE ~ "Y")) %>% 
  mutate(Tags = case_when(Tag == "" ~ "N", TRUE ~ "Y")) %>% 
  mutate(PubUpdateFreq = case_when(PublishUpdateFreq == "" ~ "N", TRUE ~ "Y")) %>% 
  mutate(Licensing = case_when(License == "" ~ "N", TRUE ~ "Y")) %>% 
  mutate(DataProvidedBy = case_when(DataProvided == "" ~ "N", TRUE ~ "Y")) %>%
  mutate(URL = ifelse(DataProvidedBy == "" & SourceURL == "", "N", 
                      ifelse(is.null(DataProvidedBy) == FALSE & SourceURL == "", "N", 
                             ifelse(DataProvidedBy == "" & is.null(SourceURL) == FALSE, "Y",
                                    ifelse(is.null(DataProvidedBy) == FALSE & is.null(SourceURL) == FALSE, "Y", "N" ))))) %>% 
  mutate(DataCustodian = case_when(DataCustodians == "" ~ "N", TRUE ~ "Y")) %>%
  mutate(Contact = case_when(ContactEmail == "" ~ "N", TRUE ~ "Y")) %>% 
  mutate(Department = case_when(Dep == "" ~ "N", TRUE ~ "Y"))

proj3<-project3 %>% 
  select(UID, Name, RowLabel, Tags, PubUpdateFreq, Licensing, DataProvidedBy, URL, DataCustodian, Contact, Department) %>% 
  arrange(UID)

#
today = as.Date(Sys.Date())
write.csv(proj3, paste0(today,"_Project3.csv"))
gc()