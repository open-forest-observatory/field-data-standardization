# Author: Emily Marie Purvis
# Date: 10.2.2023
# Goal: Write code that 1. imports data from googlesheets to R and then 2. writes the data to the database server we will be using (PostgreSQL)

#### Load libraries ####
library(tidyverse)
library(googlesheets4)

#### Import and get rid of NAs in character columns ####

field-projects <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=1) %>% mutate_if(is.character, ~replace_na(.,""))

field-hyperplots <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=2) %>% mutate_if(is.character, ~replace_na(.,""))

field-plots <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=3) %>% mutate_if(is.character, ~replace_na(.,""))

field-trees <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=4) %>% mutate_if(is.character, ~replace_na(.,""))