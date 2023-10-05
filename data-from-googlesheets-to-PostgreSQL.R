# Author: Emily Marie Purvis
# Date: 10.2.2023
# Goal: Write code that 1. imports data from googlesheets to R and then 2. writes the data to the database server we will be using (PostgreSQL)

#### Load libraries ####
library(tidyverse)
library(googlesheets4)

#### Import, get rid of NAs in character columns, and pad project_id, plot_id, etc columns with leading zeros ####

# field-projects

'field-projects' <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=1) %>% mutate_if(is.character, ~replace_na(.,"")) 

`field-projects`$project_id <- formatC(as.numeric(`field-projects`$project_id), width = 4, format = "d", flag = "0")

# field-hyperplots

'field-hyperplots' <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=2) %>% mutate_if(is.character, ~replace_na(.,""))

`field-hyperplots`$hyperplot_id <- formatC(as.numeric(`field-hyperplots`$hyperplot_id), width = 4, format = "d", flag = "0")

`field-hyperplots`$project_id <- formatC(as.numeric(`field-hyperplots`$project_id), width = 4, format = "d", flag = "0")

# field-plots

'field-plots' <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=3) %>% mutate_if(is.character, ~replace_na(.,""))

`field-plots`$plot_id <- formatC(as.numeric(`field-plots`$plot_id), width = 4, format = "d", flag = "0")

`field-plots`$project_id <- formatC(as.numeric(`field-plots`$project_id), width = 4, format = "d", flag = "0")

`field-plots`$hyperplot_id <- formatC(as.numeric(`field-plots`$hyperplot_id), width = 4, format = "d", flag = "0")

# field-trees

'field-trees' <- read_sheet('https://docs.google.com/spreadsheets/d/1GjDseDCR1BX_EIkJAni7rk2zvK6nHmZz1nOFBd1d6k4/edit?usp=sharing', sheet=4) %>% mutate_if(is.character, ~replace_na(.,""))

`field-trees`$plot_id <- formatC(as.numeric(`field-trees`$plot_id), width = 4, format = "d", flag = "0")
