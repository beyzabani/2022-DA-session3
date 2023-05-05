#Data cleaning
# Global Health Observatory - WHO
# https://www.who.int/data/gho/info/gho-odata-api
#install.packages("shiny")
library(tidyverse)
library(httr)
library(rvest)

# Get the list with all the variables that are available (indicators)
#  https://www.who.int/data/gho/data/indicators
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/Indicator',
  verbose()
)

cnt <- content(response, as = "parsed")
dat <- tibble(
  code = map_chr(cnt$value, 1), 
  name = map_chr(cnt$value, 2), 
  language = map_chr(cnt$value, 3) 
)

#dat
dat$name[which(grepl("HIV",toupper(dat$name)) )]
dat$code[which(grepl("HIV",toupper(dat$name)) )][9]

#MDG_0000000029: Prevalence of HIV among adults aged 15 to 49 (%)
#WHS2_138 number: deaths bc of HIV
#HIV_0000000006: Number of people dying from HIV-related causes

#	MDG_0000000021 : Population aged 15-24 years with comprehensive correct knowledge of HIV/AIDS (%)


# Download data from one indicator: 
dat[dat$code == "MDG_0000000029",]
dat[dat$code == "WHS2_138",]
dat[dat$code == "HIV_0000000006",]
dat[dat$code == "MDG_0000000021",]


####Prevalence of HIV among adults aged 15 to 49 (%)
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/MDG_0000000029',
  verbose()
)

prevalence <- content(response, as = "parse")

prevalence <- tibble(
  id = map_chr(prevalence$value, 1), 
  country = map_chr(prevalence$value, 4), 
  year = map_dbl(prevalence$value, 6), 
  value = map_chr(prevalence$value, ~(.x)$Value)
)

prevalence$value = gsub("\\[[^]]*\\]", "", (prevalence$value))

prevalence$value = as.numeric(gsub('<','', (prevalence$value))) 

prevalence_clean <- prevalence %>% 
  drop_na(value)

saveRDS(prevalence_clean, file= "prevalence.rds")



