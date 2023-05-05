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

# Plot data as a bar chart
library(ggplot2)

plot29 <- ggplot(prevalence_clean, aes(x = year, y = value, group = country, color = country)) +
  geom_line() +
  labs(title = "Prevalence of HIV among adults aged 15 to 49",
       x = "Year", y = "Prevalence (%)",
       color = "Country") +
  theme(legend.position = "bottom")

plot29





#Number of people dying from HIV-related causes
response <-  httr::GET(
  url = 'https://ghoapi.azureedge.net/api/HIV_0000000006',
  verbose()
)

death_hiv2 <- content(response, as = "parse")

death_hiv2 <- tibble(
  id = map_chr(death_hiv2$value, 1), 
  country = map_chr(death_hiv2$value, 4), 
  year = map_dbl(death_hiv2$value, 6), 
  value = map_chr(death_hiv2$value, ~(.x)$Value)
)

death_hiv2$value = as.numeric(gsub('<','', death_hiv2$value))


# Extract data and clean it
data <- content(response, as = "parsed")
data <- tibble(
  year = map_dbl(data$value, 6),
  deaths = map_chr(data$value, ~(.x)$Value)
)
data$deaths <- as.numeric(gsub('<','', data$deaths))

# Plot data as a line chart
plot06 <- ggplot(data, aes(x = year, y = deaths)) +
  geom_line() +
  labs(title = "Number of People Dying from HIV-related Causes by Year",
       x = "Year", y = "Number of Deaths")




##I am trying to analyze the trend of HIV/AIDS 
#prevalence and mortality rate across different countries and identify the 
#factors that contribute to these differences.

# death_hiv$value

death_hiv2 <- tibble(
  id = map_chr(death_hiv$value, 1), 
  country = map_chr(death_hiv$value, 4), 
  year = map_dbl(death_hiv$value, 6), 
  value = map_chr(death_hiv$value, ~(.x)$Value)
)

death_hiv2$value = as.numeric(gsub('<','', death_hiv$value))

summary(death_hiv$value)
ggplot(dat, aes(x = year, y = value)) + 
  geom_smooth(aes(group = country), size = 0.1, se = FALSE, color = "black") + 
  geom_smooth(size = 2)
