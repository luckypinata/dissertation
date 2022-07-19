#!/usr/bin/env Rscript

require(rvest)
require(dplyr)
require(tidyverse)
require(data.table)
require(tibble)
require(jsonlite)
require(RCurl)
require(RSQLite)
require(DBI)

# load index data
states_df <- read.csv("data/index.csv", sep = ";") # 2017 - 2021
colnames(states_df)[colnames(states_df) == "Country"] <- "country"
colnames(states_df)[colnames(states_df) == "Year"] <- "year"
unique_states <- unique(states_df$Country)

# function to scrape 5-bank asset concentration from World Bank API
wb_api <- function(indicator, timeframe, filename) { 
  
  # formatting inputs and declaring vars
  banks_df <- list()
  api_endpoint <- "https://api.worldbank.org/v2/country/all/"
  t <- paste0("?date=", timeframe)
  i <- paste0("indicator/", indicator)
  
  # formatting url for api call
  url <- paste0(api_endpoint, i, t, "&format=json")
  print(url)
  
  # placing GET request and extracting df and max pages
  r <- fromJSON(url)
  
  banks_df[[1]] <- r[[2]]
  
  n_pages <- as.integer(r[[1]][2])
  
  #print(nrow(banks_df[[1]]))
  
  for (j in 2:n_pages) {
    
    url <- paste0(api_endpoint, i, t, "&format=json&page=", j)
    
    print(url)
    
    r <- fromJSON(url)
    
    banks_df[[j]] <- r[[2]]
    
    #print(nrow(banks_df[[j]]))
    
  }
  
  # concatenate df's and write to file
    bind_rows(banks_df) %>%
    as_tibble() %>%
    write.csv(paste0("data/", filename, ".csv"))
  
}

####                  ####
#### Work in progress ####
####                  ####

# function to get all banks for each state
banks <- function(states = unique_states) { 
  
  banks_tibble <<- list()
  
  for (i in 1:length(states)) {
    
    url <- sprintf("https://www.swiftbic.com/banks-in-%s.html", toupper(states[i]))
    
    print(url)
    
    html <- read_html(url)
    
    table_1 <- html_table(html)[[1]] %>% 
      as_tibble()
    
    j <- 2
    
    while (str_detect(as.character(html), "Next") == TRUE) {
      
      url <- as.character(paste0("https://www.swiftbic.com/banks-in-", toupper(states[i]), "-", as.character(j), ".html"))
      
      print(url)
      
      html <- read_html(url)
      
      table_2 <- html_table(html)[[1]] %>% as_tibble()
      
      table_1 <- rbind(table_1, table_2)
      
      #print(nrow(table_1))
      
      j <- j + 1
      
    }
    
    table_1$country <- rep(states[i], nrow(table_1))
    
    banks_tibble[[i]] <- table_1
    
  }
  
  banks_tibble <<- do.call("rbind", banks_tibble) %>%
    as_tibble()
  
}

write.csv(banks_tibble, "data/banks.csv")

# function to find number of branches for each state
# google maps API function
banks <- read.csv("data/banks.csv")
key <- load("gmaps.rda")

n_branches <- function(banks) {
  
  # get region top level domains (region=)
  region_url <- "https://en.wikipedia.org/wiki/Country_code_top-level_domain"
  region_domains <- read_html(region_url) %>% 
    html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>% 
    html_table() %>% 
    .[[1]] %>% 
    select(1,2) %>%
    filter(Entity %in% unique_states)
  
  region_domains$`Name[7]` <- sub("(\\.)(\\w+)", "\\2", region_domains$`Name[7]`)
  region_domains$`Name[7]` <- toupper(region_domains$`Name[7]`)
  
  # https://maps.googleapis.com/maps/api/place/textsearch/json?query=[]&region=[]
  # need query=
  # need region=
  url <- "https://maps.googleapis.com/maps/api/place/textsearch/json?query=%s"
  
  for (i in 1:nrow(region_domains)) {
    
    # function (see below)
    
  }
  
}

# function draft
for (i in 1:nrow(region_domains)) {
  
  nth_region <- region_domains[i,]
  print(paste0("nth region:", nth_region))
  nth_country <- nth_region$Entity
  print(paste0("nth_country", nth_country))
  nth_banks <- banks %>% filter(nth_country == country)
  
  print(head(nth_banks))
  
}

####                  ####
##########################
####                  ####


# read file for banks' market concentration
banks_concentration_raw <- read.csv("data/banks_con.csv")
colnames(banks_concentration_raw)[colnames(banks_concentration_raw) == "countryiso3code"] <- "country_code"
colnames(banks_concentration_raw)[colnames(banks_concentration_raw) == "date"] <- "year"

# filter desired states
# do this after reading and combining tables from db
market_concentration <- banks_concentration_raw %>%
  filter(country.value %in% unique_states)

# read sme financing data
sme_fin_raw <- read.csv("data/sme_financing.csv", sep = ",")
colnames(sme_fin_raw)[colnames(sme_fin_raw) == "COUNTRY"] <- "country_code"
colnames(sme_fin_raw)[colnames(sme_fin_raw) == "YEAR"] <- "year"

# get gdp growth data
# wb_api("NY.GDP.MKTP.KD.ZG", "2017:2021", "gdp_growth")

# read gdp growth data
gdp_growth <- read.csv("data/gdp_growth.csv")
colnames(gdp_growth)[colnames(gdp_growth) == "countryiso3code"] <- "country_code"
colnames(gdp_growth)[colnames(gdp_growth) == "date"] <- "year"

# read financial development index
fin_dev <- read.csv("data/fd_index.csv", sep = ";")
colnames(fin_dev)[colnames(fin_dev) == "code"] <- "country_code"

# get urbanization data
# wb_api("SP.URB.TOTL.IN.ZS", "2017:2021", "urb_rate")

# read urbanization data
urb <- read.csv("data/urb_rate.csv")
colnames(urb)[colnames(urb) == "countryiso3code"] <- "country_code"
colnames(urb)[colnames(urb) == "date"] <- "year"

# get literacy data
# SE.ADT.LITR.ZS
# wb_api("SE.ADT.LITR.ZS", "2017:2021", "lit_rate")

# read literacy data
lit <- read.csv("data/lit_rate.csv")
colnames(lit)[colnames(lit) == "countryiso3code"] <- "country_code"
colnames(lit)[colnames(lit) == "date"] <- "year"

# look at it over time for these 4 states
# then look at it for 2021 only for more states, with more granular detail 
# (FINDEXABLE index, for 2020, start of pandemic, where this need was more acute than ever; are the two indices comparable?)
# Findexable also has from 2020 - 2021, maybe can do pandemic comparison stuff?

# foreign direct investment
# wb_api("BX.KLT.DINV.CD.WD", "2017:2021", "fdi")

# read fdi data
fdi <- read.csv("data/fdi.csv", sep = ";")
colnames(fdi)[colnames(fdi) == "countryiso3code"] <- "country_code"
colnames(fdi)[colnames(fdi) == "date"] <- "year"

# get country ISO, country name, country region, combine data
ctry_reg_dat <- getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>%
  read.csv(text = .)
colnames(ctry_reg_dat)[colnames(ctry_reg_dat) == "alpha.3"] <- "country_code"

# make SQL db
dis_db <- dbConnect(RSQLite::SQLite(), "dis_db.sqlite")

# get list of df's from env
df_l <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame))) 
dbWriteTable(dis_db, "lit" , lit)

# write df's into SQL db
for (i in df_l) {
  
  dbWriteTable(dis_db,i, eval(parse(text=i)))
  
}

dbDisconnect(dis_db)

# read tables from db and combine to final df
dat<- dbConnect(RSQLite::SQLite(), "dis_db.sqlite")



###############################
### Not sure if going there ###
###############################

  


  

  
  
  
  
  
  
  
  
  
  


