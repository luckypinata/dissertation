#!/usr/bin/env Rscript

require(rvest)
require(dplyr)
require(tidyverse)
require(data.table)
require(tibble)
require(jsonlite)

# load index data
states_df <- read.csv("data/index.csv", sep = ";") # 2017 - 2021
unique_states <- unique(states_df$Country)

# function to scrape 5-bank asset concentration from World Bank API
bank_asset_concentration_scraper <- function(indicator, timeframe) { 
  
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
  
  n_pages <- r[[1]][2]
  
  #print(nrow(banks_df[[1]]))
  
  for (j in 2:as.integer(n_pages)) {
    
    url <- paste0(api_endpoint, i, t, "&format=json&page=", j)
    
    print(url)
    
    r <- fromJSON(url)
    
    banks_df[[j]] <- r[[2]]
    
    #print(nrow(banks_df[[j]]))
    
  }
  
  # concatenate df's and write to file
  f <- bind_rows(banks_df) %>%
    as_tibble() %>%
    write.csv("data/banks_con.csv")
  
}

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
  
  # get region top level domains
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

  
  
  
  
  
  
  
  
  
  
  


