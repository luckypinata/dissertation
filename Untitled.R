#!/usr/bin/env Rscript

require(rvest)
require(dplyr)
require(tidyverse)
require(data.table)
require(tibble)

states <- read.csv("data/index.csv", sep = ";") # 2017 - 2021
unique_states <- unique(states$Country)

banks <- function(states = unique_states) { # function to get all banks for each state
  
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
      
      print(html)
      
      table_2 <<- html_table(html)[[1]] %>% as_tibble()
      
      table_1 <- rbind(table_1, table_2)
      
      print(class(table_1))
      
      print(nrow(table_1))
      
      j <- j + 1
      
    }
    
    table_1$country <- rep(states[i], nrow(table_1))
    
    banks_tibble[[i]] <- table_1
    
  }
  
  banks_tibble <<- do.call("rbind", banks_tibble) %>%
    as_tibble()
  
  # perform some wrangling? will the names translate into accurate maps searches?
  
}

write.csv(banks_tibble, "data/banks.csv")

# function to find number of branches for each state
# google maps function
# load("gmaps.rda")
banks <- read.csv("data/banks.csv")
key <- load("gmaps.rda")

branches <- function(banks) {
  
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
    
    # function
    
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

  
  
  
  
  
  
  
  
  
  
  


