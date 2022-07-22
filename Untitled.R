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
require(maps)

# load index data
states_df <- read.csv("data/index.csv", sep = ";") # 2017 - 2021

#change colnames for db key coherence
colnames(states_df) <- c("country", "year", "fintech_index")

# create iteration vector for wb function
unique_states <- unique(states_df$country)

# function to scrape World Bank API by indicator and time (can easily add more fields)
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
  
    # concatenate df's
    c_df <- bind_rows(banks_df)
    
    # add unique colnames
    colnames(c_df)[colnames(c_df) == "countryiso3code"] <- "country_code"
    colnames(c_df)[colnames(c_df) == "date"] <- "year"
    
    names <- colnames(c_df)
    #print(paste0("Old colnames:", names))
    
    for (i in 1:length(names)) {
      
      if (names[i] != "country_code" & names[i] != "year") {
        
        names[i] <- paste0(filename, "_", names[i])
        
        }
      
    }
    
    colnames(c_df) <- names
    #print(paste0("New colnames:", names))
    print(paste0("Writing file:   ", "data/", filename, ".csv", " ..."))
    
    # write file
    write.csv2(c_df, paste0("data/", filename, ".csv"))
  
}

# scrape 5-bank asset concentration data
wb_api("GFDD.OI.06", "2017:2021", "banks_con")

# read file for banks' market concentration
banks_concentration_raw <- read.csv("data/banks_con.csv", sep = ";")

# read sme financing data and format colnames for sql db key cohesion
sme_fin_raw <- read.csv("data/sme_financing.csv", sep = ",")
colnames(sme_fin_raw)[colnames(sme_fin_raw) == "COUNTRY"] <- "country_code"
colnames(sme_fin_raw)[colnames(sme_fin_raw) == "YEAR"] <- "year"

# get gdp growth data
wb_api("NY.GDP.MKTP.KD.ZG", "2017:2021", "gdp_growth")

# read gdp growth data
gdp_growth <- read.csv("data/gdp_growth.csv", sep = ";")

# read financial development index and format colname for sql db cohesion
fin_dev <- read.csv("data/fd_index.csv", sep = ";")
colnames(fin_dev)[colnames(fin_dev) == "code"] <- "country_code"

# get urbanization data
wb_api("SP.URB.TOTL.IN.ZS", "2017:2021", "urb_rate")

# read urbanization data
urb <- read.csv("data/urb_rate.csv", sep = ";")

# get literacy data
wb_api("SE.ADT.LITR.ZS", "2017:2021", "lit_rate")

# read literacy data
lit <- read.csv("data/lit_rate.csv", sep = ";")

# foreign direct investment
wb_api("BX.KLT.DINV.CD.WD", "2017:2021", "fdi")

# read fdi data
fdi <- read.csv("data/fdi.csv", sep = ";")

# get country ISO, country name, country region, combine data
ctry_reg_dat <- getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>%
  read.csv(text = .)
colnames(ctry_reg_dat)[colnames(ctry_reg_dat) == "alpha.3"] <- "country_code"

# make SQL db
dis_db <- dbConnect(RSQLite::SQLite(), "dis_db.sqlite")

# get list of df's from env
df_l <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame))) 

# write df's into SQL db
for (i in df_l) {
  
  dbWriteTable(dis_db,i, eval(parse(text=i)))
  
}

dbDisconnect(dis_db)

# read tables from db and combine to final df
dat_b<- dbConnect(RSQLite::SQLite(), "dis_db.sqlite")

# sql query for combined dataset
dat_raw <- dbGetQuery(dat_b, "
                      SELECT *
                      FROM states_df AS ind
                      LEFT JOIN ctry_reg_dat AS iso
                      ON ind.country = iso.name
                      LEFT JOIN banks_concentration_raw AS b_con
                      ON iso.country_code = b_con.country_code AND ind.year = b_con.year
                      LEFT JOIN sme_fin_raw AS sme_fin
                      ON iso.country_code = sme_fin.country_code AND ind.year = sme_fin.year
                      LEFT JOIN gdp_growth
                      ON iso.country_code = gdp_growth.country_code AND ind.year = gdp_growth.year
                      LEFT JOIN fdi
                      ON iso.country_code = fdi.country_code AND ind.year = fdi.year
                      LEFT JOIN urb
                      ON iso.country_code = urb.country_code AND ind.year = urb.year
                      LEFT JOIN lit
                      ON iso.country_code = lit.country_code AND ind.year = lit.year
                      LEFT JOIN fin_dev
                      ON iso.country_code = fin_dev.country_code AND ind.year = fin_dev.year
                      ")

dbDisconnect(dat_b)

# import and wrangle financing gap data to make into graph
dat_f1 <- read.csv("data/fin_gap_dat.csv", sep = ";") %>%
  select(1,2,5,6) %>%
  drop_na()
colnames(dat_f1) <- c("country", "region", "fin_gap",  "fin_gap_pcnt")
dat_f1$fin_gap_pcnt <- gsub(pattern = "[\"%]", "", x = dat_f1$fin_gap_pcnt)
dat_f1$fin_gap_pcnt <- as.numeric(dat_f1$fin_gap_pcnt)
dat_f1 <- dat_f1 %>% drop_na()
dat_f1$country[dat_f1$country == "Russian Federation"] <- "Russia"

# load map data
world_map <- map_data("world")

#create plot f1
f1 <- ggplot(dat_f1) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", size = 0.25) +
  geom_map(map = world_map, aes(map_id = country, fill = fin_gap_pcnt), size = 0.25) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "SME Financing Gap\n (% of GDP)") +
  expand_limits(x = world_map$long, y = world_map$lat) 
  # remove background grid, x and y axes as well as labels, change color scheme.
f1

sme_empl_dat <- read.csv("data/sme_employment_data.csv", sep = ";")

sme_empl_final <- sme_empl_dat %>%
  drop_na() %>%
  rowwise() %>%
  mutate(pcnt = sum(c(X1.9,X10.19,X20.49,X50.249))/Total) %>%
  select(c(X.Country, pcnt))

avg_sme_empl_share <- mean(sme_empl_final$pcnt)

f2 <- ggplot(data = sme_empl_final, mapping=aes(y=X.Country,x=pcnt,fill=X.Country)) + 
  geom_bar(stat="identity") +
  geom_vline(xintercept = mean(sme_empl_final$pcnt, na.rm=TRUE)) + # add value to line
  theme(legend.position = "none")
  # remove background grid, make axis labels, add title, change col scheme?
f2


# clean data
# make graphs

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
  
  # finish this still
  
}

####                  ####
##########################
####                  ####