
require(rvest)
require(dplyr)
require(tidyverse)
require(data.table)

states <- read.csv("/Users/gummoxxx/Library/Mobile Documents/com~apple~CloudDocs/DISSERTATION/data/index.csv", sep = ";") # 2017 - 2021
unique_states <- unique(states$Country)

load("gmaps.rda")

# function to get unique states for which there is an index 
# (load .csv file, unique())

# function to get all banks for each state
# use www.swiftbic.com
# maybe some wrangling

# function to find number of branches for each state

res <- function(unique_states, banks) {
  
  
  
  
  
  
}

"""
url_1 <- "https://www.swiftbic.com/banks-in-MEXICO.html"
html_1 <- read_html(url_1)
table_1 <- html_table(html_1)[[1]] %>%
  data_frame()

url_2 <- "https://en.wikipedia.org/wiki/List_of_banks_in_Mexico"
html_2 <- read_html(url_2)
nodes_2 <- html_nodes(html_2, xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/ul[3]")
content_2 <- html_text(nodes_2) %>% str_split("\\n") %>% unlist()

for (i in 1:length(content_2)) {
  if (content_2[i] %like% table[[1]][i]) {
    print(TRUE)
  } else {
    print(FALSE)
  }
}
"""


