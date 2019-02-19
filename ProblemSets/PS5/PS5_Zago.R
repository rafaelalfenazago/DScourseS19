#!/bin/sh.

#Question 3

library(rvest)
library(stringr)
library(dplyr)

webpage <- read_html ("https://www.bbc.com/news/world-latin-america-19652436.ht$
results <- webpage %>% html_nodes(".story-body__inner") %>% html_nodes("p")
library(dplyr)
records <- vector("list", length = length(results))
for (i in seq_along(results)) {
    date <- str_c(results[i] %>% html_nodes("strong") %>% html_text(trim = TRUE$
    fact <- str_sub(xml_contents(results[i])[2] %>% html_text(trim = TRUE))
   records[[i]] <- data_frame(date = date, fact = fact)
}
df <- bind_rows(records)
df

#Question 4 

library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson)
library(jsonlite)
library(RCurl)

# Base URL path
base_url = "https://data.colorado.gov/resource/tv8u-hswn.json?"
full_url = paste0(base_url, "county=Boulder",
             "&$where=age between 20 and 40",
             "&$select=year,age,femalepopulation")

# view full url
full_url

# encode the URL with characters for each space.
full_url <- URLencode(full_url)
full_url

library(rjson)
# Convert JSON to data frame
pop_proj_data_df  <- fromJSON(full_url)
head(pop_proj_data_df, n = 2)
typeof(pop_proj_data_df)

# view data structure
str(pop_proj_data_df)

# turn columns to numeric and remove NA values
pop_proj_data_df <- pop_proj_data_df %>%
 mutate_at(c( "age", "year", "femalepopulation"), as.numeric)

str(pop_proj_data_df)

# plot the data
ggplot(pop_proj_data_df, aes(x = year, y = femalepopulation,
 group = factor(age), color = age)) + geom_line() +
     labs(x = "Year",
          y = "Female Population - Age 20-40",
          title = "Projected Female Population",
          subtitle = "Boulder, CO: 1990 - 2040")

