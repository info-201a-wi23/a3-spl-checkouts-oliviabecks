
library(dplyr)
library(tidyverse)

#load data
spl_df <- read.csv("~/Desktop/info201/a3-spl-checkouts-oliviabecks/2022-2023-All-Checkouts-SPL-Data.csv")

spl_df <- spl_df %>% mutate(checkout_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01")) 
spl_df$checkout_date <- as.Date(spl_df$checkout_date, "%Y-%m-%d")
rooney_df <- spl_df %>% filter(str_detect(Creator, "Sally Rooney"))
rooney_df <- mutate_all(rooney_df, funs(tolower))
rooney_df$Title <- gsub(".*beautiful world, where are you.*", "Beautiful World, Where Are You", rooney_df$Title)
rooney_df$Title <- gsub(".*normal people.*", "Normal People", rooney_df$Title)
rooney_df$Title <- gsub(".*conversations with friends.*", "Conversations With Friends", rooney_df$Title)
rooney_df <- transform(rooney_df, Checkouts = as.numeric(Checkouts))
rooney_df <- transform(rooney_df, CheckoutYear = as.numeric(CheckoutYear))
rooney_df <- transform(rooney_df, CheckoutMonth = as.numeric(CheckoutMonth))

# most popular book
most_popular <- rooney_df %>% group_by(Title) %>% summarise(total_checkouts = sum(Checkouts)) 
top_book <- most_popular %>% filter(total_checkouts == max(total_checkouts)) %>% pull(Title)

top_book_checkouts <- most_popular %>% filter(total_checkouts == max(total_checkouts)) %>% pull(total_checkouts)

# month with most checkouts
checkouts_per_month <- rooney_df %>% group_by(checkout_date) %>% summarise(total_checkouts = sum(Checkouts))
highest_month <- checkouts_per_month %>% filter(total_checkouts == max(total_checkouts)) %>% pull(checkout_date)
highest_month_checkouts <- checkouts_per_month %>% filter(total_checkouts == max(total_checkouts)) %>% pull(total_checkouts)

# month with least checkouts
lowest_month <- checkouts_per_month %>% filter(total_checkouts == min(total_checkouts)) %>% pull(checkout_date)
lowest_month_checkouts <- checkouts_per_month %>% filter(total_checkouts == min(total_checkouts)) %>% pull(total_checkouts)

# most common publisher
publishers <- rooney_df %>% group_by(Publisher) %>% summarise(total_checkouts = sum(Checkouts))
best_publisher <- publishers %>% filter(total_checkouts == max(total_checkouts)) %>% pull(Publisher)

# average number of checkouts for most popular book
avg_checkouts_df <- rooney_df %>% group_by(Title) %>% summarise(avg_checkouts = mean(Checkouts))
avg_checkouts_beautiful_world <- avg_checkouts_df %>% filter(Title == "Beautiful World, Where Are You") %>% pull(avg_checkouts)

# create summary list
summary_info <- list()
summary_info$most_popular_book <- most_popular_book
summary_info$month_most_checkouts <- highest_month
summary_info$month_most_checkouts_num <- highest_month_checkouts
summary_info$month_least_checkouts <- lowest_month
summary_info$month_least_checkouts_num <- lowest_month_checkouts
summary_info$most_common_publisher <- best_publisher
summary_info$avg_checkouts_best_book <- avg_checkouts_beautiful_world

  
  
  
  
  
  
  
  
