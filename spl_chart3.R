
#load data
library("dplyr")
library("ggplot2")

spl_df <- read.csv("~/Desktop/info201/a3-spl-checkouts-oliviabecks/2022-2023-All-Checkouts-SPL-Data.csv")

spl_df <- spl_df %>% mutate(checkout_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01")) 
spl_df$checkout_date <- as.Date(spl_df$checkout_date, "%Y-%m-%d")

rooney_df <- spl_df %>% filter(str_detect(Creator, "Sally Rooney"))
rooney_df <- spl_df %>% filter(str_detect(Creator, "Sally Rooney"))
rooney_df <- mutate_all(rooney_df, funs(tolower))
rooney_df$Publisher <- gsub(".*books on tape.*", "Books on Tape", rooney_df$Publisher)
rooney_df$Publisher <- gsub(".*lindhardt og ringhof.*", "Lindhardt Og Ringhof", rooney_df$Publisher)
rooney_df$Publisher <- gsub(".*macmillan publishers.*", "Macmillan Publishers", rooney_df$Publisher)
rooney_df$Publisher <- gsub(".*macmillan audio.*", "Macmillan Audio", rooney_df$Publisher)
rooney_df$Publisher <- gsub(".*penguin random house.*", "Penguin Random House", rooney_df$Publisher)
rooney_df$Publisher <- gsub(".*random house, inc.*", "Random House, inc.", rooney_df$Publisher)
rooney_df <- transform(rooney_df, Checkouts = as.numeric(Checkouts))
# rooney_df <- transform(rooney_df, CheckoutYear = as.numeric(CheckoutYear))
# rooney_df <- transform(rooney_df, CheckoutMonth = as.numeric(CheckoutMonth))
rooney_df$checkout_date <- as.Date(rooney_df$checkout_date, "%Y-%m-%d")

checkouts_per_publisher <- rooney_df %>% group_by(Publisher) %>% summarise(total_checkouts = sum(Checkouts))
checkouts_per_publisher <- transform(checkouts_per_publisher, total_checkouts = as.numeric(total_checkouts))

# graph
ggplot(checkouts_per_publisher, aes(x = Publisher, y = total_checkouts, fill = Publisher)) + 
  geom_bar(stat = "identity") +
  labs(title = "Sally Rooney 2022 SPL Checkouts by Publisher",
       x = "Publisher",
       y = "Number of Checkouts") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) 
