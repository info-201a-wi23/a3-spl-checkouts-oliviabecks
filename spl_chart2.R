
# load libraries
library("dplyr")
library("ggplot2")

# load data
spl_df <- read.csv("~/Desktop/info201/a3-spl-checkouts-oliviabecks/2022-2023-All-Checkouts-SPL-Data.csv")
spl_df <- spl_df %>% mutate(checkout_date = paste0(CheckoutYear, "-", CheckoutMonth, "-01")) 
spl_df$checkout_date <- as.Date(spl_df$checkout_date, "%Y-%m-%d")

# create rooney df
rooney_df <- spl_df %>% filter(str_detect(Creator, "Sally Rooney"))
rooney_df <- spl_df %>% filter(str_detect(Creator, "Sally Rooney"))
rooney_df <- mutate_all(rooney_df, funs(tolower))
rooney_df$Title <- gsub(".*beautiful world, where are you.*", "Beautiful World, Where Are You", rooney_df$Title)
rooney_df$Title <- gsub(".*normal people.*", "Normal People", rooney_df$Title)
rooney_df$Title <- gsub(".*conversations with friends.*", "Conversations With Friends", rooney_df$Title)
rooney_df$Title <- gsub(".*gente normal.*", "Normal People", rooney_df$Title)
rooney_df <- transform(rooney_df, Checkouts = as.numeric(Checkouts))
# rooney_df <- transform(rooney_df, CheckoutYear = as.numeric(CheckoutYear))
# rooney_df <- transform(rooney_df, CheckoutMonth = as.numeric(CheckoutMonth))
rooney_df$checkout_date <- as.Date(rooney_df$checkout_date, "%Y-%m-%d")

# group checkouts by date and material
checkouts_per_month_material <- rooney_df %>% group_by(checkout_date, MaterialType) %>% summarise(total_checkouts = sum(Checkouts))

# graph
ggplot(checkouts_per_month_material) +
  geom_line(mapping = aes(x = checkout_date,
                          y = total_checkouts,
                          color = MaterialType)) + 
  ylim(0, 500) +
  labs(title = "Sally Rooney 2022 SPL Checkouts by Book Type",
       x = "Date",
       y = "Number of Checkouts") +
  labs(color= 'Book Type') +
  scale_color_manual(labels = c("AudioBook", "eBook"),
                     values = c("salmon", "turquoise3"))





