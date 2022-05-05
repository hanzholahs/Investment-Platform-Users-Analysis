# Bibit Problem Test
# ------------------
# Users Behavioural Exploratory
# Hanzholah Shobri
# 2022-01-31

library(tidyverse)
library(lubridate)



# Import ------------------------------------------------------------------

load("./data/tidy/dataset1.Rdata")
load("./data/tidy/dataset2.Rdata")

customers <- dat1
positions <- dat2

rm(dat1)
rm(dat2)

customers <- mutate(customers, age_group = cut(age, breaks = seq(15, 85, 5)))



# Analysis ----------------------------------------------------------------

# Calculating profit margin
positions <- mutate(positions, 
                    margin = (AUM - invested) / invested,
                    margin = ifelse(!is.nan(margin), margin, 0),
                    profit = ifelse(margin > 0, TRUE, FALSE))

# Calculating and identifying customer buys
positions <- positions %>% 
  split(~product) %>% 
  map(~group_by(., ID)) %>% 
  map(~mutate(., inv_diff = c(0, diff(invested)))) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(buy  = ifelse(inv_diff > 0, TRUE, FALSE),
         sell = ifelse(inv_diff < 0, TRUE, FALSE)) %>% 
  arrange(ID, date, product)

# Average number of buys
filter(positions, buy) %>% pull(ID) %>% table %>% as.vector %>% summary
filter(positions, buy) %>% pull(ID) %>% table %>% as.vector %>% 
  qplot(binwidth = 1)


# Relationship Between Buy Decision and Date/Time -------------------------

# Buy dates
filter(positions, buy) %>% 
  ggplot(aes(x = date, fill = product)) + 
  geom_bar() + 
  scale_x_date(breaks = seq(min(positions$date), max(positions$date), 
                            by = "day")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

filter(positions, buy) %>% 
  group_by(date, product) %>% 
  summarise(buy_amount= sum(inv_diff)) %>% 
  ggplot(aes(x = date, y = buy_amount, fill = product)) + 
  geom_col() + 
  scale_x_date(breaks = seq(min(positions$date), max(positions$date), 
                            by = "day")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Buy day
filter(positions, buy) %>% 
  group_by(wday = wday(date), product) %>% 
  summarise(vol_buy = n()) %>% 
  ggplot(aes(x = wday, y = vol_buy, fill = product)) +
  geom_col()

filter(positions, buy) %>% 
  group_by(wday = wday(date), product) %>% 
  summarise(nom_buy = sum(inv_diff)) %>% 
  ggplot(aes(x = wday, y = nom_buy, fill = product)) +
  geom_col()



# Who Are More Likely to Buy Again? ---------------------------------------

cust_buy <- inner_join(filter(positions, buy), customers, by = "ID") %>% 
  select(names(customers), inv_diff)

cust_buy %>% 
  group_by(ID) %>% 
  ggplot(aes(x = age_group, y = inv_diff)) +
  geom_col()

cust_retention <- cust_buy %>% 
  group_by(ID) %>% 
  summarise(count = n()) %>% 
  full_join(customers, by = "ID") %>% 
  mutate(count = ifelse(!is.na(count), count, 0),
         count_group = case_when(
           count == 0 ~ "no retention",
           count <= 3 ~ "low retention",
           count <= 6 ~ "med retention",
           count <= 9 ~ "hi retention",
           count > 9 ~ "very hi retention"))

map(c("gender", "occupation", "income", "ref_code", "income_source", 
      "age_group"),
    ~prop.table(table(cust_retention$count_group, cust_retention[[.]]), 2))

ggplot(cust_retention, aes(x = ref_code, y = 1, fill = count_group)) +
  geom_col(position = position_fill())



# LTV ---------------------------------------------------------------------

cust_LTV <- positions %>% 
  group_by(ID, date) %>% 
  summarise(LTV = sum(AUM) * 0.01 / 100) %>% 
  summarise(avg_LTV = mean(LTV)) %>% 
  inner_join(customers, by = "ID")




# Testing -----------------------------------------------------------------

# # Test on diff function
# diff(c(4,6,3,4,7)) 
# c(4,6,3,4,7)[-1] - c(4,6,3,4,7)[-5]

# Test on customer buy timing
# positions[1:500, ] %>%
#   split(~product) %>%
#   map(~group_by(., ID)) %>%
#   map(~mutate(., inv_diff = c(1, diff(invested)))) %>% # 1 for every new IDs row
#   walk(print, n = Inf)
#   bind_rows() %>%
#   mutate(buy = ifelse(inv_diff > 0, TRUE, FALSE)) %>%
#   arrange(ID, date, product) %>%
#   print(n = 150)