# Bibit Problem Test
# ------------------
# Exploratory by datasets
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

# all IDs in customers are also in positions 
mean(sort(unique(customers$ID)) == sort(unique(positions$ID)))



# Exploratory Dataset 1 ---------------------------------------------------

customers %>% summary

# factor data
qplot(customers$gender)
qplot(customers$ref_code)
qplot(customers$occupation)
qplot(customers$income)
qplot(customers$income_source)

table(customers$gender) / length(customers$gender)
table(customers$ref_code) / length(customers$ref_code)
table(customers$occupation) / length(customers$occupation)
table(customers$income) / length(customers$income)
table(customers$income_source) / length(customers$income_source)


# numeric data
qplot(customers$age, binwidth = 1)
qplot(customers$age, geom = "boxplot")

summary(customers$age)


# datetime data
qplot(hour(customers$reg_datetime), geom = "bar")
ggplot(customers, aes(x = as.factor(date(reg_datetime)))) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



# Exploratory Dataset 2 ---------------------------------------------------

positions
positions %>% summary

# group by product
group_by(positions, product) %>% 
  summarise(invested = sum(invested), AUM = sum(AUM)) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(x = product, y = value, fill = factor(name))) +
  geom_col(position = position_dodge())

group_by(positions, product) %>% 
  summarise(invested = sum(invested), AUM = sum(AUM)) %>% 
  mutate(prop_invested = invested / sum(invested),
         prop_AUM = AUM / sum(AUM))


# group by date
group_by(positions, date) %>% 
  summarise(invested = sum(invested), AUM = sum(AUM)) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line(size = 1.5) +
  geom_smooth(se = FALSE, colour = "grey30", method = "lm")

group_by(positions, yday = yday(date)) %>% 
  summarise(invested = sum(invested)) %>% 
  lm(invested ~ yday, .)


# group by date and product
group_by(positions, product, date) %>% 
  summarise(invested = sum(invested), AUM = sum(AUM)) %>%
  pivot_longer(-c(1, 2)) %>% 
  ggplot(aes(x = date, y = value, colour = product)) +
  geom_line(size = 1.5) +
  facet_wrap(.~name)

group_by(positions, product, date) %>% 
  summarise(invested = sum(invested), AUM = sum(AUM)) %>%
  pivot_longer(-c(1, 2)) %>% 
  ggplot(aes(x = date, y = value, colour = product)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  facet_wrap(.~name)

group_by(positions, product, yday = yday(date)) %>% 
  summarise(invested = sum(invested)) %>% 
  split(~product) %>% 
  map(~lm(invested ~ yday, data = .))
