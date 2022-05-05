# Bibit Problem Test
# ------------------
# Exploratory by Demographics
# Hanzholah Shobri
# 2022-01-31

library(tidyverse)
library(lubridate)
library(ggthemes)



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



# Analysis on Gender ------------------------------------------------------
table(customers$gender)

# Total investment 
inner_join(customers, positions, by = "ID") %>% 
  group_by(gender) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = gender, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(gender) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested / table(customers$gender))) %>% 
  ggplot(aes(x = gender, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(gender, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = gender, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Analysis on Occupation --------------------------------------------------
table(customers$occupation)

# Total investment
inner_join(customers, positions, by = "ID") %>% 
  group_by(occupation) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = occupation, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(occupation) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested / 
                                         table(customers$occupation))) %>% 
  ggplot(aes(x = occupation, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(occupation, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = occupation, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Analysis on the Use of Referral Code ------------------------------------
table(customers$ref_code)

# Total investment
inner_join(customers, positions, by = "ID") %>% 
  group_by(ref_code) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = ref_code, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(ref_code) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested/table(customers$ref_code))) %>% 
  ggplot(aes(x = ref_code, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(ref_code, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = ref_code, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Analysis on Income ------------------------------------------------------
table(customers$income)

# Total investment
inner_join(customers, positions, by = "ID") %>% 
  group_by(income) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = income, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(income) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested / table(customers$income))) %>% 
  ggplot(aes(x = income, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(income, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = income, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Analysis on Income Source -----------------------------------------------
table(customers$income_source)

# Total investment
inner_join(customers, positions, by = "ID") %>% 
  group_by(income_source) %>% 
  summarise(invested = sum(invested)) %>%  
  ggplot(aes(x = income_source, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(income_source) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested /
                                         table(customers$income_source)))%>% 
  ggplot(aes(x = income_source, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(income_source, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = income_source, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Analysis on Income Source -----------------------------------------------
table(customers$age_group)

# Total investment
inner_join(customers, positions, by = "ID") %>% 
  group_by(age_group) %>% 
  summarise(invested = sum(invested)) %>%  
  ggplot(aes(x = age_group, y = invested)) +
  geom_col()

# Contribution per user
inner_join(customers, positions, by = "ID") %>% 
  group_by(age_group) %>% 
  summarise(invested = sum(invested)) %>% 
  mutate(inv_per_tot_user = as.numeric(invested/table(customers$age_group))) %>% 
  ggplot(aes(x = age_group, y = inv_per_tot_user)) +
  geom_col()

# Portfolio diversity
inner_join(customers, positions, by = "ID") %>% 
  group_by(age_group, product) %>% 
  summarise(invested = sum(invested)) %>% 
  ggplot(aes(x = age_group, y = invested, fill = product)) +
  geom_col(position = position_fill())



# Testing -----------------------------------------------------------------

# # Calculation of the total contribution per user
# inner_join(customers, positions, by = "ID") %>% 
#   filter(80 < age, age <= 85) %>% 
#   summarise(sum(invested) / 2)



# Save all graphics -------------------------------------------------------

# # Clear plots from the previous session 
# file.remove(list.files("./figures/Question 1/", full.names = TRUE))
# 
# # Copy all temporarily created plots in the session from 
# p.dir.path  <- list.files(tempdir(), pattern = "rs-graphics", full.names = TRUE)
# p.png.paths <- list.files(p.dir.path, pattern = ".png", full.names = TRUE)
# 
# file.copy(from = p.png.paths, to = "./figures/Question 1/")