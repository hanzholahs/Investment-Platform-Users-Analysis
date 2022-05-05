library(tidyverse)
library(lubridate)
library(patchwork)
library(ggthemes)


# Import ------------------------------------------------------------------

load("./data/tidy/dataset1.Rdata")
load("./data/tidy/dataset2.Rdata")

customers <- dat1
positions <- dat2

rm(dat1)
rm(dat2)

# Dividing age into groups
customers <- mutate(customers, age_group = cut(age, breaks = seq(15, 85, 5)))

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
         sell = ifelse(inv_diff < 0, TRUE, FALSE),
         margin = ifelse(!is.infinite(margin), margin, 0)) %>% 
  arrange(ID, date, product)

# Aggregating data from the sources
users <- inner_join(customers, positions, by = "ID")



# General Overview --------------------------------------------------------

# number of users
customers

# date range
range(positions$date)

# total firstly bought investment per product
users_total_first_buy_per_product <- c(
  saham = filter(users, product == "saham") %>% split(~ID) %>% 
    map_dbl(~slice(., 1) %>% pull(invested)) %>% sum,
  `pasar uang` = filter(users, product == "pasar uang") %>% split(~ID) %>% 
    map_dbl(~slice(., 1) %>% pull(invested)) %>% sum,
  `pendapatan tetap` = filter(users, product == "pendapatan tetap") %>% 
    split(~ID) %>% map_dbl(~slice(., 1) %>% pull(invested)) %>% sum,
  campuran = filter(users, product == "campuran") %>% split(~ID) %>% 
    map_dbl(~slice(., 1) %>% pull(invested)) %>% sum
)

# total funds invested per product
users_total_next_buys_per_product <- filter(users, buy) %>% 
  group_by(product) %>% 
  summarise(next_buys = sum(invested)) %>% 
  mutate(first_buy = users_total_first_buy_per_product,
         # # check that rows are inserted correctly
         # product2 = names(users_total_first_buy_per_product),
         total_buy = first_buy + next_buys,
         percentage = scales::percent(total_buy / sum(total_buy)),
         product = fct_reorder(product, total_buy))

filter(positions, date %in% range(positions$date)) %>% 
  group_by(date, product) %>% 
  summarise(total_invested = sum(invested)) %>% 
  pivot_wider(names_from = date, values_from = total_invested) %>% 
  mutate(
    difference = `2021-10-29` - `2021-08-04`,
    growth_rate = (`2021-10-29` / `2021-08-04`) ^ 
      (1/length(unique(positions$date)))
  )


# invested amount growth
users_total_next_buys_per_product %>% 
  ggplot(aes(x = "", y = total_buy, fill = fct_rev(product))) +
  geom_col(colour = "grey20") +
  geom_label(aes(label = percentage), color = "white", fill = "grey10", 
             size = 5, position = position_stack(vjust = 0.5), 
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  scale_fill_ptol() +
  labs(fill = "Product") +
  theme_tufte() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

group_by(positions, product, date) %>% 
  summarise(total_invested = sum(invested)) %>%
  ggplot(aes(x = date, y = total_invested, colour = product, 
             linetype = product)) +
  geom_line(size = 1.25) +
  scale_x_date(breaks = seq(min(positions$date), max(positions$date), 
                            by = "week"),
               labels = scales::date_format("%d %b")) +
  scale_y_continuous(labels = scales::number_format(scale = 10^(-6)),
                     breaks = seq(0, 1.75e10, 2.5e9)) +
  scale_fill_ptol() +
  labs(y = "Invested Amount (in million)", x = NULL,
       colour = "Product", linetype = "Product") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.75, vjust = 1),
        legend.position="bottom")



# Users Demographics ------------------------------------------------------



demographics_counts <- map(customers[, demographics_cols], table) %>% 
  map(~tibble(level = names(.), value = .)) %>% 
  bind_rows() %>% 
  mutate(attribute = rep(names(customers)[demographics_cols],
                         demographics_cols_length))

demographics_counts %>% 
  ggplot(aes(x = value, y = level, fill = attribute)) +
  geom_col() +
  facet_grid(attribute~., scales = "free", space = "free") +
  theme_tufte()


# Investment by Gender
p_dem_1 <- inner_join(customers, positions, by = "ID") %>% 
  group_by(gender) %>% 
  summarise(invested = sum(invested) / length(unique(positions$date))) %>%
  inner_join(summarise(group_by(customers, gender), count = n()), 
             by = "gender") %>% 
  mutate(inv_per_tot_user = as.numeric(invested / 
                                         table(customers$gender)) / 1e3) %>% 
  select(-invested) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(y = gender, x = value, fill = gender)) +
  geom_col() +
  facet_wrap(~name, scales = "free_x") +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = NULL, y = "Gender") +
  theme_tufte() + 
  theme(strip.text.x = element_blank())

# Investment by Occupation
p_dem_2 <- inner_join(customers, positions, by = "ID") %>% 
  group_by(occupation) %>% 
  summarise(invested = sum(invested) / length(unique(positions$date))) %>%
  inner_join(summarise(group_by(customers, occupation), count = n()), 
             by = "occupation") %>% 
  mutate(inv_per_tot_user = as.numeric(invested /
                                         table(customers$occupation)) / 1e3)%>% 
  select(-invested) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(y = occupation, x = value, fill = occupation )) +
  geom_col() +
  facet_wrap(~name, scales = "free_x") +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = NULL, y = "Occupation") +
  theme_tufte() + 
  theme(strip.text.x = element_blank())

# Investment by Income Source income_source
p_dem_3 <- inner_join(customers, positions, by = "ID") %>% 
  group_by(income_source) %>% 
  summarise(invested = sum(invested) / length(unique(positions$date))) %>%
  inner_join(summarise(group_by(customers, income_source), count = n()), 
             by = "income_source") %>% 
  mutate(inv_per_tot_user = as.numeric(invested /
                                         table(customers$income_source))/1e3)%>% 
  select(-invested) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(y = income_source, x = value, fill = income_source )) +
  geom_col() +
  facet_wrap(~name, scales = "free_x") +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = NULL, y = "Income_source") +
  theme_tufte() + 
  theme(strip.text.x = element_blank())

# Investment by Income
p_dem_4 <- inner_join(customers, positions, by = "ID") %>% 
  group_by(income) %>% 
  summarise(invested = sum(invested) / length(unique(positions$date))) %>%
  inner_join(summarise(group_by(customers, income), count = n()), 
             by = "income") %>% 
  mutate(inv_per_tot_user = as.numeric(invested /
                                         table(customers$income)) / 1e3)%>% 
  select(-invested) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(y = income, x = value, fill = income )) +
  geom_col() +
  facet_wrap(~name, scales = "free_x") +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = NULL, y = "Income") +
  theme_tufte() + 
  theme(strip.text.x = element_blank())

# Investment by age group
p_dem_5 <- inner_join(customers, positions, by = "ID") %>% 
  group_by(age_group) %>% 
  summarise(invested = sum(invested) / length(unique(positions$date))) %>%
  inner_join(summarise(group_by(customers, age_group), count = n()), 
             by = "age_group") %>% 
  mutate(inv_per_tot_user = as.numeric(invested /
                                         table(customers$age_group)) / 1e3)%>% 
  select(-invested) %>% 
  pivot_longer(-1) %>% 
  ggplot(aes(y = age_group, x = value, fill = age_group )) +
  geom_col() +
  facet_wrap(~name, scales = "free_x") +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = NULL, y = "Age Group") +
  guides(fill = "none") +
  theme_tufte() + 
  theme(strip.text.x = element_blank())

# Combining the graphs
(p_dem_1 / p_dem_2 / p_dem_3 / p_dem_4) +
  plot_layout(design = "A\nA\nA
                        B\nB\nB\nB\nB\nB\nB\nB\nB
                        C\nC\nC\nC\nC\nC\nC\nC\nC\nC
                        D\nD\nD\nD\nD\nD") &
  guides(fill = "none") 



# User Retention ----------------------------------------------------------

# Proportion of user retention
users %>% 
  group_by(ID) %>% 
  summarise(retain = sum(buy) > 0) %>% 
  group_by(retain) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count),
         percentage = scales::percent(prop),
         retain = ifelse(retain, "Yes", "No"),
         retain = fct_reorder(retain, prop)) %>% 
  ggplot(aes(x = "", y = prop, fill = retain)) +
  geom_col() +
  geom_label(aes(label = percentage), color = "white", fill = "grey10", 
             size = 5, position = position_stack(vjust = 0.5), 
             show.legend = FALSE) +
  scale_fill_ptol() +
  coord_polar(theta = "y") +
  theme_tufte() +
  labs(fill = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")

# Retention categories
users_retention <- inner_join(filter(positions, buy), customers, by = "ID") %>% 
  select(names(customers), inv_diff) %>% 
  group_by(ID) %>% 
  summarise(count = n()) %>% 
  full_join(customers, by = "ID") %>% 
  mutate(count = ifelse(!is.na(count), count, 0),
         count_group = case_when(
           count == 0 ~ "no buy",
           count <= 3 ~ "1-3 buys",
           count <= 6 ~ "4-6 buys",
           count <= 9 ~ "7-9 buys",
           count > 9 ~ "> 9 buys") %>% 
           factor(levels = c("no buy", "1-3 buys", "4-6 buys", "7-9 buys", 
                             "> 9 buys")))
# number of buys
users_retention %>% 
  ggplot(aes(x = count_group)) +
  geom_bar() +
  theme_tufte() +
  labs(x = "Buys in Aug-Oct 2021", y = "Number of users")

summary(filter(users_retention, count > 0) %>% pull(count))

# Buy timing aggregate
filter(positions, buy) %>%
  group_by(date, product) %>% 
  summarise(inv_diff = sum(inv_diff)) %>% 
  ggplot(aes(x = date, y = inv_diff)) +
  geom_area(aes(fill = product), position = position_stack()) + 
  geom_smooth(se = FALSE, colour = "grey20", show.legend = FALSE) 

# Buy timing by product
filter(positions, buy) %>%
  group_by(date, product) %>% 
  summarise(inv_diff = sum(inv_diff)) %>% 
  ggplot(aes(x = date, y = inv_diff, fill = product)) +
  geom_area() + 
  geom_smooth(se = FALSE, colour = "grey20", show.legend = FALSE) +
  facet_wrap(.~product, ncol = 4, scales = "free_y") +
  scale_x_date(breaks = seq(min(positions$date), max(positions$date), 
                            by = "5 days"),
               labels = scales::date_format("%d %b")) +
  scale_y_continuous(labels = scales::number_format(scale = 10^(-6))) +
  scale_fill_ptol() +
  labs(x = NULL, fill = NULL,
       y = "Additional Amount of Money\nInvested (in Million)") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")

# high retention users
users_retention <- mutate(users_retention, retain = count > 0)

total_new_buys_per_ID <- filter(users, buy) %>% 
  group_by(ID) %>% 
  summarise(total = sum(inv_diff))

ID_high_retention_value <- filter(users, buy) %>% 
  group_by(ID) %>% 
  summarise(total = sum(inv_diff)) %>% 
  filter(total >= quantile(total_new_buys_per_ID$total, probs = 0.95))

high_user_retention <- filter(users_retention, 
                              count > 3, 
                              ID %in% ID_high_retention_value$ID)

(ggplot(high_user_retention, aes(x = age_group, fill = age_group)) +
    geom_bar() +
    scale_fill_ptol() +
    labs(y = "Age Group",x = NULL, fill = NULL)  +
    theme_tufte()) /
  (ggplot(high_user_retention, aes(x = occupation, fill = occupation)) +
     geom_bar() +
     scale_fill_ptol() +
     labs(y = "Occupation",x = NULL, fill = NULL)  +
     theme_tufte()) /
  (ggplot(high_user_retention, aes(x = income, fill = income)) +
     geom_bar() +
     scale_fill_ptol() +
     labs(y = "Income",x = NULL, fill = NULL)  +
     theme_tufte()) /
  (ggplot(high_user_retention, aes(x = income_source, fill = income_source)) +
     geom_bar() +
     scale_fill_ptol() +
     labs(y = "Income Source",x = NULL, fill = NULL)  +
     theme_tufte()) +
  plot_layout(design = "A\nA\nA\nA\nA\nA\nA\nA\nA\nA
                        B\nB\nB\nB\nB\nB\nB
                        C\nC\nC\nC\nC\nC
                        D\nD\nD\nD\nD\nD\nD\nD\nD") &
  coord_flip() &
  guides(fill = "none") &
  theme(legend.position = "bottom")

# # New buy user profiles
# (ggplot(users_retention, aes(x = gender, fill = retain)) +
#     geom_bar(position = position_fill()) +
#     ggplot(users_retention, aes(x = occupation, fill = retain)) +
#     geom_bar(position = position_fill()) +
#     ggplot(users_retention, aes(x = income, fill = retain)) +
#     geom_bar(position = position_fill())) /
#   (ggplot(users_retention, aes(x = income_source, fill = retain)) +
#      geom_bar(position = position_fill()) +
#      ggplot(users_retention, aes(x = age_group, fill = retain)) +
#      geom_bar(position = position_fill()) +
#      ggplot(users_retention, aes(x = ref_code, fill = retain)) +
#      geom_bar(position = position_fill()))
# 
# (ggplot(filter(users_retention, count < 10, count > 0),
#         aes(x = occupation, y = count)) + geom_boxplot() +
#     ggplot(filter(users_retention, count < 10, count > 0),
#            aes(x = income, y = count)) + geom_boxplot() +
#     ggplot(filter(users_retention, count < 10, count > 0),
#            aes(x = income_source, y = count)) + geom_boxplot()) / 
#   (ggplot(filter(users_retention, count < 10, count > 0),
#           aes(x = age_group, y = count)) + geom_boxplot() +
#      ggplot(filter(users_retention, count < 10, count > 0),
#             aes(x = ref_code, y = count)) + geom_boxplot() +
#      ggplot(filter(users_retention, count < 10, count > 0),
#             aes(x = gender, y = count)) + geom_boxplot())



# Asset Under Management (AUM) --------------------------------------------

# Users LTV
users_LTV <- positions %>% 
  group_by(ID, date) %>% 
  summarise(LTV = sum(AUM) * 0.01 / 100) %>% 
  summarise(avg_LTV = mean(LTV)) %>% 
  inner_join(customers, by = "ID")

summary(users_LTV$avg_LTV)
filter(users_LTV, avg_LTV > 50000) %>% 
  ggplot(aes(x = avg_LTV)) + geom_histogram(bins = 20)

(ggplot(users_LTV, aes(x = gender, y = avg_LTV)) + geom_boxplot() +
    ggplot(users_LTV, aes(x = occupation, y = avg_LTV)) + geom_boxplot() +
    ggplot(users_LTV, aes(x = income, y = avg_LTV)) + geom_boxplot() +
    ggplot(users_LTV, aes(x = income_source, y = avg_LTV)) + geom_boxplot() +
    ggplot(users_LTV, aes(x = age_group, y = avg_LTV)) + geom_boxplot() +
    ggplot(users_LTV, aes(x = ref_code, y = avg_LTV)) + geom_boxplot())

# Return Growth
positions %>%
  group_by(date, product) %>% 
  summarise(margin = mean(margin)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = margin, colour = product)) +
  geom_line(size = 1.3) +
  scale_x_date(breaks = seq(min(positions$date), max(positions$date), 
                            by = "week"),
               labels = scales::date_format("%d %b")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_ptol() +
  labs(x = NULL, y = "Return", colour = NULL) +
  theme_tufte() +
  theme(legend.position = "bottom")

ID_high_LTV <- users_LTV %>% 
  arrange(desc(avg_LTV)) %>% 
  head(500) %>% pull(ID)

users_LTV %>% 
  arrange(desc(avg_LTV)) %>% 
  head(500) %>% 
  summary

ID_high_LTV


# high user LTV
quantile(users_LTV$avg_LTV, probs = 0.95)


total_new_buys_per_ID <- filter(users, buy) %>% 
  group_by(ID) %>% 
  summarise(total = sum(inv_diff))

high_LTV <- users_LTV %>% 
  filter(avg_LTV >= quantile(users_LTV$avg_LTV, probs = 0.95))


(ggplot(high_LTV, aes(x = age_group, fill = age_group)) +
    geom_bar() +
    labs(y = "Age Group",x = NULL, fill = NULL)  +
    theme_tufte()) /
  (ggplot(high_LTV, aes(x = occupation, fill = occupation)) +
     geom_bar() +
     labs(y = "Occupation",x = NULL, fill = NULL)  +
     theme_tufte()) /
  (ggplot(high_LTV, aes(x = income, fill = income)) +
     geom_bar() +
     labs(y = "Income",x = NULL, fill = NULL)  +
     theme_tufte()) /
  (ggplot(high_LTV, aes(x = income_source, fill = income_source)) +
     geom_bar() +
     labs(y = "Income Source",x = NULL, fill = NULL)  +
     theme_tufte()) +
  plot_layout(design = "A\nA\nA\nA\nA\nA\nA\nA\nA\nA\nA\nA\nA
                        B\nB\nB\nB\nB\nB\nB\nB\nB
                        C\nC\nC\nC\nC\nC
                        D\nD\nD\nD\nD\nD\nD\nD\nD\nD") &
  coord_flip() &
  guides(fill = "none") &
  theme(legend.position = "bottom")
