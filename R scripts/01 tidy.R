# Bibit Problem Test
# ------------------
# Tidy Data
# Hanzholah Shobri
# 2022-01-31

library(tidyverse)



# Import ------------------------------------------------------------------

dat1 <- read_csv("./data/raw/Stockbit_Bibit_PST_dataset1.csv")
dat2 <- read_csv("./data/raw/Stockbit_Bibit_PST_dataset2.csv")

sample_n(dat1, 15)
sample_n(dat2, 15)



# Tidy: Dataset 1 ---------------------------------------------------------

# dat1 headers
names(dat1) <- c("ID", "reg_datetime", "gender", "age", "occupation", "income",
                 "ref_code", "income_source")

# dat1 data types
dat1 <- dat1 %>% 
  mutate_if(is_character, as_factor) %>% 
  mutate(ref_code = ifelse(!is.na(ref_code), T, F),
         income = fct_relevel(income, 
                              c("< 10 Juta", 
                                "Rp 10 Juta - 50 Juta",
                                "> Rp 50 Juta - 100 Juta", 
                                "> Rp 100 Juta - 500 Juta",
                                "> Rp 500 Juta - 1 Miliar", 
                                "> Rp 1 Miliar")),
         income_source = fct_infreq(income_source),
         occupation = fct_infreq(occupation))
levels(dat1$income)
levels(dat1$income_source)



# Tidy: Dataset 2 ---------------------------------------------------------

# dat2 table format
dat2 <- dat2 %>% pivot_longer(-c(1:2)) %>% na.omit() %>% 
  mutate(product = str_replace_all(name, "_", " ") %>% 
           str_extract("(Saham|Pasar Uang|Pendapatan Tetap|Campuran)") %>% 
           tolower(),
         names = ifelse(str_detect(name, "AUM"), "AUM", "invested")) %>% 
  select(-name) %>%
  mutate_if(is_character, as_factor) %>% 
  pivot_wider(names_from = names, values_from = value) %>% 
  select("ID" = 1, 2, 3, 5, 4)



# Export ------------------------------------------------------------------

# last check datasets 1 and 2
dat1 %>% glimpse
dat2 %>% glimpse

dat1 %>% summary
dat2 %>% summary

# export datasets 1 and 2
write_csv(dat1, file = "./data/tidy/dataset1.csv")
write_csv(dat2, file = "./data/tidy/dataset2.csv")

save(dat1, file = "./data/tidy/dataset1.Rdata")
save(dat2, file = "./data/tidy/dataset2.Rdata")