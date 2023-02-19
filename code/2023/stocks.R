library(tidyverse)
library(showtext)
library(ggh4x)
library(patchwork)


big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

font_add_google("Righteous", "righteous")
showtext_auto()

#clean companies names
clean_names <- big_tech_companies %>%
  mutate(
    clean_name = tolower(stringi::stri_extract_first_words(company, 1)),
    clean_name = case_when(
      str_detect(clean_name, "international") ~ "ibm",
      str_detect(clean_name, "amazon") ~ "amazon",
      TRUE ~ clean_name))
  


adobe_tesla <- big_tech_stock_prices %>%
    mutate(date = as.Date(date)) %>% 
  filter(stock_symbol == "ADBE" | stock_symbol == "TSLA") %>% 
  select(date, stock_symbol, adj_close) %>% 
  pivot_wider(names_from = stock_symbol, values_from =adj_close) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = ADBE, color = "ADBE")) +
  geom_line(aes(y = TSLA, color = "TSLA")) +
  stat_difference(aes(ymin = TSLA, ymax = ADBE), alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  theme_minimal() +
  theme(legend.position = "top")

adobe_apple <- big_tech_stock_prices %>%
  mutate(date = as.Date(date)) %>% 
  filter(stock_symbol == "ADBE" | stock_symbol == "AAPL") %>% 
  select(date, stock_symbol, adj_close) %>% 
  pivot_wider(names_from = stock_symbol, values_from =adj_close) %>%
  ggplot(aes(x = date)) +
  # One line for Cat rescues
  geom_line(aes(y = ADBE, color = "adobe")) +
  # Another line for Not_Cat rescues
  geom_line(aes(y = AAPL, color = "apple")) +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  stat_difference(aes(ymin = AAPL, ymax = ADBE), alpha = 0.3) +
  theme_minimal()

adobe_amazon <- big_tech_stock_prices %>%
  mutate(date = as.Date(date)) %>% 
  filter(stock_symbol == "ADBE" | stock_symbol == "AMZN") %>% 
  select(date, stock_symbol, adj_close) %>% 
  pivot_wider(names_from = stock_symbol, values_from =adj_close) %>% 
  ggplot(aes(x = date)) +
  # One line for Cat rescues
  geom_line(aes(y = ADBE, color = "ADBE")) +
  # Another line for Not_Cat rescues
  geom_line(aes(y = AMZN, color = "AMZN")) +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  # stat_difference() from ggh4x package applies the conditional fill
  # based on which of Not_Cat and Cat is larger.
  stat_difference(aes(ymin = AMZN, ymax = ADBE), alpha = 0.3) +
  theme_minimal()

adobe_tesla + adobe_apple / adobe_amazon
