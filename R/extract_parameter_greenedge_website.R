# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Export the list of parameter found on the GE website. This list has been then
# imported in Google Sheet for manual work.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(xml2)
library(rvest)

rm(list = ls())

df <- read_html("data/raw/Green Edge Parameters.html")

res <- df %>% html_nodes("table") %>% 
  html_table()

res <- bind_rows(res) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  select(-(s_f:fiche)) %>% 
  rename(pi = resp) %>% 
  rename(parameter = no_param) %>% 
  mutate(parameter = str_remove(parameter, "^\\(\\d+\\)\\W+"))

res %>% 
  write_csv("data/clean/green_edge_parameters.csv")
