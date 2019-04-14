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

extract_parameters_cyber_website <- function(file) {
  df <- read_html("data/raw/Green Edge Parameters_2016.html")

  res <- df %>%
    html_nodes("table") %>%
    html_table()

  res <- bind_rows(res) %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    select(-(s_f:fiche)) %>%
    rename(pi = resp) %>%
    rename(parameter = no_param) %>%
    mutate(parameter = str_remove(parameter, "^\\(\\d+\\)\\W+"))

  res <- res %>%
    # mutate_if(is.character, iconv, from = "UTF-8", to = "LATIN1") %>%
    mutate(year = str_match(file, "\\d+"))

  return(res)
}

res <- map_df(
  list(
    "data/raw/Green Edge Parameters_2015.html",
    "data/raw/Green Edge Parameters_2016.html"
  ),
  extract_parameters_cyber_website
)

res

res %>%
  write_csv("data/clean/green_edge_parameters.csv")

# Extract PI ------------------------------------------

pi <- res %>% 
  # distinct(pi) %>%
  mutate(pi = str_split(pi, "/|,")) %>% 
  unnest() %>% 
  mutate(pi = str_trim(pi)) %>% 
  distinct(pi) %>% 
  arrange(pi)

pi

write_csv(pi, "data/clean/pi.csv")
