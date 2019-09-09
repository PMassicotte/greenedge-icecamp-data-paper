# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Create a LaTeX pastable list of authors. To be used within the ESSD LaTeX
# template.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the Google sheet ---------------------------------------------------

authors <- gs_title("table2") %>% 
  gs_read(ws = "authors") %>% 
  janitor::clean_names() %>% 
  arrange(lastname) %>% 
  rename_at(vars(contains("institution")), ~str_replace(., ".*", "institution"))

# Add myself and Marcel as first and last authors -------------------------

authors <- authors %>%
  filter(!str_detect(lastname, "Massicotte|Babin")) %>%
  add_row(
    author = "Massicotte P.",
    firstname = "Philippe",
    lastname = "Massicotte",
    email = "philippe.massicotte@takuvik.ulaval.ca",
    institution = "UMI Takuvik, CNRS/Université Laval, Québec, QC Canada",
    orcid_fill_if_you_have_one = "0000-0002-5919-4116",
    .before = 1
  ) %>%
  add_row(
    author = "Babin M.",
    firstname = "Marcel",
    lastname = "Babin",
    email = "marcel.babin@takuvik.ulaval.ca",
    institution = "UMI Takuvik, CNRS/Université Laval, Québec, QC Canada"
  )

# Extract the institution -------------------------------------------------

extract_institution <- function(institution) {
  
  institution %>% 
    str_split("\\(\\d*\\)") %>% 
    map(function(x){x[!x ==""]}) %>% 
    map(str_trim) %>% 
    unlist()
}

authors <- authors %>% 
  replace_na(list(institution = "unidentified")) %>% 
  mutate(institution = map(institution, extract_institution)) %>% 
  unnest(cols = institution)

library(data.table)

authors <- authors %>% 
  select(lastname, firstname, email, orcid = orcid_fill_if_you_have_one, institution) 

setDT(authors)[, id := .GRP, by = institution]

authors %>% 
  as_tibble() %>% 
  mutate(affiliation = paste0("affiliation", id)) %>% 
  select(-id) %>% 
  pivot_wider(names_from = affiliation, values_from = institution) %>% 
  write_csv("~/Desktop/affiliations.csv")
