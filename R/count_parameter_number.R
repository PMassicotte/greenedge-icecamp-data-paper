# Count the numer of parameters we have in the table 2.

gs_title("table2") %>% 
  gs_read(ws = "parameters") %>% 
  janitor::clean_names() %>% 
  distinct(parameter_for_table_2)
