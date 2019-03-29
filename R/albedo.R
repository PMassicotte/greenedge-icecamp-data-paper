rm(list = ls())

df <- data.table::fread("/media/data4tb/greenedege/2016/optics_Verin/albedo/Albedo_GE2016_colonnes.txt") %>% 
  as_tibble()

df <- df %>% 
  gather(wavelenght, albedo, starts_with("Albedo"), convert = TRUE) %>% 
  mutate(wavelenght = parse_number(wavelenght)) %>% 
  janitor::clean_names()

df

unique(df$data_name)

df %>% 
  filter(data_name == "day37-tr-75") %>% 
  filter(between(wavelenght, 400, 700)) %>% 
  ggplot(aes(x = wavelenght, y = albedo)) +
  geom_line() +
  facet_wrap(~measurement_number, scales = "free") +
  scale_y_continuous(labels = scales::percent)


res <- df %>% 
  group_by(expedition, julian_day, wavelenght) %>% 
  summarise(albedo = mean(albedo), n = n())

res %>% 
  slice(which.min(abs(wavelenght - 450))) %>% 
  ggplot(aes(x= julian_day, y = albedo)) +
  geom_point()

