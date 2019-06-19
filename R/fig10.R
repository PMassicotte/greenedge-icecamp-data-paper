# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Zooplankton time series.
#
# https://drive.google.com/file/d/1bK0t52DlzpjFTsOc6bu5ZKXHPH3jAd6H/view for the
# formulas
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- data.table::fread("data/raw/ecotaxa_export_801_20190523_1437.tsv", dec = ",") %>%
  as_tibble() %>%
  mutate(sampling_date = lubridate::parse_date_time(object_date, orders = "Ymd")) %>%
  mutate(sample_tot_vol = str_replace(sample_tot_vol, ",", "\\.")) %>%
  mutate(sample_tot_vol = parse_number(sample_tot_vol)) %>% 
  select(-classif_auto_name)

# Graph to check water volume against measurement depths ------------------

df %>%
  ggplot(aes(y = sample_tot_vol, x = factor(object_depth_max))) +
  geom_boxplot()

# In 2015, depth was set to 9999, which is in fact 350 m (see previous graph)

df <- df %>%
  mutate(object_depth_max = case_when(object_depth_max == 9999 ~ 350, TRUE ~ object_depth_max))

# Remove detritus and temporary observations
df <- df %>%
  filter(str_detect(object_annotation_category, "bubble|artefact|detri|^t\\d+|<|othertocheck", negate = TRUE)) %>%
  filter_if(is.character, all_vars(str_detect(., "seaweed", negate = TRUE))) %>%
  filter_if(is.character, any_vars(str_detect(., "living"))) %>% 
  # Remove algea (advice from Thibaud)
  filter_if(is.character, all_vars(str_detect(., regex("Coscinodiscus|Ceratiaceae", ignore_case = TRUE), negate = TRUE)))
# %>% 
#   filter_if(is.character, any_vars(str_detect(., regex("Copepoda", ignore_case = TRUE))))

unique(df$object_annotation_category)

# Split the hierarchy -----------------------------------------------------

# Looks like there is at maximum 12 categories

unique(df$object_annotation_hierarchy) %>%
  str_split(">") %>%
  map_int(length) %>%
  range()

df <- df %>%
  separate(object_annotation_hierarchy, into = letters[1:12], sep = ">", fill = "right")


# Check volume filtered ---------------------------------------------------

df %>% 
  ggplot(aes(x = sample_tot_vol, fill = factor(object_depth_max))) +
  geom_histogram() +
  labs(fill = "Depth max (m)")

# Count number of individuals ---------------------------------------------

res <- df %>%
  group_by(sampling_date, object_depth_max, sample_tot_vol, acq_sub_part, acq_id, i) %>%
  summarise(n = n()) %>%
  ungroup()

res

# Mean individual per liter -----------------------------------------------

res <- res %>%
  mutate(concentration_ind_m3 = (n * acq_sub_part) / sample_tot_vol) %>% 
  group_by(sampling_date, object_depth_max, i) %>%
  summarise(concentration_ind_m3 = mean(concentration_ind_m3)) %>% #TODO: Mean or sum? I think it is mean
  ungroup()

# Turn implicit missing into explicit missing -----------------------------

res <- res %>% 
  complete(nesting(object_depth_max, sampling_date), i, fill = list(concentration_ind_m3 = 0))

# Relevels ----------------------------------------------------------------

res <- res %>% 
  mutate(year = lubridate::year(sampling_date)) %>% 
  filter(year == 2016) %>% 
  mutate(i = case_when(is.na(i) ~ "Other", TRUE ~ i)) %>% 
  mutate(i = fct_relevel(i, "Other", after = Inf))

# res <- res %>% 
#   group_by(year = lubridate::year(sampling_date), object_depth_max, i) %>% 
#   mutate(prop = concentration_ind_m3 / sum(concentration_ind_m3)) %>% 
#   filter(prop >= 0.1)

# Plot --------------------------------------------------------------------

mylabel <- c(
  "100" = "Average over 100 m depth in 2016",
  "350" = "Average over 350 m depth in 2016"
)

p <- res %>%
  # mutate(i = f) %>% 
  # filter(str_detect(i, "Copepoda", negate = TRUE)) %>%
  ggplot(aes(x = sampling_date, y = concentration_ind_m3, fill = i)) +
  geom_area() +
  facet_wrap(~ object_depth_max, labeller = labeller(object_depth_max = mylabel), ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_datetime(expand = c(0, 10), date_breaks = "3 weeks", date_labels = "%b-%d") +
  xlab(NULL) +
  ylab(bquote("Abundance" ~ (ind~m^{-3}))) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  guides(fill = guide_legend(ncol = 3)) +
  scale_fill_manual(values = pals::brewer.accent(length(unique(res$i)))) +
  theme(legend.margin = margin(6, 16, 6, 6))
  
ggsave("graphs/fig10.pdf", width = 8, height = 12, units = "cm", device = cairo_pdf)


# Relative abundance ------------------------------------------------------

res %>%
  mutate(year = lubridate::year(sampling_date)) %>% 
  filter(year == 2016) %>% 
  mutate(i = case_when(is.na(i) ~ "Other", TRUE ~ i)) %>%
  mutate(i = fct_relevel(i, "Other", after = Inf)) %>% 
  group_by(object_depth_max, sampling_date) %>% 
  mutate(relative_n = concentration_ind_m3 / sum(concentration_ind_m3)) %>% 
  ggplot(aes(x = sampling_date, y = relative_n, fill = i)) +
  geom_area() +
  facet_wrap(~ object_depth_max, labeller = labeller(object_depth_max = mylabel), ncol = 1) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_x_datetime(expand = c(0, 10), date_breaks = "3 weeks", date_labels = "%b-%d") +
  xlab(NULL) +
  ylab(bquote("Abundance" ~ (ind~L^{-1}))) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 5)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  guides(fill = guide_legend(ncol = 3)) +
  scale_fill_manual(values = pals::brewer.accent(length(unique(res$i))))

ggsave("graphs/fig10_relative.pdf", width = 8, height = 12, units = "cm", device = cairo_pdf)