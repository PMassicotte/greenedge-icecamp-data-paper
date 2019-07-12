# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figure showing the temporal evolution of chla in both ice and water.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

chla <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/pigments/greenedge_pigments.csv", guess_max = 1e6) %>% 
  filter(str_detect(mission, "ice_camp")) %>% 
  filter(sample_type %in% c("water", "ice")) %>% 
  filter(pigment == "Chlorophyll a") %>% 
  filter(sample_source != "underice") %>% 
  filter(method == "HPLC")


# Water chla --------------------------------------------------------------

## Keep profiles with at least measures at 3 different depths
chla_water_m2 <- chla %>% 
  filter(sample_type == "water") %>% 
  filter(depth_m <= 100) %>% # Should we use 40m?
  drop_na(conc_mg_m3) %>% 
  group_by(mission, date) %>% 
  mutate(n = n()) %>% 
  filter(n >= 3)

## Integrate over water column
chla_water_m2 <- chla_water_m2 %>% 
  arrange(depth_m) %>% 
  group_by(mission, date, sample_type) %>% 
  nest() %>% 
  mutate(tchla_mg_m2 = map_dbl(data, ~pracma::trapz(.$depth_m, .$conc_mg_m3))) %>% # 1 ug L == 1 mg m-3
  select(-data)


# Ice chla ----------------------------------------------------------------

## Extract ice chla which is already in mg X m2
chla_ice_m2 <- chla %>%
  filter(sample_type == "ice") %>% 
  filter(sample_source %in% c("0-3 cm", "3-10 cm")) %>% 
  group_by(mission, sample_type, date, sample_source) %>% 
  summarise(conc_mg_m2 = mean(conc_mg_m2)) %>% 
  group_by(mission, date) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  group_by(mission, date, sample_type) %>% 
  summarise(tchla_mg_m2 = sum(conc_mg_m2))

chla_m2 <- bind_rows(chla_ice_m2, chla_water_m2) %>% 
  mutate(doy = as.numeric(format(date, "%j"))) %>% 
  mutate(year = as.numeric(format(date, "%Y")))


# Plot --------------------------------------------------------------------

mylabels <- c(
  "2015" = "Ice camp 2015",
  "2016" = "Ice camp 2016"
)

p <- chla_m2 %>%
  mutate(depth_level = ifelse(is.na(sample_type), "water", sample_type)) %>%
  mutate(group = ifelse(year == 2015 & between(doy, 130, 170) & depth_level == "ice", "snow", NA)) %>% 
  ggplot(aes(x = doy, y = tchla_mg_m2, color = depth_level)) +
  geom_line(size = 0.25) +
  geom_point(show.legend = FALSE, size = 0.5) +
  facet_wrap( ~ year, ncol = 1, labeller = labeller(year = mylabels)) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  xlab(NULL) +
  ylab(bquote("Total chlorophyll a ("*mg~m^{-2}*")")) +
  theme(plot.subtitle = element_text(size = 8)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    limits = c(90, 210),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
  scale_color_brewer(palette = "Set2", breaks = c("ice", "water"), labels = c("Ice", "Water")) +
  theme(legend.justification = c(0, 1), legend.position = c(0.07, 0.99)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 0.5)))

# anot <- tibble(x = 150, y = 50, depth_level = "ice", label = "sadsdf", year = 2015)
# 
# p + 
#   geom_text(data = anot, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 2)

ggsave("graphs/fig8.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)

# Stats for the paper -----------------------------------------------------

# Minimum ice chla at the end of the sampling

chla_m2 %>% 
  group_by(mission, sample_type) %>% 
  filter(date == max(date))

  