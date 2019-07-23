# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Temporal evolution of the snow and ice thickness.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv("/mnt/nfs/scratch/mariepieramyot/backup/ice_temp_salinity_thick/greenedge_ice_thick.csv")

df <- df %>% 
  select(mission, date, sample_type, snow_thickness, sample_thickness_cm_average) %>% 
  group_by(mission, date, sample_type) %>% 
  summarise(sample_thickness_cm_average = mean(sample_thickness_cm_average)) %>% 
  spread(sample_type, sample_thickness_cm_average) %>% 
  mutate(yday = lubridate::yday(date))

mylabels <- c(
  "ice_camp_2015" = "Ice camp 2015",
  "ice_camp_2016" = "Ice camp 2016"
)

p <- df %>%
  ggplot(aes(x = yday)) +
  geom_ribbon(aes(ymin = 0, ymax = snow, fill = "Snow")) +
  geom_ribbon(aes(ymin = -ice, ymax = 0, fill = "Ice")) +
  facet_wrap(~mission, ncol = 1, labeller = labeller(mission = mylabels)) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    limits = c(110, 190),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
  ylab("Thickness (cm)") +
  xlab(NULL) +
  scale_fill_manual(breaks = c("Snow", "Ice"), values = c("Snow" = "#474444", "Ice" = "#5d80b6")) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25)

ggsave("graphs/fig02.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)

# Stats for the paper -----------------------------------------------------

df %>% 
  # gather(layer, thickness, ice, snow) %>% 
  # select_if(is.numeric) %>% 
  group_by(mission) %>% 
  skimr::skim()

