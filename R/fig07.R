# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Showing IOPs before and during the bloom.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("https://gist.githubusercontent.com/friendly/67a7df339aa999e2bcfcfec88311abfc/raw/761a7688fba3668a84b2dfe42a655a1b246ca193/wavelength_to_rgb.R")

iop <- feather::read_feather("../../green_edge/underice_kd/data/clean/iop.feather") %>% 
  filter(date %in% as.Date(c("2016-06-17", "2016-06-29"))) %>% 
  drop_na(mean_c)

# Plots -------------------------------------------------------------------

color <- lapply(unique(iop$wavelength), wavelength_to_rgb) %>% unlist()
color <- setNames(color, unique(iop$wavelength))

mylabels <- c(
  "2016-06-17" = "2016-06-17\nPre-bloom conditions",
  "2016-06-29" = "2016-06-29\nBloom conditions"
)

## Attenuation
p <- iop %>%
  ggplot(aes(x = mean_c, y = depth_grid, color = wavelength, group = wavelength)) +
  geom_path(size = 0.25) +
  facet_wrap(~date, labeller = labeller(date = mylabels), ncol = 1) +
  scale_y_reverse() +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  scale_colour_gradientn(
    colours = color,
    breaks = seq(400, 800, by = 50), limits = c(400, 750),
    guide = guide_colorbar(barwidth = unit(0.25, "cm"), barheight = unit(4, "cm"))
  ) +
  ylab("Depth (m)") +
  xlab(bquote("Beam attenuation" ~ (m^{-1}))) +
  labs(color = bquote(lambda*(nm)))

ggsave("graphs/fig07.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)

# Stats for the paper -----------------------------------------------------

# Do use the clean GE data, not the one in the kd project because, there I am
# not using all wavelengths
iop %>% 
  select(wavelength, mean_bbp) %>% 
  drop_na() %>% 
  distinct(wavelength) 
