# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Primary production in both water column and in ice.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df_2015 <- read_excel("~/Desktop/données production primaire P. Massicote.xlsx", range = "A5:C25", col_names = c("yday", "water", "ice")) %>% 
  mutate(year = 2015)

df_2016 <- read_excel("~/Desktop/données production primaire P. Massicote.xlsx", range = "E5:G27", col_names = c("yday", "water", "ice")) %>% 
  mutate(year = 2016)

df <- bind_rows(df_2015, df_2016) %>%
  pivot_longer(matches("water|ice"), names_to = "type", values_to = "pp") %>% 
  mutate(date = as.Date(paste(year, yday), format = "%Y%j"))

mylabels <- c(
  "2015" = "Ice camp 2015",
  "2016" = "Ice camp 2016"
)

df %>% 
  drop_na(pp) %>% 
  ggplot(aes(x = yday, y = pp, color = factor(type))) +
  geom_line(size = 0.25) +
  geom_point(show.legend = FALSE, size = 0.5) +
  facet_wrap( ~ year, ncol = 1, labeller = labeller(year = mylabels)) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  xlab(NULL) +
  ylab(bquote("Primary production ("*mmoles~C~m^{-2}~d^{-1}*")")) +
  theme(plot.subtitle = element_text(size = 8)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    limits = c(118, 190),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
  scale_color_brewer(palette = "Set2", breaks = c("ice", "water"), labels = c("Ice", "Water")) +
  theme(legend.justification = c(1, 1), legend.position = c(0.99, 0.99)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 0.5)))

ggsave("graphs/fig10.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)
