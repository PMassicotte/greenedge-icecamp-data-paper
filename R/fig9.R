# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Physiology of ice algae using PAM data (Johann).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(data.table)

df <- readxl::read_excel(
  "data/raw/Fv-Fm ice camp Green Edge 2016_Pour Philippe.xlsx",
  sheet = "Total",
  skip = 2,
  .name_repair = "minimal"
) %>%
  janitor::clean_names()

list_of_dfs <- split.default(df, rep(1:4, c(3, 5, 3, 3)))
list_of_dfs[[5]] <- list_of_dfs[[2]][, c(1, 4, 5)]
list_of_dfs[[2]] <- list_of_dfs[[2]][, 1:3]

df <- list_of_dfs %>%
  map(~ .[complete.cases(.), ]) %>%
  map(~ mutate(., type = grep("^sea|^water", names(.), value = TRUE))) %>%
  map(setNames, nm = c("julian", "measure", "sd", "type")) %>%
  bind_rows() %>% 
  mutate(type2 = case_when(
    str_detect(type, "sea") ~ "Ice algae",
    type == "water_1_5_m_depth" ~ "1.5 m",
    type == "water_10_m_depth" ~ "10 m",
    type == "water_40_m_depth" ~ "40 m",
    type == "water_60_m_depth" ~ "60 m"
  ))

# Remove 60 m data because we do not have a lot of observation at that depth

df <- df %>% 
  filter(str_detect(type, "60", negate = TRUE))

df %>%
  ggplot(aes(x = julian, y = measure, color = type2)) +
  geom_line(size = 0.25) +
  geom_pointrange(aes(ymin = measure - sd, ymax = measure + sd), show.legend = FALSE, size = 0.1) +
  xlab(NULL) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "2 weeks") %>% lubridate::yday(),
    # limits = c(125, 190),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b-%d")
    }
  ) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, NA)) +
  ylab(bquote(F[v]*"/"*F[m])) +
  theme(legend.position = c(1, 0.01), legend.justification = c(1.01, -0.01)) +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.25, "cm")) +
  theme(legend.direction = "horizontal") +
  scale_color_brewer(palette = "Set2", breaks = c("Ice algae", "1.5 m", "10 m", "40 m", "60 m"))

ggsave("graphs/fig9.pdf", width = 8, height = 6, units = "cm", device = cairo_pdf)
