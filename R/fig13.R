# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Physiology of ice algae using PAM data (Johann).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(data.table)


# Plot PAM ----------------------------------------------------------------

df <- readxl::read_excel("/media/data4tb/greenedge/DATA/not-on-cyber-yet/GE16_PAMcompil_29Oct.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(sample_type %in% c("water", "ice")) %>% 
  mutate_at(vars(starts_with("fv")), parse_number) %>% 
  mutate(depth_m = paste0(depth_m, " m")) %>% 
  mutate(depth_m = case_when(
    depth_m == "NA m" ~ "Ice algae",
    TRUE ~ depth_m
  )) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(depth_m %in% c("Ice algae", "1.5 m", "10 m", "40 m", "60 m")) %>% 
  mutate(depth_m = fct_relevel(depth_m, "Ice algae", "1.5 m", "10 m", "40 m", "60 m"))

p1 <- df %>%
  ggplot(aes(x = date, y = fv_fm_avg, color = depth_m)) +
  annotate(
    "rect",
    xmin = min(df$date) - 5,
    xmax = max(df$date) + 5,
    ymin = 0.50,
    ymax = 0.75,
    fill = "gray85",
    alpha = 0.5
  ) +
  geom_line(size = 0.25) +
  geom_pointrange(aes(ymin = fv_fm_avg - fv_fm_sd, ymax = fv_fm_avg + fv_fm_sd),
                  show.legend = FALSE,
                  size = 0.1
  ) +
  xlab(NULL) +
  scale_x_date(
    breaks = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "3 weeks"),
    date_labels = "%b %d",
    limits = c(min(df$date) - 5, max(df$date) + 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  ylab(bquote(F[v] * "/" * F[m])) +
  guides(color = guide_legend(override.aes = list(size = 0.5), nrow = 2)) +
  scale_color_manual(values = pals::brewer.set1(n = length(unique(df$depth_m)))) +
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c(1.01, -0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.25, "cm"),
    legend.direction = "horizontal"
  ) 

# df <- readxl::read_excel(
#   "data/raw/Fv-Fm ice camp Green Edge 2016_Pour Philippe.xlsx",
#   sheet = "Total",
#   skip = 2,
#   .name_repair = "minimal"
# ) %>%
#   janitor::clean_names()
# 
# list_of_dfs <- split.default(df, rep(1:4, c(3, 5, 3, 3)))
# list_of_dfs[[5]] <- list_of_dfs[[2]][, c(1, 4, 5)]
# list_of_dfs[[2]] <- list_of_dfs[[2]][, 1:3]
# 
# df <- list_of_dfs %>%
#   map(~ .[complete.cases(.), ]) %>%
#   map(~ mutate(., type = grep("^sea|^water", names(.), value = TRUE))) %>%
#   map(setNames, nm = c("julian", "measure", "sd", "type")) %>%
#   bind_rows() %>%
#   mutate(type2 = case_when(
#     str_detect(type, "sea") ~ "Ice algae",
#     type == "water_1_5_m_depth" ~ "1.5 m",
#     type == "water_10_m_depth" ~ "10 m",
#     type == "water_40_m_depth" ~ "40 m",
#     type == "water_60_m_depth" ~ "60 m"
#   )) %>%
#   mutate(type2 = fct_relevel(type2, "Ice algae", "1.5 m", "10 m", "40 m", "60 m")) %>%
#   mutate(date = as.Date(paste0("2016-", julian), "%Y-%j"))

# Remove 60 m data because we do not have a lot of observation at that depth

# df <- df %>%
#   filter(str_detect(type, "60", negate = TRUE))

# p1 <- df %>%
#   ggplot(aes(x = date, y = measure, color = type2)) +
#   annotate(
#     "rect",
#     xmin = min(df$date) - 5,
#     xmax = max(df$date) + 5,
#     ymin = 0.50,
#     ymax = 0.75,
#     fill = "gray85",
#     alpha = 0.5
#   ) +
#   geom_line(size = 0.25) +
#   geom_pointrange(aes(ymin = measure - sd, ymax = measure + sd),
#     show.legend = FALSE,
#     size = 0.1
#   ) +
#   xlab(NULL) +
#   scale_x_date(
#     breaks = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "3 weeks"),
#     date_labels = "%b %d",
#     limits = c(min(df$date) - 5, max(df$date) + 5),
#     expand = c(0, 0)
#   ) +
#   scale_y_continuous(limits = c(0, NA)) +
#   ylab(bquote(F[v] * "/" * F[m])) +
#   guides(color = guide_legend(override.aes = list(size = 0.5), nrow = 2)) +
#   scale_color_manual(values = pals::brewer.set1(n = length(unique(df$type2)))) +
#   theme(
#     legend.position = c(1, 0.01),
#     legend.justification = c(1.01, -0.01),
#     legend.title = element_blank(),
#     legend.text = element_text(size = 6),
#     legend.key.size = unit(0.25, "cm"),
#     legend.direction = "horizontal"
#   ) 

# Plot PvsE ---------------------------------------------------------------

pvse <- read_csv("/media/data4tb/greenedge/clean/pvse/greenedge_pvse.csv")

pvse <- pvse %>%
  filter(mission == "ice_camp_2016") %>%
  filter(sample_type == "water") %>%
  filter(depth_m != "interface") %>%
  mutate(depth_m = parse_number(depth_m))

# Clear outlier
pvse$ek[pvse$depth_m == 1.5][1] <- NA

p2 <- pvse %>%
  drop_na(ek) %>%
  filter(depth_m %in% c(1.5, 5, 10)) %>%
  mutate(depth_m = paste(depth_m, "m")) %>%
  mutate(depth_m = fct_relevel(depth_m, c("1.5 m", "5 m", "10 m"))) %>%
  ggplot(aes(x = date, y = ek, color = depth_m)) +
  geom_point(size = 1, show.legend = FALSE) +
  geom_line(size = 0.25) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  ylab(bquote(E[k] ~ (mu * mol ~ m^{
    -2
  } ~ s^{
    -1
  }))) +
  xlab(NULL) +
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c(1.01, -0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.25, "cm"),
    legend.direction = "horizontal"
  ) +
  guides(color = guide_legend(override.aes = list(size = 0.5), nrow = 1)) +
  scale_color_manual(values = c(
    "1.5 m" = pals::brewer.set1(n = 10)[8],
    "5 m" = pals::brewer.set1(n = 10)[9],
    "10 m" = pals::brewer.set1(n = 10)[3]
  ))

# Save --------------------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A"
  )

ggsave("graphs/fig13.pdf", width = 8, height = 12, units = "cm", device = cairo_pdf)

# Stats -------------------------------------------------------------------

range(pvse$date)

pvse %>%
  filter(depth_m %in% c(1.5, 5, 10)) %>%
  select(depth_m, ek) %>%
  group_by(depth_m) %>%
  summarise(min(ek, na.rm = TRUE), max(ek, na.rm = TRUE))

pvse %>%
  # filter(depth_m %in% c(1.5, 5, 10)) %>%
  summarise(mean(ek, na.rm = TRUE), sd(ek, na.rm = TRUE), n = n())