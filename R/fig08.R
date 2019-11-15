# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Temporal evolution of NO3.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(ggisoband)

rm(list = ls())

df <- data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/nutrients/greenedge_nutrients.csv") %>%
  filter(str_detect(mission, "ice_camp")) %>%
  drop_na(no3_um_l) %>%
  filter(sample_source == "niskin") %>%
  filter(filter_type == "gff") %>%
  mutate(date = as.Date(date)) %>%
  group_by(mission, depth_m, date) %>%
  summarise(no3_um_l = mean(no3_um_l), n = n()) %>%
  filter(depth_m <= 60) %>%
  filter(no3_um_l != max(no3_um_l)) # There is 1 outlier in 2016, remove it

hist(df$no3_um_l)

df <- df %>%
  group_by(mission) %>%
  nest() %>%
  mutate(interpolated_no3 = map(data, function(df) {
    res <- df %>%
      mutate(yday = lubridate::yday(date)) %>%
      dplyr::select(yday, depth_m, no3_um_l) %>%
      # mutate(date = as.numeric(date, origin = "1970-01-01", tz = "UTC")) %>%
      mba.surf(500, 500, extend = TRUE)

    res2 <- expand.grid(yday = res$xyz.est$x, depth_m = res$xyz.est$y) %>%
      mutate(no3_um_l = as.vector(res$xyz.est$z))

    return(res2)
  }))

df <- df %>%
  unnest(interpolated_no3) %>%
  drop_na(no3_um_l) %>%
  mutate(no3_um_l = ifelse(no3_um_l < 0, 0, no3_um_l)) %>%
  mutate(bin = cut(
    no3_um_l,
    seq(min(no3_um_l), max(no3_um_l) + 1, by = 0.5),
    include.lowest = TRUE,
    right = TRUE
  ))

# %>%
#   mutate(date = as.Date(date, origin = "1970-01-01", tz = "UTC"))

# Plot --------------------------------------------------------------------

mylabels <- c(
  "ice_camp_2015" = "Ice camp 2015",
  "ice_camp_2016" = "Ice camp 2016"
)

p <- df %>%
  ggplot(aes(x = yday, y = depth_m, fill = no3_um_l, z = no3_um_l)) +
  geom_tile() +
  scale_fill_viridis_c(
    guide = guide_colorbar(barwidth = unit(0.25, "cm"), barheight = unit(4, "cm"))
  ) +
  geom_isobands(color = NA, breaks = seq(0, 8, by = 0.5)) +
  scale_y_reverse(expand = c(0, 0)) +
  # scale_fill_manual(values = colorRampPalette(viridis::viridis(16))(16)) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    expand = c(0, 0),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
  facet_wrap(~mission, ncol = 1, labeller = labeller(mission = mylabels)) +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  ) +
  labs(fill = "Nitrate\n(\u03BCmol L\u207b\u00b9)") +
  # labs(fill = bquote(atop(NO[3]^"-", (mu * mol ~ L^{-1})))) +
  xlab(NULL) +
  ylab("Depth (m)")

ggsave("graphs/fig08.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)
