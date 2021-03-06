library(rmatio)
library(ggisoband) # devtools::install_github("clauswilke/ggisoband")

rm(list = ls())

read_sal <- function(file) {
  mat <- read.mat(file)

  df <- expand.grid(
    depth = mat$yyi[, 1],
    yday = mat$xxi[1, ]
  ) %>%
    mutate(date = as.POSIXct((yday - 719529) * 86400, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(yday = lubridate::yday(date)) %>%
    mutate(sal = as.vector(mat$SAL_INTERP)) %>%
    filter(depth <= 100) %>%
    mutate(year = lubridate::year(date))
}

files <- list(
  "data/raw/GE2015_CTD_INTERPv2.mat",
  "data/raw/GE2016_CTD_INTERPv2.mat"
)

df <- map_df(files, read_sal)

mylabels <- c(
  "2015" = "Ice camp 2015",
  "2016" = "Ice camp 2016"
)

# Check min and max
data.table::fread("/mnt/nfs/scratch/mariepieramyot/backup/ctd/greenedge_ctd.csv") %>%
  filter(str_detect(mission, "ice_camp")) %>%
  drop_na(psal_psu) %>%
  mutate(date = as.Date(date)) %>%
  group_by(mission, pres_decibars, date) %>%
  summarise(asal_g_kg = mean(asal_g_kg), n = n()) %>%
  # filter(pres_decibars <= 100) %>%
  group_by(mission) %>%
  filter(asal_g_kg == min(asal_g_kg) | asal_g_kg == max(asal_g_kg))


# Plot --------------------------------------------------------------------

df %>%
  drop_na() %>%
  ggplot(aes(
    x = yday,
    y = depth,
    fill = sal,
    z = sal
  )) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    expand = c(0, 0),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
  scale_fill_viridis_c(
    limits = c(31.5, 33),
    oob = scales::squish,
    breaks = seq(31.5, 33, by = 0.5),
    direction = 1,
    guide = guide_colorbar(barwidth = unit(0.25, "cm"), barheight = unit(4, "cm"))
  ) +
  geom_isobands(breaks = seq(30, 45, by = 0.1), color = NA) +
  facet_wrap(~year, labeller = labeller(year = mylabels), ncol = 1) +
  xlab(NULL) +
  ylab("Depth (m)") +
  labs(fill = "Absolute\nsalinity\n(g kg\u207b\u00b9)") +
  # labs(fill = bquote(atop("Asbolute\nsalinity", (g ~ kg^{-1})))) +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )

ggsave("graphs/fig03.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)
