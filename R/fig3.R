library(rmatio)
library(ggisoband)

rm(list = ls())

read_sal <- function(file) {
  
  mat <- read.mat(file)
  
  df <- expand.grid(
    depth = mat$yyi[, 1],
    yday = mat$xxi[1, ]
  ) %>% 
    mutate(date = as.POSIXct((yday - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>% 
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
    direction = 1
  ) +
  geom_isobands(breaks = seq(30, 45, by = 0.1), color = NA) +
  facet_wrap(~year, labeller = labeller(year = mylabels), ncol = 1) +
  xlab(NULL) +
  ylab("Depth (m)") +
  labs(fill = "Salinity\n(g/kg)") +
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) +
  theme(legend.key.size = unit(0.25, "cm")) +

ggsave("graphs/fig3.pdf", width = 8, height = 10, units = "cm", device = cairo_pdf)