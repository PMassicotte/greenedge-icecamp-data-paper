# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figure on flow cytometry. Data from D. Vaulot.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

ge_fcm <-
  readxl::read_excel("data/raw/GE_all_Cytometry version 1.6.xlsx", sheet = "All data") %>%
  mutate(
    depth_m_char = as.character(depth_m),
    date = as.Date(date)
  ) %>% 
  janitor::clean_names()

# Taking the Ice Camp data only
ge_fcm_ic <- filter(
  ge_fcm,
  (operation == "Ice camp")
  & (sample_type == "water")
  & (depth_level != "underice")
  & !(depth_m %in% c(0, 8, 14, 18, 65, 100, 200, 350))
) %>% 
  drop_na(bact_m_l)

ge_fcm_ic <- ge_fcm_ic %>% 
  group_by(year = lubridate::year(date)) %>%
  nest() %>%
  mutate(interpolated_bact_m_l = map(data, function(df) {
    res <- df %>%
      mutate(yday = lubridate::yday(date)) %>%
      dplyr::select(yday, depth_m, bact_m_l) %>%
      # mutate(date = as.numeric(date, origin = "1970-01-01", tz = "UTC")) %>%
      mba.surf(500, 500, extend = TRUE, h = 5.5)
    
    res2 <- expand.grid(yday = res$xyz.est$x, depth_m = res$xyz.est$y) %>%
      mutate(bact_m_l = as.vector(res$xyz.est$z))
    
    return(res2)
  }))

# Plot --------------------------------------------------------------------

mylabels <- c(
  "2015" = "Ice camp 2015",
  "2016" = "Ice camp 2016"
)

scientific_10x <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}

p <- ge_fcm_ic %>% 
  unnest(interpolated_bact_m_l) %>% 
  ggplot(aes(x = yday, y = depth_m, fill = bact_m_l, z = bact_m_l)) +
  geom_raster() +
  # geom_isobands(color = NA, breaks = seq(1e5, 12e5, by = 1e5 / 2)) +
  facet_wrap(~year, ncol = 1, labeller = labeller(year = mylabels)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month") %>% lubridate::yday(),
    expand = c(0, 0),
    labels = function(x) {
      as.Date(paste0("2015-", x), "%Y-%j") %>% format("%b")
    }
  ) +
scale_fill_viridis_c(
  option = "plasma",
  alpha = 1,
  trans = "log", 
  label = scientific_10x,
  breaks = c(0e5, 1e5, 2e5, 4e5, 8e5, 16e5),
  guide = guide_colorbar(barwidth = unit(0.25, "cm"), barheight = unit(4, "cm"))
) +
  theme(legend.text = element_text(size = 8, vjust = 1)) +
  theme(legend.title = element_text(size = 8)) +
  # theme(legend.key.size = unit(0.25, "cm")) +
  labs(fill = "Bacteria\nper mL") +
  xlab(NULL) +
  ylab("Depth (m)") 
# +
#   geom_point(data = ge_fcm_ic %>% unnest(data), aes(x = julian_day), size = 0.01, color = "white")
# +
#   geom_contour(breaks = seq(1e5, 8e5, by = 1e5 / 2), color = "black", size = 0.05)

ggsave("graphs/fig09.pdf", width = 8.3, height = 10, units = "cm", device = cairo_pdf)
