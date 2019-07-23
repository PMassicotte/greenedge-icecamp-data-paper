# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Tidal survey data (E. Rhem).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

interpolate <- function(data, var) {
  
  res <- data %>%
    drop_na({{var}}) %>% 
    select({{var}}) %>% 
    mba.surf(200, 200, extend = TRUE)
  
  res2 <- expand.grid(date_time = res$xyz.est$x, depth_m = res$xyz.est$y) %>%
    mutate(interpolated = as.vector(res$xyz.est$z))
  
  return(res2)
}

# mat data -------------------------------------------------------------

mat <- rmatio::read.mat("/media/data4tb/greenedge/DATA/not-on-cyber-yet/tida-survey/SemiDiurnal2015_Pcoord.mat")

df <- tibble(
  date_time = as.vector(mat$tq),
  depth_m = as.vector(mat$pq),
  cdom = as.vector(mat$cdomg),
  temperature = as.vector(mat$Tg),
  density = as.vector(mat$s0g2)
) %>% 
  mutate(date_time = as.POSIXct((date_time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")) 

# res <- df %>% 
#   interpolate(c(date_time, depth_m, temperature)) %>% 
#   mutate(date_time = as.POSIXct((date_time - 719529) * 86400, origin = "1970-01-01", tz = "UTC"))

df

color <- mat$cmap
color <- rgb(color[,1], color[,2], color[,3], maxColorValue = 1)

isoline <- tibble(
  depth_m = as.vector(mat$pq),
  date_time = as.vector(mat$tq),
  density = as.vector(mat$s0g2)
) %>%
  mutate(date_time = as.POSIXct((date_time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")) %>%
  drop_na() %>% 
  filter(between(density, 26.1, 27.1))

p1 <- df %>%
  drop_na(temperature) %>%
  ggplot(aes(
    x = date_time,
    y = depth_m,
    fill = temperature,
    z = temperature
  )) +
  geom_isobands(binwidth = 0.1, color = NA) +
  scale_y_reverse(expand = c(0, 0), limits = c(NA, 0)) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color) +
  # scale_fill_viridis_c(breaks = seq(-10, 10, by = 0.4), option = "C", begin = 0.5) +
  xlab(NULL) +
  ylab("Pressure (dbar)") +
  labs(fill = "Temperature (°C)") +
  geom_contour(
    data = isoline,
    aes(x = date_time, y = depth_m, z = density),
    color = "black",
    binwidth = 0.1,
    inherit.aes = FALSE,
    size = 0.25
  ) +
  metR::geom_text_contour(
    data = isoline,
    aes(x = date_time, y = depth_m, z = density),
    inherit.aes = FALSE,
    binwidth = 0.1,
    stroke = 0.2,
    check_overlap = TRUE,
    family = "Poppins",
    size = 3
  ) +
  theme(legend.position = "none") 

p2 <- df %>%
  drop_na(cdom) %>% 
  ggplot(aes(x = date_time, y = depth_m, fill = cdom, z = cdom)) +
  geom_isobands(binwidth = 0.1, color = NA) +
  scale_y_reverse(expand = c(0, 0), limits = c(NA, 0)) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, limits = c(6, 11), oob = scales::squish) +
  xlab(NULL) +
  ylab("Pressure (dbar)") +
  labs(fill = "CDOM\nfluorescence (ppb)") +
  geom_contour(
    data = isoline,
    aes(x = date_time, y = depth_m, z = density),
    color = "black",
    binwidth = 0.1,
    inherit.aes = FALSE,
    size = 0.25
  ) +
  metR::geom_text_contour(
    data = isoline,
    aes(x = date_time, y = depth_m, z = density),
    inherit.aes = FALSE,
    binwidth = 0.1,
    stroke = 0.2,
    check_overlap = TRUE,
    family = "Poppins",
    size = 3
  ) +
  theme(legend.position = "none")

# s0coord data ------------------------------------------------------------

mat <- rmatio::read.mat("/media/data4tb/greenedge/DATA/not-on-cyber-yet/tida-survey/SemiDiurnal2015_S0coord.mat")

df <- tibble(
  date_time = as.vector(mat$tq),
  depth_m = as.vector(mat$s0q),
  cdom = as.vector(mat$cdomg),
  temperature = as.vector(mat$Tg)
) %>%
  mutate(date_time = as.POSIXct((date_time - 719529) * 86400, origin = "1970-01-01", tz = "UTC"))

color <- mat$cmap
color <- rgb(color[,1], color[,2], color[,3], maxColorValue = 1)

p3 <- df %>%
  drop_na(temperature) %>% 
  ggplot(aes(x = date_time, y = depth_m, fill = temperature, z = temperature)) +
  geom_isobands(binwidth = 0.1, color = NA) +
  scale_y_reverse(expand = c(0, 0), breaks = seq(26.1, 27.1, by = 0.1))  +
  scale_x_datetime(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color) +
  xlab(NULL) +
  ylab(bquote(sigma[theta]~(kg~m^{-3}))) +
  labs(fill = "Temperature (°C)") +
  geom_hline(yintercept = seq(26.1, 27.1, by = 0.1), size = 0.25) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_colorbar(
    direction = "horizontal",
    title.position = "top",
    barwidth = 10
  ))

p4 <- df %>%
  drop_na(cdom) %>%
  ggplot(aes(x = date_time, y = depth_m, fill = cdom, z = cdom)) +
  geom_isobands(binwidth = 0.1, color = NA) +
  scale_y_reverse(expand = c(0, 0), breaks = seq(26.1, 27.1, by = 0.1)) +
  scale_x_datetime(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, limits = c(6, 11), oob = scales::squish, guide = guide_legend(
    direction = "horizontal",
    title.position = "top"
  )) +
  xlab(NULL) +
  ylab(bquote(sigma[theta]~(kg~m^{-3}))) +
  labs(fill = "CDOM fluorescence (ppb)") +
  geom_hline(yintercept = seq(26.1, 27.1, by = 0.1), size = 0.25) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  guides(fill = guide_colorbar(
    direction = "horizontal",
    title.position = "top",
    barwidth = 10
  ))

# Combine plots -----------------------------------------------------------

p5 <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A")

# ggsave("~/Desktop/figxxx.pdf", width = 16, height = 18, units = "cm", device = cairo_pdf)
ggsave("graphs/fig04.png", width = 16, height = 18, units = "cm", dpi = 300)
  
tide <- tibble(
  hour = 0:23,
  height = c(0.8, 0.7, 0.7, 0.7, 0.8, 0.9, 1.0, 1.1, 1.1, 1.0, 0.9, 0.7, 0.5, 0.3, 0.2, 0.3, 0.3, 0.5, 0.7, 0.9, 1.0, 1.1, 1.1, 1.0)
)

tide %>% 
  filter(between(hour, 8, 21)) %>% 
  ggplot(aes(x = hour, y = height)) +
  geom_line() +
  geom_point()
