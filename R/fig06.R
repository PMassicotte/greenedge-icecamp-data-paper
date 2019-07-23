files <- c("data/raw/AverageCosine20Juny2016.xlsx", "data/raw/AverageCosine16May2015.xlsx", "data/raw/AverageCosine4July2016.xlsx")

df <- map_df(files, function(file) {
  sheets <- excel_sheets(file)
  
  map2(file, sheets, ~read_excel(.x, .y, col_names = .y)) %>% 
    reduce(bind_cols) %>% 
    janitor::clean_names() %>% 
    rename_at(vars(starts_with("pep")), ~"planar-irradiance_par") %>% 
    rename_at(vars(starts_with("pes")), ~"scalar-irradiance_par") %>% 
    rename_at(vars(starts_with("pac")), ~"average-cosine_par") %>% 
    rename_at(vars(starts_with("ep")), ~str_replace(., ".*(_\\d{3}$)", "planar-irradiance\\1")) %>% 
    rename_at(vars(starts_with("es")), ~str_replace(., ".*(_\\d{3}$)", "scalar-irradiance\\1")) %>% 
    rename_at(vars(starts_with("ac")), ~str_replace(., ".*(_\\d{3}$)", "average-cosine\\1")) %>% 
    rename_at(vars(starts_with("depth")), ~"depth_m") %>% 
    mutate(filename = basename(file))
  
})

df <- df %>% 
  pivot_longer(cols = -c("depth_m", "filename")) %>% 
  separate(name, into = c("variable", "wave_range"), sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  janitor::clean_names()

mylabel <- c(
  "AverageCosine20Juny2016.xlsx" = "2016-06-20",
  "AverageCosine16May2015.xlsx" = "2015-05-16",
  "AverageCosine4July2016.xlsx" = "2016-07-04"
)

p1 <- df %>% 
  ggplot(aes(y = depth_m, color = wave_range)) +
  geom_path(aes(x = planar_irradiance, linetype = "Planar irradiance"), size = 0.25) +
  geom_path(aes(x = scalar_irradiance, linetype = "Scalar irradiance"), size = 0.25) +
  scale_y_reverse(limits = c(NA, 0)) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  facet_wrap(~filename, scales = "free_x", labeller = labeller(filename = mylabel), ncol = 1) +
  xlab("Downwelling irradiance") +
  scale_color_manual(
    breaks = c("442", "532", "par"),
    values = pals::brewer.dark2(3),
    labels = c(bquote("442 nm"~(mW~m^{-2}~nm^{-1})), bquote("532 nm"~(mW~m^{-2}~nm^{-1})), bquote("PAR"~(mu*mol~m^{-2}~s^{-1})))
  ) +
  theme(legend.title = element_blank()) +
  ylab("Depth (m)") +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.3, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 0.5), nrow = 2)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5), nrow = 2)) +
  theme(
    legend.justification = "right",
    # legend.margin = margin(0, 0, 0, 10),
    legend.box.margin = margin(-10, -10, -10, 200)
  )

equal_breaks <- function(n = 3, s = 0.05, ...) {
  function(x) {
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n)
  }
}

p2 <- df %>% 
  ggplot(aes(y = depth_m, color = wave_range)) +
  geom_path(aes(x = average_cosine), size = 0.25) +
  # geom_path(aes(x = scalar_irradiance, linetype = "Scalar irradiance")) +
  scale_y_reverse(limits = c(NA, 0)) +
  scale_x_continuous(labels = function(x) format(round(x, digits = 2), nsmall = 2), expand = c(0.1, 0), breaks = equal_breaks(n = 4, s = 0.05)) +
  # scale_x_log10() +
  # annotation_logticks(sides = "b") +
  facet_wrap(~filename, labeller = labeller(filename = mylabel), ncol = 1, scales = "free_x") +
  xlab("Downwelling average cosine") +
  scale_color_manual(
    breaks = c("442", "532", "par"),
    values = pals::brewer.dark2(3),
    labels = c(bquote("442 nm"~(mW~m^{-2}~nm^{-1})), bquote("532 nm"~(mW~m^{-2}~nm^{-1})), bquote("PAR"~(mu*mol~m^{-2}~s^{-1})))
  ) +
  theme(legend.position = "none") +
  ylab("Depth (m)") 

p1 +
  p2 +
  patchwork::plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "A"
  )

ggsave("graphs/fig06.pdf", width = 16, height = 18, units = "cm", device = cairo_pdf)
