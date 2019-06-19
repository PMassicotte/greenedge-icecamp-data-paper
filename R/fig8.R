# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Fig presenting taxonomy from the IFCB.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# (i <- image_read("https://plankton.mio.osupytheas.fr/wp-content/uploads/2018/11/Attheya-septentrionalis_07_1.jpg", density = 600) %>%
#   # image_scale(geometry = "100x") %>%
#   # image_crop(geometry_area(0, 500)) %>%
#   image_annotate("Attheya septentrionalis", color = "white", size = 40, font = "Open Sans", location = "+10+0", boxcolor = "gray") %>%
#   image_annotate("With curled setea in between cells", color = "white", size = 24, font = "Open Sans", location = "+10+60", boxcolor = "gray") %>%
#   image_border(color = "white")
# )


files <- fs::dir_ls("data/raw/images_ifcb/", recursive = TRUE, regexp = ".png")

set.seed(589)

df <- split(files, dirname(files)) %>%
  map(.f = function(x) {
    i <- sample(1:length(x), 1)
    x[[i]]
  }) %>%
  enframe(value = "path") %>%
  unnest(path) %>%
    mutate(i = map(path, image_read, density = 600)) %>%
  mutate(i = map(i, ~image_border(.x, color = "white", geometry = "2x2"))) %>% 
  mutate(info = map(i, image_info)) %>%
  unnest(info) %>%
  filter(height >= 150) %>% 
  mutate(i = map(i, ~image_scale(.x, geometry = "150x"))) %>% 
  mutate(i = map2(i, basename(name), image_annotate, color = "white", size = 8, font = "Open Sans", location = "+10+5", boxcolor = "gray")) %>%
  filter(!str_detect(name, "temporary"))

a <- image_append(c(df$i[[1]], df$i[[2]], df$i[[3]]), stack = TRUE)
b <- image_append(c(df$i[[4]], df$i[[5]], df$i[[6]]), stack = TRUE)
c <- image_append(c(df$i[[7]], df$i[[8]], df$i[[9]], df$i[[13]]), stack = TRUE)
d <- image_append(c(df$i[[10]], df$i[[11]], df$i[[12]]), stack = TRUE)

ii <- image_append(c(a, b, c))

image_write(ii, "graphs/fig8.png")
