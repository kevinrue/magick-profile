jitter_value <- 0
library(dbscan)
source("magick-profile/global.R")
set.seed(1)
img_raw <- image_read(path = "~/Downloads/radcliffe-camera.jpg")
str(img_raw)
img_matrix <- make_matrix(img_raw)
dim(img_matrix)
img_matrix[1:4, 1:4]
gg <- make_jitterplot(img_matrix, downsample = 125, point_size = 1, jitter = jitter_value)
gg_data <- gg$data
head(gg_data)
gg_data <- subset(gg_data, col < 550)
gg_data$knndist <- kNNdist(as.matrix(gg_data), k = 7)
# gg_data$cluster <- cut(gg_data$knndist, seq(min(gg_data$knndist), max(gg_data$knndist), length.out = 10))
gg_data$cluster <- cut(gg_data$knndist, unique(quantile(gg_data$knndist, probs = seq(0, 1, length.out = 12))))
ggplot(gg_data, aes(row, col, colour = cluster)) +
  # geom_jitter(size = 2, width = jitter_value, height = jitter_value) +
  geom_point()+
  theme_void() +
  guides(colour = "none") +
  # scale_colour_brewer(palette = "Paired") +
  scale_colour_brewer(palette = "Paired")

# RColorBrewer::display.brewer.all()
