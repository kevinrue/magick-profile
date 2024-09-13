library(dbscan)
source("magick-profile/global.R")
set.seed(1)
img_raw <- image_read(path = "obds/oxford-city-skyline_am01184.jpg")
str(img_raw)
compose_types()
img_flat <- image_flatten(img_raw, "Threshold")
img_data <- image_data(img_flat)
img_matrix <- img_data[1, , ] < 245
storage.mode(img_matrix) <- "integer"
# make_heatmap(img_matrix)
# img_matrix <- make_matrix(img_raw, operator = )
# img_matrix <- 1-img_matrix
# dim(img_matrix)
# img_matrix[1:4, 1:4]
# make_heatmap(img_matrix)
gg <- make_jitterplot(img_matrix, downsample = 150, point_size = 1, jitter = 5)
gg_data <- gg$data
head(gg_data)
# gg_data <- subset(gg_data, col < 550)
# gg_data <- subset(gg_data, row > -250 & row < 460)
gg_data$knndist <- kNNdist(as.matrix(gg_data), k = 10)
# gg_data$cluster <- cut(gg_data$knndist, seq(min(gg_data$knndist), max(gg_data$knndist), length.out = 10))
gg_data$cluster <- cut(gg_data$knndist, unique(quantile(gg_data$knndist, probs = seq(0, 1, length.out = 8))))

gg_data$dbscan <- as.factor(dbscan(as.matrix(gg_data[, 1:2]), eps = 5, minPts = 5)$cluster)

# ggplot(gg_data, aes(row, col, colour = dbscan)) +
#   geom_jitter(size = 0.5, width = 3, height = 3)

gg_data <- subset(gg_data, dbscan == 1 & !is.na(cluster))

raw_colors <- RColorBrewer::brewer.pal(nlevels(gg_data$cluster), name = "Paired")
muted_colors <- scales::muted(raw_colors, l = 50, c = 70)
# plot(
#   x = c(seq_along(my_colors), seq_along(my_colors)),
#   y = c(rep(1, length(my_colors)), rep(2, length(my_colors))),
#   col = c(my_colors, muted_colors),
#   pch=20, cex=5)

gg_data$cluster_muted <- as.factor(paste0(ifelse(gg_data$col < 50, "muted_", "raw_"), gg_data$cluster))
levels(gg_data$cluster_muted)

color_scale <- c(muted_colors, my_colors)
names(color_scale) <- levels(gg_data$cluster_muted)

ggplot(gg_data, aes(row, col, colour = cluster_muted)) +
  geom_jitter(size = 0.5, width = 3, height = 3) +
  scale_colour_manual(values = color_scale) +
  theme_void() +
  guides(colour = "none")
  # scale_colour_brewer(palette = "Paired")
# RColorBrewer::display.brewer.all()

