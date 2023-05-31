file <- "~/../Downloads/anna-magick/Screenshot 2023-05-31 204843.png"
downsample <- 300
k_centers <- 3
point_size <- 1
jitter <- 1
axis_label_prefix <- "TSNE"

library(magick)

# import iamge ----

imageObject <- magick::image_read(path = file)
# imageObject

# make matrix ----

# Modulate, Threshold
flat <- image_flatten(imageObject, 'Threshold')
# flat
data <- image_data(flat)
# data
imageMatrix <- data[1, ,] != "ff"
storage.mode(imageMatrix) <- "integer"
# imageMatrix
# dim(imageMatrix)

# heatmap(imageMatrix, Rowv = NA, Colv = NA)

# make jitter plot ----

matrix <- imageMatrix

# Get matrix size to preserve original aspect ratio
x_range <- c(1, ncol(matrix))
y_range <- c(1, nrow(matrix))
# Turn binary matrix into point coordinates
xy_coord <- as.data.frame(which(matrix == 1, arr.ind = TRUE))
# Flip the Y axis
xy_coord$col <- -xy_coord$col
if (!isFALSE(downsample)) {
  keep <- iSEE::subsetPointsByGrid(
    X = xy_coord$row,
    Y = xy_coord$col,
    resolution = downsample
  )
  xy_coord <- xy_coord[keep,]
}
# Identify clusters
if (k_centers > 1) {
  xy_coord$cluster <- as.factor(kmeans(x = as.matrix(xy_coord[, c("row", "col")]), centers = k_centers)$cluster)
}
# Make coordinates look centered
x_center <- x_range[2] / 2
y_center <- -y_range[2] / 2
xy_coord$row <- xy_coord$row - x_center
xy_coord$col <- xy_coord$col - y_center
x_range <- x_range - x_center
y_range <- y_range + y_center # we work in negative values here
# Plot
gg <- ggplot(xy_coord)
if (k_centers > 1) {
  gg <- gg + geom_jitter(aes(row, col, color=cluster),
                         size = point_size,
                         width = jitter,
                         height = jitter)
}  else {
  gg <- gg + geom_jitter(aes(row, col),
                         color="black",
                         size = point_size,
                         width = jitter,
                         height = jitter)
}
gg <- gg +
  coord_cartesian(xlim = x_range, ylim = y_range) +
  labs(x = paste(axis_label_prefix, 1L),
       y = paste(axis_label_prefix, 2L)) +
  guides(color="none") +
  theme(panel.background = element_blank())
gg
