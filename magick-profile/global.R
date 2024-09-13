require(magick)
require(ComplexHeatmap)
require(ggplot2)
require(iSEE)

make_matrix <- function(object, operator = c("Modulate", "Threshold"), type = "numeric") {
  print(operator)
  operator <- match.arg(operator)
  flat <- image_flatten(object, operator)
  # flat
  data <- image_data(flat)
  # data
  non_white <- data[1, ,] != "ff"
  storage.mode(non_white) <- "integer"
  non_white
}

make_heatmap <- function(matrix) {
    if (is.null(matrix)) {
        return(plot.new())
    }
    hm <- Heatmap(
        matrix = t(matrix),
        col = c("1" = "black", "0" = "white"),
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        show_heatmap_legend = FALSE
    )
    draw(hm)
}

make_scatterplot <- function(matrix,
                             downsample = FALSE,
                             point_size = 1,
                             axis_label_prefix = "TSNE") {
    if (is.null(matrix)) {
        return(plot.new())
    }
    # Get matrix size to preserve original aspect ratio
    x_range <- c(1, ncol(matrix))
    y_range <- c(1, nrow(matrix))
    # Turn binary matrix into point coordinates
    xy_coord <- as.data.frame(which(matrix == 1, arr.ind = TRUE))
    # Flip the Y axis
    xy_coord$col <- -xy_coord$col
    if (!isFALSE(downsample)) {
        keep <- subsetPointsByGrid(
            X = xy_coord$row,
            Y = xy_coord$col,
            resolution = downsample
        )
        xy_coord <- xy_coord[keep,]
    }
    # Make coordinates look centered
    x_center <- x_range[2] / 2
    y_center <- -y_range[2] / 2
    xy_coord$row <- xy_coord$row - x_center
    xy_coord$col <- xy_coord$col - y_center
    x_range <- x_range - x_center
    y_range <- y_range + y_center # we work in negative values here
    # Plot
    ggplot(xy_coord) +
        geom_point(aes(row, col), size = point_size) +
        labs(x = paste(axis_label_prefix, 1L),
             y = paste(axis_label_prefix, 2L)) +
        coord_cartesian(xlim = x_range, ylim = y_range) +
        theme(panel.background = element_blank())
}

make_jitterplot <- function(matrix,
                            downsample = FALSE,
                            point_size = 1,
                            jitter = 1,
                            axis_label_prefix = "TSNE",
                            k_centers = 1) {
    if (is.null(matrix)) {
        return(plot.new())
    }
    # Get matrix size to preserve original aspect ratio
    x_range <- c(1, ncol(matrix))
    y_range <- c(1, nrow(matrix))
    # Turn binary matrix into point coordinates
    xy_coord <- as.data.frame(which(matrix == 1, arr.ind = TRUE))
    # Flip the Y axis
    xy_coord$col <- -xy_coord$col
    if (!isFALSE(downsample)) {
        keep <- subsetPointsByGrid(
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
}

make_original <- function(object) {
    if (is.null(object)) {
        return(plot.new())
    }
    image_draw(object)
}
