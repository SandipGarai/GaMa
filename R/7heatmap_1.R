#' Generate Heatmaps
#'
#' This function generates interactive and static heatmaps based on the average similarity matrix.
#'
#' @param average_percent_similarity A matrix containing average similarity values between SampleIDs.
#'   Rows and columns should be labeled with SampleIDs.
#' @param html_title Title for the interactive heatmap (HTML version).
#' @param tiff_title Title for the static heatmap (TIFF version).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param width_inch Width of the heatmap in inches.
#' @param height_inch Height of the heatmap in inches.
#' @param dpi Dots per inch for the static heatmap.
#'
#' @return A list containing the file names of the generated heatmaps.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile theme_bw scale_fill_gradient element_blank
#' @importFrom heatmaply heatmaply
#' @importFrom reshape2 melt
#' @examples
#' \dontrun{
#' # Example usage:
#' output_directory <- "output_directory/new"
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' heatmap_files <- generate_heatmaps(average_percent_similarity)
#'
#' # Save the interactive heatmap as an HTML file ####
#' html <- file.path(output_directory, "5. heatmap.html")
#' htmlwidgets should be installed and loaded
#' htmlwidgets::saveWidget(heatmap_files$html, file = html)
#'
#' # save the TIFFE images ####
#'
#' # heatmap_tiff_file1
#' tiff1 <- file.path(output_directory, "5. heatmap1.tiff")
#' tiff(tiff1, width = width_inch, height = height_inch, units = "in", res = dpi)
#' heatmap(as.matrix(average_percent_similarity), main = "Heatmap of Average Similarity")
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # heatmap_tiff_file2
#' tiff2 <- file.path(output_directory, "5. heatmap2.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff2, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(heatmap_files$tiff2)
#'
#' # Close the TIFF device
#' dev.off()
#'
#' # heatmap_tiff_file3
#' tiff3 <- file.path(output_directory, "5. heatmap3.tiff")
#'
#' # Open the TIFF device and create the heatmap.2 with hierarchical clustering dendrogram
#' # gplots should be installed and loaded
#' gplots::heatmap.2(as.matrix(average_percent_similarity),
#'           dendrogram = "row",
#'           Colv = "Rowv",
#'           scale = "row",
#'           main = "Heatmap of Average Similarity")
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @rdname I.generate_heatmaps
#' @order 9
generate_heatmaps <- function(average_percent_similarity,
                              html_title = "Heatmap of Average Similarity",
                              tiff_title = "Heatmap of Average Similarity",
                              xlab = "SampleID", ylab = "SampleID",
                              width_inch = 8, height_inch = 6, dpi = 300) {

  # Create the interactive heatmap with dendrogram branches and save as HTML
  heatmap_html_file <- heatmaply(as.matrix(average_percent_similarity),
                                 Rowv = TRUE, Colv = TRUE,
                                 main = html_title,
                                 xlab = xlab, ylab = ylab)

  melted_data <- melt(average_percent_similarity)

  Var1 <- melted_data$Var1
  Var2 <- melted_data$Var2
  value <- melted_data$value

  heatmap_tiff_file2 <- ggplot(data = melted_data,
                               aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() +
    theme_bw() +
    scale_fill_gradient(low = "blue", high = "green", limits = c(0, 1)) +
    labs(title = tiff_title, x = xlab, y = ylab) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(vjust = 0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())

  # Return the file names of the generated heatmaps
  return(list(html = heatmap_html_file,
              tiff2 = heatmap_tiff_file2))
}
