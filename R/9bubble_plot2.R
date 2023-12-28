#' Bubble Plot Percentage Function
#'
#' This function generates a bubble plot using ggplot2 based on percentage data.
#'
#' @param Cluster_SampleID_Percentage A data frame containing cluster, sample ID, and percentage data.
#' @param title The title of the plot.
#' @param x_label The label for the x-axis.
#' @param y_label The label for the y-axis.
#' @param size_label The label for the size variable.
#' @param color_label The label for the color variable.
#'
#' @return A ggplot object representing the bubble plot.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with Cluster_SampleID_Percentage
#' Cluster_SampleID_Percentage <- Cluster_SampleID_Percentage
#' bubble_plot_percentage <- bubble_plot_percentage(Cluster_SampleID_Percentage,
#'                                                 title = "Bubble Plot",
#'                                                 x_label = "Clusters",
#'                                                 y_label = "Sample ID",
#'                                                 size_label = "Percentage",
#'                                                 color_label = "Sample ID")
#'
#' # Save the bubble plot as a TIFF image
#' output_directory <- "output_directory/new"
#' width_inch <- 8
#' height_inch <- 6
#' dpi <- 300
#'
#' # Set the file name for the TIFF image
#' tiff_file <- file.path(output_directory, "bubble_plot_percentage.tiff")
#'
#' # Open the TIFF device
#' tiff(tiff_file, width = width_inch, height = height_inch, units = "in", res = dpi)
#'
#' # Print and save the bubble plot
#' print(bubble_plot_percentage)
#'
#' # Close the TIFF device
#' dev.off()
#' }
#' @rdname M.bubble_plot_percentage
#' @order 13
bubble_plot_percentage <- function(Cluster_SampleID_Percentage, title, x_label, y_label, size_label, color_label) {

  Clusters <- Cluster_SampleID_Percentage$Clusters
  SampleID <- Cluster_SampleID_Percentage$SampleID
  Percentage <- Cluster_SampleID_Percentage$Percentage

  plot <- ggplot(Cluster_SampleID_Percentage, aes(x = as.factor(Clusters), y = SampleID, size = Percentage, color = as.factor(SampleID))) +
    geom_count() +
    labs(title = title,
         x = x_label,
         y = y_label,
         size = size_label,
         color = color_label) +
    theme_minimal()

  return(plot)
}
