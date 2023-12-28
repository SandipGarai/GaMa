# Function to plot and return visualizations
#' Plots the number of sequences and the probability of sequences per SampleID.
#'
#' This function takes a data frame with 'SampleID' and 'SequenceID' columns and
#' creates two bar plots. The first plot shows the number of sequences per SampleID,
#' and the second plot shows the probability of sequences per SampleID.
#'
#' @param final_data A data frame with 'SampleID' and 'SequenceID' columns.
#'
#' @return A list containing two ggplot2 plots: 'plot_num_sequences' and 'plot_prob'.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plots <- SampleID_vs_NumSequences(final_data)
#'
#' # Set the file name for the TIFF images
#' tiff_file_num_sequences <- "output_directory/0. SampleID_vs_NumSequences.tiff"
#' tiff_file_prob <- "output_directory/0. SampleID_vs_Probability.tiff"
#'
#' # Set the width, height, and DPI parameters
#' width_inch <- 8
#' height_inch <- 8
#' dpi <- 300
#'
#' # Open the TIFF devices and save the plots
#' tiff(tiff_file_num_sequences, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(plots$plot_num_sequences)
#' dev.off()
#'
#' tiff(tiff_file_prob, width = width_inch, height = height_inch, units = "in", res = dpi)
#' print(plots$plot_prob)
#' dev.off()
#' }
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr group_by summarize mutate n
#' @importFrom ggplot2 ggplot aes_string geom_bar labs theme_minimal theme element_text
#' @importFrom magrittr %>%
#' @rdname E.SampleID_vs_NumSequences
#' @order 5
SampleID_vs_NumSequences <- function(final_data) {

  SampleID <- final_data$SampleID

  # Create an empty vector NumSequences
  NumSequences <- sequence_counts$NumSequences
  prob <- sequence_counts$prob

  # Check the number of sequences in each SampleID
  sequence_counts <- final_data %>%
    group_by(SampleID) %>%
    summarize(NumSequences = n()) %>%
    mutate(prob = NumSequences / sum(NumSequences))

  # Create a bar plot with vertical x-axis labels for NumSequences
  plot_num_sequences <- ggplot(sequence_counts, aes_string(x = "SampleID", y = "NumSequences")) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Number of Sequences per SampleID",
         x = "SampleID",
         y = "Number of Sequences") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  # Create a bar plot with vertical x-axis labels for prob
  plot_prob <- ggplot(sequence_counts, aes_string(x = "SampleID", y = "prob")) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Probability of Sequences per SampleID",
         x = "SampleID",
         y = "Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  # Return the plots
  return(list(plot_num_sequences = plot_num_sequences, plot_prob = plot_prob))
}
