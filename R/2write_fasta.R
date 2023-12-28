# Write Fasta
#' Writes a data frame to a FASTA file.
#'
#' This function takes a data frame with 'SequenceID' and 'Sequence' columns
#' and writes the contents to a FASTA file. Each row in the data frame corresponds
#' to a sequence in the FASTA file, with the 'SequenceID' used as the header line
#' and the 'Sequence' as the sequence line.
#'
#' @param data A data frame with 'SequenceID' and 'Sequence' columns.
#'
#' @return A character vector representing the contents of the FASTA file.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- final_data
#'
#' # Call the function
#' fasta_content <- write_fasta(data)
#'
#' # Print or use the `fasta_content` as needed
#' print(fasta_content)
#'
#' # Specify the output file path
#' output_file_path <- "output.fasta"
#'
#' # Open a connection to the output file
#' output_file <- file(output_file_path, "w")
#'
#' # Write the content to the file
#' writeLines(fasta_content, output_file)
#'
#' # Close the output file
#' close(output_file)
#'
#' # Print a message indicating successful file creation
#' cat("FASTA file has been created and saved at:", output_file_path, "\n")
#' }
#' @export
#' @rdname D.write_fasta
#' @order 4
write_fasta <- function(data) {
  # Initialize an empty character vector
  fasta_content <- character()

  # Iterate through each row in the data frame
  for (i in 1:nrow(data)) {
    # Add the header line (">SequenceID") to the character vector
    fasta_content <- c(fasta_content, paste(">", data$SequenceID[i]))

    # Add the sequence line to the character vector
    fasta_content <- c(fasta_content, data$Sequence[i])
  }

  # Return the character vector representing the contents of the FASTA file
  return(fasta_content)
}
