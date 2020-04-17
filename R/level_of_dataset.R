#' Determine the level / primary key of dataset
#'
#' Knowing the level of dataset is paramount to effectively and efficiently manipulate data, and the level of dataset is unknown oftentimes.
#' This function checks for count of unique records in all possible column combinations to
#' determine the level of dataset.
#' Check for text file generated for column combinations with unique records.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output text file (should end in ".txt", although the backend will append if not)
#' Function's default is "level_of_dataset_<system_time>.txt"
#' @return Does not return to calling function, writes to file system rather
#' @export
#' @examples
#' \dontrun{
#' level_of_data(dataset = iris[,c("mpg", "cyl", "disp", "hp")], output_filename = "level_mtcars.txt")
#' }

level_of_data <- function(dataset, output_filename = "") {

  if (output_filename == "") {
    output_filename <- gsub(x = paste0("level_of_data_", Sys.time(), ".txt"), pattern = " |:|-", replacement = "_")
  }
  if (substr(output_filename, nchar(output_filename) - 3, nchar(output_filename)) != ".txt") {
    output_filename <- paste0(output_filename, ".txt")
  }
  message("Writing level of data results to ", output_filename)

  concatenated <- NULL

  generate_column_combinations <- function(dataset, n) {

    result <- utils::combn(names(dataset), n) %>%
      t %>%
      data.frame()

    result <- result %>%
      stats::setNames(paste0("V", seq_len(length(result)))) %>%
      as.matrix()
  }

  check_for_level <- function(dataset, column_combinations) {
    for (j in seq_len(nrow(column_combinations))) {
      concatenated_combination <- dataset %>%
        dplyr::select(column_combinations[j, ]) %>%
        tidyr::unite(concatenated, sep = ";;;") %>%
        dplyr::pull(concatenated)

      residual <- length(concatenated_combination) -  length(unique(concatenated_combination))

      print(paste(column_combinations[j, ], collapse = " x "), "----", "residual:", format(residual, big.mark = ",", scientific = FALSE), "\n")

      if (residual == 0) {
        print(paste(paste(column_combinations[j, ], collapse = " x "), "is a level", "\n"))
      }else{
        print(paste(paste(column_combinations[j, ], collapse = " x "), "is not a level", "\n"))
      }
      print("")
      rm(concatenated_combination, residual); invisible(gc());
    }
  }

  data.table::setDF(dataset)
  message(paste0(ncol(dataset), " columns present in dataframe passed to function:"))
  dataset %>% names %>% paste(collapse = ", ") %>% paste0("\n") %>% message

  sink(output_filename, type = c("output"), append = TRUE)  # Ensures the glimpse output is written to output_filename text file
  print(paste0(ncol(dataset), " columns present in dataframe passed to function:", "\n"))
  dataset %>% names %>% paste(collapse = ", ") %>% paste0("\n\n") %>% print

  for (i in seq_len(ncol(dataset))) {
    print(paste0(i, " COLUMN COMBINATION(S) BELOW:", "\n\n"))
    column_combinations <- generate_column_combinations(dataset, i)
    check_for_level(dataset, column_combinations)
    message(paste0(i, " column combination(s) checked for level at ", Sys.time()))
  }
  sink()  # Unmounts output_filename text file

  invisible()
}
