#' Generate frequency of each entry in each column of dataframe
#'
#' Real-life data is rarely perfect and fields in a data.frame contains entries not anticipated.
#' It helps to know your data (along with functions you use) before performing any manipulations on it.
#' This function generates frequency table excel, each column of input dataframe in a separate sheet in output excel.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output text file (should end in ".xlsx")
#' Strongly advised to pass this parameter, else the function's default is "frequency_table.xlsx"
#' @param columns_to_exclude List of columns in input dataset for which frequencies need not be generated
#' Pass a vector of columns for this parameter.
#' For e.g. c("hp","mpg","carb")
#' @param maximum_entries Maximum unique entries in output.
#' For e.g. setting this parameter to 10000 will return only top 10000 occurring entries in each column
#' @param format_width Boolean input indicating if output excel cells' column width need to be formatted to "auto"
#' @param Sl_No_required Boolean input indicating if Sl_No column needs to be present in output excel
#' @param Frequency_required Boolean input indicating if Frequency column needs to be present in output excel
#' @param Percentage_required Boolean input indicating if Percentage column needs to be present in output excel
#' @param Cumulative_Percentage_required Boolean input indicating if Cumulative_Percentage column needs to be present in output excel
#' @param String_Length_required Boolean input indicating if String_Length column needs to be present in output excel
#' @param Sl_No_to_last Boolean input indicating if Sl_No column should be the last column in output excel
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table .N
#' @examples
#' \dontrun{
#' frequency_table(dataset = mtcars, output_filename = tempfile())
#' }

frequency_table = function(dataset, output_filename = "", columns_to_exclude = c(), maximum_entries = 2^20, format_width = TRUE, 
Sl_No_required = TRUE, 
Frequency_required = TRUE, 
Percentage_required = TRUE, 
Cumulative_Percentage_required = FALSE, 
String_Length_required = TRUE, 
Sl_No_to_last = FALSE){

  .data <- Frequency <- Cumulative_Percentage <- Percentage <- Sl_No <- String_Length <- NULL

  if(output_filename == ""){
    output_filename = gsub(x = paste0("frequency_table_",Sys.time(),".xlsx"),pattern = " |:|-",replacement = "_")
  }
  if(substr(output_filename,nchar(output_filename)-4, nchar(output_filename)) !=".xlsx"){
    output_filename = paste0(output_filename, ".xlsx")
  }
  message("Writing frequency table of dataset to ",output_filename)

  dataset = dataset[, !colnames(dataset) %in% columns_to_exclude] # Exclude columns of no interest
  dataset = dataset %>% dplyr::mutate_if(is.factor, as.character) # Change factor columns to character
  data.table::setDT(dataset)                                      # Changing class of dataset to data.table
  workbook = openxlsx::createWorkbook(output_filename)            # Creating and mounting an empty excel file

  for(i in 1:ncol(dataset)){                                      # Loop over each column
    sheet_name = names(dataset)[i]                                # Each column will be stored in a sheet of excel

    # Excel sheet name can have a maximum of 31 characters. If a column-name has more than 31 characters, below line will pick first 15 and last 16 characters.
    sheet_name = ifelse(nchar(sheet_name)>31, paste0(substr(sheet_name,1,15), substr(sheet_name,nchar(sheet_name)-15, nchar(sheet_name))), sheet_name)

    openxlsx::addWorksheet(workbook, sheet_name)                  # Add a new empty sheet for this particular column

    frequency_table = dataset[, i, with = FALSE]                  # Retain current column
    frequency_table = as.data.frame(frequency_table[,list(Frequency=.N), by = c(names(dataset)[i])]) # Aggregate to get frequency

    frequency_table = frequency_table %>%
      dplyr::arrange(dplyr::desc(Frequency)) %>%                              # Sort by decreasing order of frequency
      dplyr::mutate(Percentage = 100*Frequency/sum(Frequency)) %>%     # Create new column, frequency to percentage
      dplyr::mutate(Cumulative_Percentage = cumsum(Percentage)) %>% # Create new column, Cumulative Percentage
      dplyr::mutate(Sl_No = 1:length(.data$Frequency)) %>% # Create new column, serial number
      dplyr::mutate(String_Length = nchar(.data[[names(dataset)[i]]])) %>% # Create new column, string length = number of characters in the value
      dplyr::select(Sl_No,dplyr::everything(),String_Length)                  # Reorder columns

    # Excel supports a maximum of 2^20 rows (1048576). Therefore, Any entries below that in frequency table will not be written
    frequency_table = frequency_table[1:min(nrow(frequency_table),maximum_entries),]

    if(format_width == TRUE){
      openxlsx::setColWidths(workbook, sheet = i, cols = 1:5, widths = "auto")
    }
    if(Sl_No_to_last == TRUE){
      frequency_table = frequency_table %>% dplyr::select(-Sl_No,dplyr::everything(),Sl_No)
    }
    if(Sl_No_required == FALSE){
      frequency_table = frequency_table %>% dplyr::select(-Sl_No)
    }
    if(Frequency_required == FALSE){
      frequency_table = frequency_table %>% dplyr::select(-Frequency)
    }
    if(Percentage_required == FALSE){
      frequency_table = frequency_table %>% dplyr::select(-Percentage)
    }
    if(Cumulative_Percentage_required == FALSE){
      frequency_table = frequency_table %>% dplyr::select(-Cumulative_Percentage)
    }
    if(String_Length_required == FALSE){
      frequency_table = frequency_table %>% dplyr::select(-String_Length)
    }

    openxlsx::writeData(workbook, i, frequency_table)                  # Write frequency table to workbook at ith position

    invisible(gc())                                                    # Silent garbage collection
    message(paste0(names(dataset)[i]," - Completed"))                  # For status printing
  }
  openxlsx::saveWorkbook(workbook, file = output_filename)             # Write final excel and unmount it from R
  rm(dataset,i,frequency_table,workbook,sheet_name)                    # Clean up the intermediate variables (Habit!)
  invisible(gc())                                                      # Silent garbage collection

  invisible()                                                          # To return nothing
}
