# Data Formatting Functions ----

#' Format PMBB Echo File
#' 
#' Reads and formats echocardiogram data files with standardized column names.
#' 
#' @param file_path Character. Path to the data file.
#' @param measure_name_col Character. Name of the column containing measure names.
#'   Default is "COMMON_NAME".
#' @param measure_value_col Character. Name of the column containing measure values.
#'   Default is "ORD_VALUE".
#' @param IID_col Character. Name of the column containing patient IDs.
#'   Default is "PMBB_ID".
#' @param measure_unit_col Character. Name of the column containing measure units.
#'   Default is "REFERENCE_UNIT".
#' @param procedure_name_col Character. Name of the column containing procedure names.
#'   Default is "DESCRIPTION".
#' @param exam_id Character. Name of the column containing exam IDs.
#'   Default is "ORDER_PROC_ID".
#' @param exam_date_col Character. Name of the column containing exam dates.
#'   Default is "ORDERING_DATE_SHIFTED".
#' 
#' @return A data frame with standardized column names.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- pmbb_echofile_format("path/to/echo_data.csv")
#' 
#' # For ProSolv data with different column names
#' df_prosolv <- pmbb_echofile_format(
#'   "prosolv_data.csv",
#'   measure_name_col = "COMMON_NAME",
#'   exam_date_col = "exam_date_shifted"
#' )
#' }
pmbb_echofile_format <- function(file_path, 
                                measure_name_col = "COMMON_NAME", 
                                measure_value_col = "ORD_VALUE", 
                                IID_col = "PMBB_ID", 
                                measure_unit_col = "REFERENCE_UNIT", 
                                procedure_name_col = "DESCRIPTION", 
                                exam_id = "ORDER_PROC_ID",
                                exam_date_col = "ORDERING_DATE_SHIFTED") {
  
  # Read the file
  df <- vroom::vroom(file_path) %>%
    # Dynamically select columns using !!sym()
    dplyr::select(
      IID = !!rlang::sym(IID_col),
      measure_name = !!rlang::sym(measure_name_col),
      measure_value = !!rlang::sym(measure_value_col),
      measure_unit = !!rlang::sym(measure_unit_col),
      procedure_name = !!rlang::sym(procedure_name_col),
      exam_date = !!rlang::sym(exam_date_col),
      exam_id = !!rlang::sym(exam_id)
    )
  
  return(df)
}

#' Process Measure Values
#' 
#' Cleans and processes measure values, handling numeric ranges and invalid patterns.
#' 
#' @param df Data frame containing measure data.
#' @param measure_type Character. Type of measure processing. Default is "numeric".
#'   Use "character" for non-numeric measures.
#' 
#' @return Data frame with cleaned measure values in a new column `measure_value_cleaned`.
#' 
#' @details 
#' For numeric measures, this function:
#' \itemize{
#'   \item Removes units from measure values
#'   \item Converts ranges (e.g., "60-70") to their mean value
#'   \item Converts valid numeric strings to numbers
#'   \item Sets invalid patterns to NA
#' }
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Process numeric measurements
#' df_clean <- process_measure_value(df, measure_type = "numeric")
#' 
#' # Keep character measurements as-is
#' df_char <- process_measure_value(df, measure_type = "character")
#' }
process_measure_value <- function(df, measure_type = "numeric") {
  if (measure_type == "numeric") {
    df <- df %>%
      dplyr::mutate(
        # Remove the measure_unit from measure_value, accounting for spaces
        measure_value_cleaned = gsub(
          paste0(" ?(", paste(unique(measure_unit), collapse = "|"), ")"), 
          "", 
          measure_value
        ),
        
        # Handle numeric ranges and invalid patterns
        measure_value_cleaned = ifelse(
          grepl("^-?[0-9]+\\.?[0-9]*-[0-9]+\\.?[0-9]*$", measure_value_cleaned),  # Detect ranges like "60-70"
          sapply(strsplit(measure_value_cleaned, "-"), 
                 function(x) {
                   nums <- as.numeric(x)
                   if (any(is.na(nums))) NA_real_ else mean(nums, na.rm = TRUE)
                 }),
          ifelse(
            grepl("^-?[0-9]+(\\.[0-9]+)?$", measure_value_cleaned),  # Detect valid single numeric values
            as.numeric(measure_value_cleaned),  # Convert valid numbers to numeric
            NA_real_  # Set all other patterns to NA
          )
        )
      )
  } else {
    df <- df %>%
      dplyr::mutate(measure_value_cleaned = measure_value)  # Retain the original values for non-numeric
  }
  
  # Calculate the number of NA values in measure_value_cleaned
  na_count <- sum(is.na(df$measure_value_cleaned))
  cat("There are ", na_count, " invalid values to remove \n")
  
  return(df)
}