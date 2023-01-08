# check the existence of columns in dataframe
CheckColumns <- function(df, col) {
  if (!all(col %in% colnames(df))) {
    invalid_col <- setdiff(col, colnames(df))
    stop(paste0(invalid_col, " not in given dataframe. Please check!"))
  }
}
