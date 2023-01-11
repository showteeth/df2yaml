# check the existence of columns in dataframe
CheckColumns <- function(df, col) {
  if (!all(col %in% colnames(df))) {
    invalid_col <- setdiff(col, colnames(df))
    stop(paste0(invalid_col, " not in given dataframe. Please check!"))
  }
}

#' Split dataframe.
#'
#' @param df Dataframe.
#' @param key_col The columns used as keys.
#' @param val_col The columns used as values, this column can contain key: value pairs.
#'
#' @return List of dataframe.
#' @importFrom magrittr %>%
#'
#' @examples
#' # df <- data.frame(
#' #   A = c("a", "a", "", "", "", "f"),
#' #   B = c("a", "b", "", "", "", "f"),
#' #   C = c("a", "b", "c", "", "", "f"),
#' #   D = c("a", "b", "c", "d", "", "f"),
#' #   E = c("a", "b", "c", "d", "e", "f"),
#' #   G = c("a", "b", "c", "d", "e", "f")
#' # ) %>%
#' #   t() %>%
#' #   as.data.frame()
#' # colnames(df) <- c("c1", "c2", "c3", "c4", "c5", "c6")
#' # SplitDF(df = df, key_col = c("c1", "c2", "c3", "c4", "c5"), val_col = "c6")
SplitDF <- function(df, key_col, val_col) {
  # check columns
  CheckColumns(df, c(key_col, val_col))
  # remove unused columns
  df <- df[c(key_col, val_col)]

  # remove all key_col are empty
  df <- df[!apply(df[key_col] == "", 1, all), ]
  # split dataframe
  df_li <- list()
  df_num <- 1
  if (length(key_col) == 1) {
    df_li[[1]] <- df
  } else if (length(key_col) >= 2) {
    for (co in 2:(length(key_col))) {
      co_df <- df[df[, key_col[co]] == "", ]
      if (nrow(co_df) > 0) {
        co_df[, key_col[co:length(key_col)]] <- NULL
        df_li[[df_num]] <- co_df
        df_num <- df_num + 1
      }
      df <- df[df[, key_col[co]] != "", ]
      if (co == length(key_col)) {
        df_li[[df_num]] <- df
      }
    }
  }
  return(df_li)
}

# split list to nested list according to val_sep and key_sep
ListSplit <- function(li, val_sep = ";", key_sep = ":") {
  res_li <- rrapply::rrapply(
    object = li,
    f = function(x) {
      if (grepl(pattern = key_sep, x = x)) {
        x_vec <- strsplit(x = x, split = paste0("[ ]*", val_sep, "[ ]*"))[[1]]
        x_tmp <- lapply(x_vec, function(x) {
          strsplit(x = x, split = paste0("[ ]*", key_sep, "[ ]*"))[[1]]
        }) %>%
          as.data.frame() %>%
          t() %>%
          as.data.frame()
        rownames(x_tmp) <- NULL
        x_li <- x_tmp %>%
          tibble::column_to_rownames(var = "V1") %>%
          t() %>%
          as.data.frame() %>%
          as.list()
        x_li
      } else {
        x
      }
    },
    how = "replace"
  )
  return(res_li)
}
