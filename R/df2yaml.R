#' Convert dataframe to YAML.
#'
#' @param df Dataframe.
#' @param key_col The columns used as keys, up to 2.
#' @param val_col The columns used as values, this column can contain key: value pairs.
#' @param val_sep The separator used to seperate different key:value pairs in \code{val_col}. Default: ";".
#' @param key_sep The separator used to seperate key and value. Default: ":".
#' @param out_yaml The output yaml file. Default: NULL (string).
#'
#' @return NULL (write YAML) or string (if \code{out_yaml} is NULL).
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames
#' @importFrom yaml as.yaml
#' @importFrom rrapply rrapply
#'
#' @examples
#' library(df2yaml)
#' test_file <- system.file("extdata", "df2yaml_l3.txt", package = "df2yaml")
#' test_data <- read.table(file = test_file, header = TRUE, sep = "\t")
#' df2yaml(df = test_data, key_col = c("paras", "subcmd"), val_col = "values")
df2yaml2 <- function(df, key_col = c("paras", "subcmd"), val_col = "values",
                     val_sep = ";", key_sep = ":", out_yaml = NULL) {
  # check columns
  CheckColumns(df, c(key_col, val_col))
  # remove unused columns
  df <- df[c(key_col, val_col)]
  # check duplicates
  df_dup <- df[duplicated(df[, key_col]), ]
  # remove duplicates
  if (nrow(df_dup) > 0) {
    warning("Detect duplicated key values, we will keep the first!")
    df_valid <- df[!duplicated(df[, key_col]), ]
  } else {
    df_valid <- df
  }
  # sort the data
  df_valid <- df_valid %>% dplyr::arrange(.data[[key_col[1]]])

  if (length(key_col) == 1) {
    # split to list
    df_valid_li <- split(x = df_valid[, val_col], f = df_valid[, key_col])
    if (!is.null(val_sep)) {
      if (is.null(key_sep)) {
        stop("Require key separator!")
      }
      # split valie
      for (ln in names(df_valid_li)) {
        ln_value <- df_valid_li[[ln]]
        if (grepl(pattern = ":", x = ln_value)) {
          ln_value_vec <- strsplit(x = ln_value, split = paste0("[ ]*", val_sep, "[ ]*"))[[1]]
          ln_value_tmp <- lapply(ln_value_vec, function(x) {
            strsplit(x = x, split = paste0("[ ]*", key_sep, "[ ]*"))[[1]]
          }) %>%
            as.data.frame() %>%
            t() %>%
            as.data.frame()
          rownames(ln_value_tmp) <- NULL
          ln_value_li <- ln_value_tmp %>%
            tibble::column_to_rownames(var = "V1") %>%
            t() %>%
            as.data.frame() %>%
            as.list()
          df_valid_li[[ln]] <- ln_value_li
        }
      }
    }
    # convert to yaml
    df_valid_yaml <- yaml::as.yaml(df_valid_li)
    # remove possible '
    df_valid_yaml <- gsub(pattern = "'", replacement = "", df_valid_yaml)
  } else if (length(key_col) == 2) {
    # seperate df according to levels
    df_valid_1 <- df_valid[df_valid[, key_col[length(key_col)]] == "", ]
    df_valid_2 <- df_valid[df_valid[, key_col[length(key_col)]] != "", ]
    # split to list
    df_valid_1_li <- split(x = df_valid_1[, val_col], f = df_valid_1[, key_col[1]])
    df_valid_2_li <- rrapply::rrapply(df_valid_2, how = "unmelt")
    if (!is.null(val_sep)) {
      if (is.null(key_sep)) {
        stop("Require key separator!")
      }
      # split valie
      for (l1n in names(df_valid_1_li)) {
        l1n_value <- df_valid_1_li[[l1n]]
        if (grepl(pattern = ":", x = l1n_value)) {
          l1n_value_vec <- strsplit(x = l1n_value, split = paste0("[ ]*", val_sep, "[ ]*"))[[1]]
          l1n_value_tmp <- lapply(l1n_value_vec, function(x) {
            strsplit(x = x, split = paste0("[ ]*", key_sep, "[ ]*"))[[1]]
          }) %>%
            as.data.frame() %>%
            t() %>%
            as.data.frame()
          rownames(l1n_value_tmp) <- NULL
          l1n_value_li <- l1n_value_tmp %>%
            tibble::column_to_rownames(var = "V1") %>%
            t() %>%
            as.data.frame() %>%
            as.list()
          df_valid_1_li[[l1n]] <- l1n_value_li
        }
      }
    }
    # convert to yaml
    df_valid_1_yaml <- as.yaml(df_valid_1_li)
    # remove possible '
    df_valid_1_yaml <- gsub(pattern = "'", replacement = "", df_valid_1_yaml)
    if (!is.null(val_sep)) {
      if (is.null(key_sep)) {
        stop("Require key separator!")
      }
      # split valie
      for (l2n in names(df_valid_2_li)) {
        l2n_li <- df_valid_2_li[[l2n]]
        for (ll2n in names(l2n_li)) {
          l2n_value <- l2n_li[[ll2n]]
          if (grepl(pattern = ":", x = l2n_value)) {
            l2n_value_vec <- strsplit(x = l2n_value, split = "[ ]*;[ ]*")[[1]]
            l2n_value_tmp <- lapply(l2n_value_vec, function(x) {
              strsplit(x = x, split = "[ ]*:[ ]*")[[1]]
            }) %>%
              as.data.frame() %>%
              t() %>%
              as.data.frame()
            rownames(l2n_value_tmp) <- NULL
            l2n_value_li <- l2n_value_tmp %>%
              tibble::column_to_rownames(var = "V1") %>%
              t() %>%
              as.data.frame() %>%
              as.list()
            l2n_li[[ll2n]] <- l2n_value_li
          }
        }
        df_valid_2_li[[l2n]] <- l2n_li
      }
    }

    # convert to yaml
    df_valid_2_yaml <- yaml::as.yaml(df_valid_2_li)
    # remove possible '
    df_valid_2_yaml <- gsub(pattern = "'", replacement = "", df_valid_2_yaml)
    # merge
    df_valid_yaml <- paste0(df_valid_1_yaml, df_valid_2_yaml)
  } else {
    stop("The length of key_col is bigger than 2!")
  }
  if (is.null(out_yaml)) {
    return(df_valid_yaml)
  } else {
    write(x = df_valid_yaml, file = out_yaml)
  }
}

#' Convert dataframe to YAML.
#'
#' @param df Dataframe.
#' @param key_col The columns used as keys.
#' @param val_col The columns used as values, this column can contain key: value pairs.
#' @param val_sep The separator used to seperate different key:value pairs in \code{val_col}. Default: ";".
#' @param key_sep The separator used to seperate key and value. Default: ":".
#' @param rm_quote Logical value, whether to remove single quotes. Default: TRUE.
#' @param out_yaml Output YAML file. Default: NULL (return string).
#'
#' @return NULL (write YAML) or string (if \code{out_yaml} is NULL).
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames
#' @importFrom yaml as.yaml
#' @importFrom rrapply rrapply
#' @export
#'
#' @examples
#' library(df2yaml)
#' test_file <- system.file("extdata", "df2yaml_l3.txt", package = "df2yaml")
#' test_data <- read.table(file = test_file, header = TRUE, sep = "\t")
#' df2yaml(df = test_data, key_col = c("paras", "subcmd"), val_col = "values")
df2yaml <- function(df, key_col, val_col,
                    val_sep = ";", key_sep = ":", rm_quote = TRUE, out_yaml = NULL) {
  # split dataframe
  df_li <- SplitDF(df = df, key_col = key_col, val_col = val_col)
  final_li <- list()
  # process the split dataframe
  for (n in 1:length(df_li)) {
    n_df <- df_li[[n]]
    n_li <- rrapply::rrapply(n_df, how = "unmelt")
    # split the list
    n_split_li <- ListSplit(li = n_li, val_sep = val_sep, key_sep = key_sep)
    final_li <- c(final_li, n_split_li)
  }
  # convert to dataframe
  df_yaml <- yaml::as.yaml(final_li)
  # remove single quote
  if (rm_quote) {
    df_yaml <- gsub(pattern = "'", replacement = "", df_yaml)
  }
  # save or print
  if (is.null(out_yaml)) {
    return(df_yaml)
  } else {
    write(x = df_yaml, file = out_yaml)
  }
}
