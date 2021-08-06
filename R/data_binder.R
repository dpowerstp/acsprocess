#' Data binder
#'
#' A function to load in a set of ACS rds files at different scales (e.g., place, county) together
#'
#' @param file_root root name of shared acs files want to bind together
#' @param dir_root directory path containing shared acs files
#'
#' @return dataframe
#' @export
#'
#' @examples
data_binder <- function(file_root,
                        dir_root){

  # pull associated filenames
  files <- grep(file_root,
                dir(dir_root),
                value = TRUE)

  df <- purrr::map_dfr(files, ~ {
    read_rds(paste0(dir_root, .x))
  })

  if (nrow(df) == 0 | is.null(df)){
    stop("Error; returned empty dataframe")
  }

  return(df)
}
