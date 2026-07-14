find_unique_drugs <- function(x, column, delimiter = ", ") {
  x <- x %>%
    dplyr::pull(column) %>%
    stringr::str_split(delimiter) %>%
    unlist() %>%
    unique() %>%
    sort()
  return(x)
}
