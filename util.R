# encode/decode between named character vectors or lists and strings

encode <- function(x) {
  list(names(x), unname(x)) %>%
    transpose() %>%
    map(~paste(.x, collapse = "=")) %>%
    paste(collapse = ",")
}

decode <- function(x) {
  parsed_x <- x %>%
    strsplit(",") %>%
    unlist() %>%
    strsplit("=")
  set_names(map_chr(parsed_x, ~.x[[2]]), map_chr(parsed_x, ~.x[[1]]))
}
