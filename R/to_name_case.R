to_name_case <- function(x) {
  
  x <- gsub("\\.", "", x)
  
  x <- strsplit(x, "")[[1]]
  x <- tolower(x)
  x[1] <- toupper(x[1])
  
  maj <- grep("\\s|[[:punct:]]", x)
  if (length(maj)) {
    x[maj + 1] <- toupper(x[maj + 1])
  }
  
  paste0(x, collapse = "")
}
