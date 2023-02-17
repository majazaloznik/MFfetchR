#' Trim leading spaces
#'
#' @param x character string
#'
#' @return trimmed character string
#' @keywords internal
trim_leading <- function (x)  {
  sub("^\\s+", "", x)
}

#' Trim all spaces
#'
#' Removes all spaces
#' @param x character string
#'
#' @return trimmed character string
#' @keywords internal
trim_inter <- function (x)  {
  gsub("\\s+", "", x)
}


#' Lookup for month codes
#'
#' Switch function for month codes
#'
#' @param x character string with month name
#'
#' @return code
#' @keywords internal
month_codes <- function(x){
  switch(x,
         "JANUAR -" = "H01",
         "JANUAR" = "M01",
         "FEBRUAR" = "M02",
         "MAREC" = "M03",
         "APRIL" = "M04",
         "MAJ" = "M05",
         "JUNIJ" = "M06",
         "JULIJ" = "M07",
         "AVGUST" = "M08",
         "SEPTEMBER" = "M09",
         "OKTOBER" = "M10",
         "NOVEMBER" = "M11",
         "DECEMBER" = "M12",
         "PREDHODNO" = NA,
         stop("Mesec ni pravilno napisan"))
}
