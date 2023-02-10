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

#' Lookup for missing konto codes
#'
#' Switch function for missing konto codes.
#'
#' @param x character input
#'
#' @return lookedup code
#' @keywords internal
konto_codes <- function(x){
  switch(x,
         "TEKO\u010cI PRIHODKI" = 901,
         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH DELE\u017dEV"  = 902,
         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH  DELEEV" = 902,
         "PREJETA MINUS DANA POSOJILA IN SPREMEMBE KAPITALSKIH DELE\u017dEV\u017e" = 902,
         "NETO ZADOL\u017dEVANJE" = 903,
         "NETO ZADOL\u017dEVANJE (ODPLA\u010cILA) DOLGA" = 903,
         "POVE\u010cANJE (ZMANJ\u0160ANJE) SREDSTEV NA RA\u010cUNIH" = 904,
         "(I.+IV.+VII.-II.-V.-VIII.)" = 904,
         "NETO FINANCRANJE" = 905,
         "NETO FINANCIRANJE" = 905,
         "PRESE\u017dEK (PRIMANJKLJAJ)"= 906,
         "PRIMARNI PRESE\u017dEK (PRIMANJKLJAJ)" = 907,
         "PLA\u010cE IN DRUGI IZDATKI ZAPOSLENIM" = 911,
         "PRISPEVKI DELODAJALCEV ZA SOCIALNO VARNOST" = 912,
         "IZDATKI ZA BLAGO IN STORITVE" = 913,
         stop(paste("Koda \u0161e ne obstaja za", x)))
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
