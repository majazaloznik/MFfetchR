source("renv/activate.R")
options(renv.config.auto.snapshot = TRUE)
Sys.setenv(http_proxy="http://proxy.gov.si:80")
Sys.setenv(http_proxy_user="http://proxy.gov.si:80")
Sys.setenv(https_proxy="http://proxy.gov.si:80")
Sys.setenv(https_proxy_user="http://proxy.gov.si:80")

cat("UMAR proxy is set!\n")
options(continue = " ")


options(
  usethis.full_name = "Maja Zaloznik",
  usethis.description = list(
    "Authors@R" = utils::person("Maja", "Zaloznik",
                                email = "maja.zaloznik@gmail.com",
                                role = c("aut", "cre")),
    Version = "0.0.0.9000"
  ),
  usethis.overwrite = TRUE
)

# to make devtools available in all interactive sessions
if (interactive()) {
  suppressMessages(require(devtools))
}
