# Fromn https://rstudio-pubs-static.s3.amazonaws.com/66092_6ed5895fde9e451d8a4b9491bbe8e8f4.html#/10

# Load required libraries
library("magrittr")
library("dplyr")
library("broom")
library("stringr")
library("knitr")

# Print with n digits of precision
fixed_digits <- function(xs, n = 2) {
  formatC(xs, digits = n, format = "f")
}


# Print three digits of a p-value, but use
# the "< .001" notation on tiny values.
format_pval <- function(ps, html = FALSE) {
  tiny <- ifelse(html, "&lt;&nbsp;.001", "< .001")
  ps_chr <- ps %>% fixed_digits(3) %>%
    remove_leading_zero
  ps_chr[ps < 0.001] <- tiny
  ps_chr
}
  
# Don't print leading zero on bounded numbers.
remove_leading_zero <- function(xs) {
  
  # Problem if any value is greater than 1.0
  digit_matters <- xs %>% as.numeric %>%
    abs %>% is_greater_than(1)
      if (any(digit_matters)) {
      warning("Non-zero leading digit")
      }
  
    str_replace(xs, "^(-?)0", "\\1")
}


morans_test <- function(model) {
  model.res <- round(residuals(model, type="response"))
  moran.test(model.res, weights)
}







