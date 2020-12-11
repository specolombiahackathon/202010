# generate anonymized data
# https://jangorecki.github.io/blog/2014-11-07/Data-Anonymization-in-R.html

library(dplyr)
library(digest)
library(data.table)



months <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

# choose columns to mask
cols_to_mask <- c("DEPARTAMENTO", "MUNICIPIO", "OPERADORA", "CONTRATO", "CAMPO", "CUENCA", "EMPRESA")


#' Function to anonymize data
#' 
#' @param x 
#' 
anonymize <- function(x, algo = "crc32"){
  unq_hashes <- vapply(unique(x), function(object) digest(object, algo = algo), FUN.VALUE = "", USE.NAMES = TRUE)
  unname(unq_hashes[x])
}


set.seed(1234)

# subset data
rawdata2 <- rawdata %>% 
  filter(grepl("PAREX", OPERADORA) | grepl("OCCIDENTAL", OPERADORA) | grepl("VETRA", OPERADORA) | grepl("TALISMAN", OPERADORA),
         YEAR %in% c(2017:2019)) %>% 
  # apply random noise to production data
  mutate(across(all_of(months), `*`, rnorm(1, 1, 0.05))) %>% 
  as.data.table()


# run anomimization
rawdata2_anonymous <- rawdata2[ , cols_to_mask := lapply(.SD, anonymize), .SDcols = cols_to_mask, with = FALSE]      


# export to Excel files per year
rawdata2_anonymous %>% 
  group_by(YEAR) %>% 
  group_walk(~ writexl::write_xlsx(.x, paste0("Producción Fiscalizada Crudo ", .y$YEAR, ".xlsx")))


