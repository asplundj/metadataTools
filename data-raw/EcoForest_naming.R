
library(dplyr)

EcoForest_naming <- tibble::tibble(
  name = c("Site_ID", "Forest_Type", "Plot_ID"),
  mandatory = c(TRUE, TRUE, FALSE),
  valid_values = list(
    c("BLA","BRA","GUL","HAL","HEM","LAN","MRK","OYT","SAR","SKO","STR","TRE"),
    c("CC","NN"),
    NULL
  ),
  default_description = c("Site code", "Forest type", "Plot identifier")
)

# 2) Save it to data/ as .rda ----------------------------------------------
usethis::use_data(EcoForest_naming, overwrite = TRUE, compress = "gzip")

