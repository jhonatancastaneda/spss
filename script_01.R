library(haven)
library(dplyr)
library(sqldf)

list.files()

data <- read_sav("SPSS_Esmeraldas_Poblacion.sav")

dim(data)
glimpse(data)

sqldf("SELECT count(P01)
      FROM data
      Group By P01")

View(data)

names(data)

install.packages('DT',dependencies = TRUE)








