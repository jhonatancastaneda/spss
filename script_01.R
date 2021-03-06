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





substring()
dplyr

data <- tbl_df(data)
lapply(data, class)
clases <- unlist (lapply(data, class))=="labelled"
clases[clases==TRUE]
clases[clases==FALSE]

columnas <- which(unlist(lapply(data, class))=="numeric")
subdata <- data[,columnas]



est <- function(vector){
  vari <- list()
  vari$min <- min (vector,na.rm = TRUE)
  vari$p5 <- quantile(vector,probs = c(0.05),na.rm = TRUE)
  vari$mediana <- median(vector,na.rm = TRUE)
  vari$p95 <- quantile(vector,probs = c(0.95),na.rm = TRUE)
  vari$max <- max(vector,na.rm = TRUE)
  return(unlist(vari))
}
  

lapply(subdata, est)


lapply(data2, is.character)



data2 <- tbl_dt(data2)




data2 <- read_sav("base_modelos.sav")

summary(data2)


#la tabla no de las miles de variables, solo de las 53 primeras



