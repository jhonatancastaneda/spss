


#1.-
  
  
install.packages('foreign',dependencies = TRUE)
library(foreign)
library(dplyr)
library(DT)
library(sqldf)

Kurt <- function(x){
  m4 <- mean((x-mean(x,na.rm = TRUE))*(x-mean(x,na.rm = TRUE))*(x-mean(x,na.rm = TRUE))*(x-mean(x,na.rm = TRUE)),na.rm = TRUE)
  n <- m4/(sd(x,na.rm = TRUE)*sd(x,na.rm = TRUE)*sd(x,na.rm = TRUE)*sd(x,na.rm = TRUE))-3
  return(n)
}
Asim <- function(x){
  m3 <- mean((x-mean(x,na.rm = TRUE))*(x-mean(x,na.rm = TRUE))*(x-mean(x,na.rm = TRUE)),na.rm = TRUE)
  n <- m3/(sd(x,na.rm = TRUE)*sd(x,na.rm = TRUE)*sd(x,na.rm = TRUE))
  return(n)
}

est <- function(vector){
  vari <- list()
  vari$Min <- min (vector,na.rm = TRUE)
  vari$P1 <- quantile(vector,probs = c(0.01),na.rm = TRUE)
  vari$P2 <- quantile(vector,probs = c(0.02),na.rm = TRUE)
  vari$P5 <- quantile(vector,probs = c(0.05),na.rm = TRUE)
  vari$Mediana <- median(vector,na.rm = TRUE)
  vari$Media <- mean(vector,na.rm = TRUE)
  vari$Kurt <- Kurt(vector)
  vari$Asim <- Asim(vector)
  vari$Rang_intrq <-  IQR(vector,na.rm = TRUE)
  vari$P95 <- quantile(vector,probs = c(0.95),na.rm = TRUE)
  vari$P98 <- quantile(vector,probs = c(0.98),na.rm = TRUE)
  vari$P99 <- quantile(vector,probs = c(0.99),na.rm = TRUE)  
  vari$Max <- max(vector,na.rm = TRUE)
  return(unlist(vari))
}
options(stringsAsFactors = FALSE)
data2 <- read.spss("base_modelos.sav",to.data.frame = TRUE)
data2 <- data2[1:52]

columnas <- which(unlist(lapply(data2, class))=="numeric")
num_base <- data2[columnas]
cod <- colnames(num_base)
tablas <- lapply(num_base, est)

print("Los resultados que se presentan en la tabla 2 para cada variable son los siguientes:")
tablas1 <- c("Min","P1","P2","P5","Mediana","Media","Kurt","Asim",
            "Rang_intrq","P95","P98","P99","Max")

TABLA_1 <- cbind(cod1,tablas1)
datatable(TABLA_1)

#TABLA_ <- cbind(cod,tablas) debería ser usado para presentar la tabla de
#resultados, pero la libreía DT ya lo hace automaticamente.
TABLA_ <- cbind(tablas)
datatable(TABLA_)




#2.-

columnas <- which(unlist(lapply(data2, class))=="character")
View(char_base) <- data2[columnas]

for (i in 1:length(char_base)){
  
  aplica <- char_base[[1]]
  
  sqldf("SELECT 
        FROM data
        Group By I04")
  
