#Loading packages and dataset
library (forecast); library(readxl); library(writexl); library(dplyr)
setwd('C:/Users/RAV_I/OneDrive/Área de Trabalho') 
d1 <- read_excel("Dataset.xlsx", sheet="Dataset")

#Set the column of the dataset to be used in the analysis
d2 <- as.ts (d1$China)

#Set the number of ts to the projection from the ARIMA models
ts <- 7

#Run the 1,000 distinct ARIMA models
try({pj1<- 0; mod <- Arima (d2, order=c(0,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj1 <- cbind(pj, aic, sd);pj1})
try({pj2<- 0; mod <- Arima (d2, order=c(0,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj2 <- cbind(pj, aic, sd);pj2})
try({pj3<- 0; mod <- Arima (d2, order=c(0,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj3 <- cbind(pj, aic, sd);pj3})
try({pj4<- 0; mod <- Arima (d2, order=c(0,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj4 <- cbind(pj, aic, sd);pj4})
try({pj5<- 0; mod <- Arima (d2, order=c(0,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj5 <- cbind(pj, aic, sd);pj5})
try({pj6<- 0; mod <- Arima (d2, order=c(0,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj6 <- cbind(pj, aic, sd);pj6})
try({pj7<- 0; mod <- Arima (d2, order=c(0,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj7 <- cbind(pj, aic, sd);pj7})
try({pj8<- 0; mod <- Arima (d2, order=c(0,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj8 <- cbind(pj, aic, sd);pj8})
try({pj9<- 0; mod <- Arima (d2, order=c(0,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj9 <- cbind(pj, aic, sd);pj9})
try({pj10<- 0; mod <- Arima (d2, order=c(0,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj10 <- cbind(pj, aic, sd);pj10})
try({pj11<- 0; mod <- Arima (d2, order=c(0,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj11 <- cbind(pj, aic, sd);pj11})
try({pj12<- 0; mod <- Arima (d2, order=c(0,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj12 <- cbind(pj, aic, sd);pj12})
try({pj13<- 0; mod <- Arima (d2, order=c(0,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj13 <- cbind(pj, aic, sd);pj13})
try({pj14<- 0; mod <- Arima (d2, order=c(0,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj14 <- cbind(pj, aic, sd);pj14})
try({pj15<- 0; mod <- Arima (d2, order=c(0,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj15 <- cbind(pj, aic, sd);pj15})
try({pj16<- 0; mod <- Arima (d2, order=c(0,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj16 <- cbind(pj, aic, sd);pj16})
try({pj17<- 0; mod <- Arima (d2, order=c(0,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj17 <- cbind(pj, aic, sd);pj17})
try({pj18<- 0; mod <- Arima (d2, order=c(0,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj18 <- cbind(pj, aic, sd);pj18})
try({pj19<- 0; mod <- Arima (d2, order=c(0,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj19 <- cbind(pj, aic, sd);pj19})
try({pj20<- 0; mod <- Arima (d2, order=c(0,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj20 <- cbind(pj, aic, sd);pj20})
try({pj21<- 0; mod <- Arima (d2, order=c(0,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj21 <- cbind(pj, aic, sd);pj21})
try({pj22<- 0; mod <- Arima (d2, order=c(0,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj22 <- cbind(pj, aic, sd);pj22})
try({pj23<- 0; mod <- Arima (d2, order=c(0,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj23 <- cbind(pj, aic, sd);pj23})
try({pj24<- 0; mod <- Arima (d2, order=c(0,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj24 <- cbind(pj, aic, sd);pj24})
try({pj25<- 0; mod <- Arima (d2, order=c(0,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj25 <- cbind(pj, aic, sd);pj25})
try({pj26<- 0; mod <- Arima (d2, order=c(0,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj26 <- cbind(pj, aic, sd);pj26})
try({pj27<- 0; mod <- Arima (d2, order=c(0,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj27 <- cbind(pj, aic, sd);pj27})
try({pj28<- 0; mod <- Arima (d2, order=c(0,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj28 <- cbind(pj, aic, sd);pj28})
try({pj29<- 0; mod <- Arima (d2, order=c(0,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj29 <- cbind(pj, aic, sd);pj29})
try({pj30<- 0; mod <- Arima (d2, order=c(0,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj30 <- cbind(pj, aic, sd);pj30})
try({pj31<- 0; mod <- Arima (d2, order=c(0,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj31 <- cbind(pj, aic, sd);pj31})
try({pj32<- 0; mod <- Arima (d2, order=c(0,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj32 <- cbind(pj, aic, sd);pj32})
try({pj33<- 0; mod <- Arima (d2, order=c(0,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj33 <- cbind(pj, aic, sd);pj33})
try({pj34<- 0; mod <- Arima (d2, order=c(0,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj34 <- cbind(pj, aic, sd);pj34})
try({pj35<- 0; mod <- Arima (d2, order=c(0,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj35 <- cbind(pj, aic, sd);pj35})
try({pj36<- 0; mod <- Arima (d2, order=c(0,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj36 <- cbind(pj, aic, sd);pj36})
try({pj37<- 0; mod <- Arima (d2, order=c(0,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj37 <- cbind(pj, aic, sd);pj37})
try({pj38<- 0; mod <- Arima (d2, order=c(0,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj38 <- cbind(pj, aic, sd);pj38})
try({pj39<- 0; mod <- Arima (d2, order=c(0,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj39 <- cbind(pj, aic, sd);pj39})
try({pj40<- 0; mod <- Arima (d2, order=c(0,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj40 <- cbind(pj, aic, sd);pj40})
try({pj41<- 0; mod <- Arima (d2, order=c(0,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj41 <- cbind(pj, aic, sd);pj41})
try({pj42<- 0; mod <- Arima (d2, order=c(0,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj42 <- cbind(pj, aic, sd);pj42})
try({pj43<- 0; mod <- Arima (d2, order=c(0,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj43 <- cbind(pj, aic, sd);pj43})
try({pj44<- 0; mod <- Arima (d2, order=c(0,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj44 <- cbind(pj, aic, sd);pj44})
try({pj45<- 0; mod <- Arima (d2, order=c(0,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj45 <- cbind(pj, aic, sd);pj45})
try({pj46<- 0; mod <- Arima (d2, order=c(0,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj46 <- cbind(pj, aic, sd);pj46})
try({pj47<- 0; mod <- Arima (d2, order=c(0,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj47 <- cbind(pj, aic, sd);pj47})
try({pj48<- 0; mod <- Arima (d2, order=c(0,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj48 <- cbind(pj, aic, sd);pj48})
try({pj49<- 0; mod <- Arima (d2, order=c(0,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj49 <- cbind(pj, aic, sd);pj49})
try({pj50<- 0; mod <- Arima (d2, order=c(0,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj50 <- cbind(pj, aic, sd);pj50})
try({pj51<- 0; mod <- Arima (d2, order=c(0,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj51 <- cbind(pj, aic, sd);pj51})
try({pj52<- 0; mod <- Arima (d2, order=c(0,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj52 <- cbind(pj, aic, sd);pj52})
try({pj53<- 0; mod <- Arima (d2, order=c(0,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj53 <- cbind(pj, aic, sd);pj53})
try({pj54<- 0; mod <- Arima (d2, order=c(0,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj54 <- cbind(pj, aic, sd);pj54})
try({pj55<- 0; mod <- Arima (d2, order=c(0,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj55 <- cbind(pj, aic, sd);pj55})
try({pj56<- 0; mod <- Arima (d2, order=c(0,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj56 <- cbind(pj, aic, sd);pj56})
try({pj57<- 0; mod <- Arima (d2, order=c(0,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj57 <- cbind(pj, aic, sd);pj57})
try({pj58<- 0; mod <- Arima (d2, order=c(0,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj58 <- cbind(pj, aic, sd);pj58})
try({pj59<- 0; mod <- Arima (d2, order=c(0,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj59 <- cbind(pj, aic, sd);pj59})
try({pj60<- 0; mod <- Arima (d2, order=c(0,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj60 <- cbind(pj, aic, sd);pj60})
try({pj61<- 0; mod <- Arima (d2, order=c(0,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj61 <- cbind(pj, aic, sd);pj61})
try({pj62<- 0; mod <- Arima (d2, order=c(0,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj62 <- cbind(pj, aic, sd);pj62})
try({pj63<- 0; mod <- Arima (d2, order=c(0,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj63 <- cbind(pj, aic, sd);pj63})
try({pj64<- 0; mod <- Arima (d2, order=c(0,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj64 <- cbind(pj, aic, sd);pj64})
try({pj65<- 0; mod <- Arima (d2, order=c(0,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj65 <- cbind(pj, aic, sd);pj65})
try({pj66<- 0; mod <- Arima (d2, order=c(0,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj66 <- cbind(pj, aic, sd);pj66})
try({pj67<- 0; mod <- Arima (d2, order=c(0,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj67 <- cbind(pj, aic, sd);pj67})
try({pj68<- 0; mod <- Arima (d2, order=c(0,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj68 <- cbind(pj, aic, sd);pj68})
try({pj69<- 0; mod <- Arima (d2, order=c(0,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj69 <- cbind(pj, aic, sd);pj69})
try({pj70<- 0; mod <- Arima (d2, order=c(0,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj70 <- cbind(pj, aic, sd);pj70})
try({pj71<- 0; mod <- Arima (d2, order=c(0,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj71 <- cbind(pj, aic, sd);pj71})
try({pj72<- 0; mod <- Arima (d2, order=c(0,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj72 <- cbind(pj, aic, sd);pj72})
try({pj73<- 0; mod <- Arima (d2, order=c(0,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj73 <- cbind(pj, aic, sd);pj73})
try({pj74<- 0; mod <- Arima (d2, order=c(0,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj74 <- cbind(pj, aic, sd);pj74})
try({pj75<- 0; mod <- Arima (d2, order=c(0,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj75 <- cbind(pj, aic, sd);pj75})
try({pj76<- 0; mod <- Arima (d2, order=c(0,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj76 <- cbind(pj, aic, sd);pj76})
try({pj77<- 0; mod <- Arima (d2, order=c(0,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj77 <- cbind(pj, aic, sd);pj77})
try({pj78<- 0; mod <- Arima (d2, order=c(0,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj78 <- cbind(pj, aic, sd);pj78})
try({pj79<- 0; mod <- Arima (d2, order=c(0,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj79 <- cbind(pj, aic, sd);pj79})
try({pj80<- 0; mod <- Arima (d2, order=c(0,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj80 <- cbind(pj, aic, sd);pj80})
try({pj81<- 0; mod <- Arima (d2, order=c(0,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj81 <- cbind(pj, aic, sd);pj81})
try({pj82<- 0; mod <- Arima (d2, order=c(0,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj82 <- cbind(pj, aic, sd);pj82})
try({pj83<- 0; mod <- Arima (d2, order=c(0,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj83 <- cbind(pj, aic, sd);pj83})
try({pj84<- 0; mod <- Arima (d2, order=c(0,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj84 <- cbind(pj, aic, sd);pj84})
try({pj85<- 0; mod <- Arima (d2, order=c(0,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj85 <- cbind(pj, aic, sd);pj85})
try({pj86<- 0; mod <- Arima (d2, order=c(0,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj86 <- cbind(pj, aic, sd);pj86})
try({pj87<- 0; mod <- Arima (d2, order=c(0,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj87 <- cbind(pj, aic, sd);pj87})
try({pj88<- 0; mod <- Arima (d2, order=c(0,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj88 <- cbind(pj, aic, sd);pj88})
try({pj89<- 0; mod <- Arima (d2, order=c(0,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj89 <- cbind(pj, aic, sd);pj89})
try({pj90<- 0; mod <- Arima (d2, order=c(0,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj90 <- cbind(pj, aic, sd);pj90})
try({pj91<- 0; mod <- Arima (d2, order=c(0,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj91 <- cbind(pj, aic, sd);pj91})
try({pj92<- 0; mod <- Arima (d2, order=c(0,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj92 <- cbind(pj, aic, sd);pj92})
try({pj93<- 0; mod <- Arima (d2, order=c(0,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj93 <- cbind(pj, aic, sd);pj93})
try({pj94<- 0; mod <- Arima (d2, order=c(0,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj94 <- cbind(pj, aic, sd);pj94})
try({pj95<- 0; mod <- Arima (d2, order=c(0,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj95 <- cbind(pj, aic, sd);pj95})
try({pj96<- 0; mod <- Arima (d2, order=c(0,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj96 <- cbind(pj, aic, sd);pj96})
try({pj97<- 0; mod <- Arima (d2, order=c(0,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj97 <- cbind(pj, aic, sd);pj97})
try({pj98<- 0; mod <- Arima (d2, order=c(0,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj98 <- cbind(pj, aic, sd);pj98})
try({pj99<- 0; mod <- Arima (d2, order=c(0,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj99 <- cbind(pj, aic, sd);pj99})
try({pj100<- 0; mod <- Arima (d2, order=c(0,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj100 <- cbind(pj, aic, sd);pj100})
try({pj101<- 0; mod <- Arima (d2, order=c(1,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj101 <- cbind(pj, aic, sd);pj101})
try({pj102<- 0; mod <- Arima (d2, order=c(1,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj102 <- cbind(pj, aic, sd);pj102})
try({pj103<- 0; mod <- Arima (d2, order=c(1,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj103 <- cbind(pj, aic, sd);pj103})
try({pj104<- 0; mod <- Arima (d2, order=c(1,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj104 <- cbind(pj, aic, sd);pj104})
try({pj105<- 0; mod <- Arima (d2, order=c(1,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj105 <- cbind(pj, aic, sd);pj105})
try({pj106<- 0; mod <- Arima (d2, order=c(1,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj106 <- cbind(pj, aic, sd);pj106})
try({pj107<- 0; mod <- Arima (d2, order=c(1,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj107 <- cbind(pj, aic, sd);pj107})
try({pj108<- 0; mod <- Arima (d2, order=c(1,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj108 <- cbind(pj, aic, sd);pj108})
try({pj109<- 0; mod <- Arima (d2, order=c(1,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj109 <- cbind(pj, aic, sd);pj109})
try({pj110<- 0; mod <- Arima (d2, order=c(1,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj110 <- cbind(pj, aic, sd);pj110})
try({pj111<- 0; mod <- Arima (d2, order=c(1,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj111 <- cbind(pj, aic, sd);pj111})
try({pj112<- 0; mod <- Arima (d2, order=c(1,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj112 <- cbind(pj, aic, sd);pj112})
try({pj113<- 0; mod <- Arima (d2, order=c(1,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj113 <- cbind(pj, aic, sd);pj113})
try({pj114<- 0; mod <- Arima (d2, order=c(1,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj114 <- cbind(pj, aic, sd);pj114})
try({pj115<- 0; mod <- Arima (d2, order=c(1,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj115 <- cbind(pj, aic, sd);pj115})
try({pj116<- 0; mod <- Arima (d2, order=c(1,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj116 <- cbind(pj, aic, sd);pj116})
try({pj117<- 0; mod <- Arima (d2, order=c(1,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj117 <- cbind(pj, aic, sd);pj117})
try({pj118<- 0; mod <- Arima (d2, order=c(1,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj118 <- cbind(pj, aic, sd);pj118})
try({pj119<- 0; mod <- Arima (d2, order=c(1,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj119 <- cbind(pj, aic, sd);pj119})
try({pj120<- 0; mod <- Arima (d2, order=c(1,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj120 <- cbind(pj, aic, sd);pj120})
try({pj121<- 0; mod <- Arima (d2, order=c(1,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj121 <- cbind(pj, aic, sd);pj121})
try({pj122<- 0; mod <- Arima (d2, order=c(1,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj122 <- cbind(pj, aic, sd);pj122})
try({pj123<- 0; mod <- Arima (d2, order=c(1,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj123 <- cbind(pj, aic, sd);pj123})
try({pj124<- 0; mod <- Arima (d2, order=c(1,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj124 <- cbind(pj, aic, sd);pj124})
try({pj125<- 0; mod <- Arima (d2, order=c(1,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj125 <- cbind(pj, aic, sd);pj125})
try({pj126<- 0; mod <- Arima (d2, order=c(1,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj126 <- cbind(pj, aic, sd);pj126})
try({pj127<- 0; mod <- Arima (d2, order=c(1,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj127 <- cbind(pj, aic, sd);pj127})
try({pj128<- 0; mod <- Arima (d2, order=c(1,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj128 <- cbind(pj, aic, sd);pj128})
try({pj129<- 0; mod <- Arima (d2, order=c(1,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj129 <- cbind(pj, aic, sd);pj129})
try({pj130<- 0; mod <- Arima (d2, order=c(1,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj130 <- cbind(pj, aic, sd);pj130})
try({pj131<- 0; mod <- Arima (d2, order=c(1,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj131 <- cbind(pj, aic, sd);pj131})
try({pj132<- 0; mod <- Arima (d2, order=c(1,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj132 <- cbind(pj, aic, sd);pj132})
try({pj133<- 0; mod <- Arima (d2, order=c(1,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj133 <- cbind(pj, aic, sd);pj133})
try({pj134<- 0; mod <- Arima (d2, order=c(1,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj134 <- cbind(pj, aic, sd);pj134})
try({pj135<- 0; mod <- Arima (d2, order=c(1,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj135 <- cbind(pj, aic, sd);pj135})
try({pj136<- 0; mod <- Arima (d2, order=c(1,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj136 <- cbind(pj, aic, sd);pj136})
try({pj137<- 0; mod <- Arima (d2, order=c(1,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj137 <- cbind(pj, aic, sd);pj137})
try({pj138<- 0; mod <- Arima (d2, order=c(1,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj138 <- cbind(pj, aic, sd);pj138})
try({pj139<- 0; mod <- Arima (d2, order=c(1,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj139 <- cbind(pj, aic, sd);pj139})
try({pj140<- 0; mod <- Arima (d2, order=c(1,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj140 <- cbind(pj, aic, sd);pj140})
try({pj141<- 0; mod <- Arima (d2, order=c(1,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj141 <- cbind(pj, aic, sd);pj141})
try({pj142<- 0; mod <- Arima (d2, order=c(1,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj142 <- cbind(pj, aic, sd);pj142})
try({pj143<- 0; mod <- Arima (d2, order=c(1,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj143 <- cbind(pj, aic, sd);pj143})
try({pj144<- 0; mod <- Arima (d2, order=c(1,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj144 <- cbind(pj, aic, sd);pj144})
try({pj145<- 0; mod <- Arima (d2, order=c(1,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj145 <- cbind(pj, aic, sd);pj145})
try({pj146<- 0; mod <- Arima (d2, order=c(1,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj146 <- cbind(pj, aic, sd);pj146})
try({pj147<- 0; mod <- Arima (d2, order=c(1,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj147 <- cbind(pj, aic, sd);pj147})
try({pj148<- 0; mod <- Arima (d2, order=c(1,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj148 <- cbind(pj, aic, sd);pj148})
try({pj149<- 0; mod <- Arima (d2, order=c(1,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj149 <- cbind(pj, aic, sd);pj149})
try({pj150<- 0; mod <- Arima (d2, order=c(1,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj150 <- cbind(pj, aic, sd);pj150})
try({pj151<- 0; mod <- Arima (d2, order=c(1,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj151 <- cbind(pj, aic, sd);pj151})
try({pj152<- 0; mod <- Arima (d2, order=c(1,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj152 <- cbind(pj, aic, sd);pj152})
try({pj153<- 0; mod <- Arima (d2, order=c(1,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj153 <- cbind(pj, aic, sd);pj153})
try({pj154<- 0; mod <- Arima (d2, order=c(1,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj154 <- cbind(pj, aic, sd);pj154})
try({pj155<- 0; mod <- Arima (d2, order=c(1,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj155 <- cbind(pj, aic, sd);pj155})
try({pj156<- 0; mod <- Arima (d2, order=c(1,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj156 <- cbind(pj, aic, sd);pj156})
try({pj157<- 0; mod <- Arima (d2, order=c(1,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj157 <- cbind(pj, aic, sd);pj157})
try({pj158<- 0; mod <- Arima (d2, order=c(1,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj158 <- cbind(pj, aic, sd);pj158})
try({pj159<- 0; mod <- Arima (d2, order=c(1,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj159 <- cbind(pj, aic, sd);pj159})
try({pj160<- 0; mod <- Arima (d2, order=c(1,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj160 <- cbind(pj, aic, sd);pj160})
try({pj161<- 0; mod <- Arima (d2, order=c(1,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj161 <- cbind(pj, aic, sd);pj161})
try({pj162<- 0; mod <- Arima (d2, order=c(1,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj162 <- cbind(pj, aic, sd);pj162})
try({pj163<- 0; mod <- Arima (d2, order=c(1,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj163 <- cbind(pj, aic, sd);pj163})
try({pj164<- 0; mod <- Arima (d2, order=c(1,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj164 <- cbind(pj, aic, sd);pj164})
try({pj165<- 0; mod <- Arima (d2, order=c(1,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj165 <- cbind(pj, aic, sd);pj165})
try({pj166<- 0; mod <- Arima (d2, order=c(1,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj166 <- cbind(pj, aic, sd);pj166})
try({pj167<- 0; mod <- Arima (d2, order=c(1,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj167 <- cbind(pj, aic, sd);pj167})
try({pj168<- 0; mod <- Arima (d2, order=c(1,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj168 <- cbind(pj, aic, sd);pj168})
try({pj169<- 0; mod <- Arima (d2, order=c(1,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj169 <- cbind(pj, aic, sd);pj169})
try({pj170<- 0; mod <- Arima (d2, order=c(1,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj170 <- cbind(pj, aic, sd);pj170})
try({pj171<- 0; mod <- Arima (d2, order=c(1,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj171 <- cbind(pj, aic, sd);pj171})
try({pj172<- 0; mod <- Arima (d2, order=c(1,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj172 <- cbind(pj, aic, sd);pj172})
try({pj173<- 0; mod <- Arima (d2, order=c(1,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj173 <- cbind(pj, aic, sd);pj173})
try({pj174<- 0; mod <- Arima (d2, order=c(1,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj174 <- cbind(pj, aic, sd);pj174})
try({pj175<- 0; mod <- Arima (d2, order=c(1,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj175 <- cbind(pj, aic, sd);pj175})
try({pj176<- 0; mod <- Arima (d2, order=c(1,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj176 <- cbind(pj, aic, sd);pj176})
try({pj177<- 0; mod <- Arima (d2, order=c(1,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj177 <- cbind(pj, aic, sd);pj177})
try({pj178<- 0; mod <- Arima (d2, order=c(1,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj178 <- cbind(pj, aic, sd);pj178})
try({pj179<- 0; mod <- Arima (d2, order=c(1,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj179 <- cbind(pj, aic, sd);pj179})
try({pj180<- 0; mod <- Arima (d2, order=c(1,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj180 <- cbind(pj, aic, sd);pj180})
try({pj181<- 0; mod <- Arima (d2, order=c(1,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj181 <- cbind(pj, aic, sd);pj181})
try({pj182<- 0; mod <- Arima (d2, order=c(1,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj182 <- cbind(pj, aic, sd);pj182})
try({pj183<- 0; mod <- Arima (d2, order=c(1,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj183 <- cbind(pj, aic, sd);pj183})
try({pj184<- 0; mod <- Arima (d2, order=c(1,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj184 <- cbind(pj, aic, sd);pj184})
try({pj185<- 0; mod <- Arima (d2, order=c(1,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj185 <- cbind(pj, aic, sd);pj185})
try({pj186<- 0; mod <- Arima (d2, order=c(1,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj186 <- cbind(pj, aic, sd);pj186})
try({pj187<- 0; mod <- Arima (d2, order=c(1,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj187 <- cbind(pj, aic, sd);pj187})
try({pj188<- 0; mod <- Arima (d2, order=c(1,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj188 <- cbind(pj, aic, sd);pj188})
try({pj189<- 0; mod <- Arima (d2, order=c(1,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj189 <- cbind(pj, aic, sd);pj189})
try({pj190<- 0; mod <- Arima (d2, order=c(1,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj190 <- cbind(pj, aic, sd);pj190})
try({pj191<- 0; mod <- Arima (d2, order=c(1,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj191 <- cbind(pj, aic, sd);pj191})
try({pj192<- 0; mod <- Arima (d2, order=c(1,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj192 <- cbind(pj, aic, sd);pj192})
try({pj193<- 0; mod <- Arima (d2, order=c(1,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj193 <- cbind(pj, aic, sd);pj193})
try({pj194<- 0; mod <- Arima (d2, order=c(1,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj194 <- cbind(pj, aic, sd);pj194})
try({pj195<- 0; mod <- Arima (d2, order=c(1,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj195 <- cbind(pj, aic, sd);pj195})
try({pj196<- 0; mod <- Arima (d2, order=c(1,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj196 <- cbind(pj, aic, sd);pj196})
try({pj197<- 0; mod <- Arima (d2, order=c(1,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj197 <- cbind(pj, aic, sd);pj197})
try({pj198<- 0; mod <- Arima (d2, order=c(1,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj198 <- cbind(pj, aic, sd);pj198})
try({pj199<- 0; mod <- Arima (d2, order=c(1,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj199 <- cbind(pj, aic, sd);pj199})
try({pj200<- 0; mod <- Arima (d2, order=c(1,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj200 <- cbind(pj, aic, sd);pj200})
try({pj201<- 0; mod <- Arima (d2, order=c(2,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj201 <- cbind(pj, aic, sd);pj201})
try({pj202<- 0; mod <- Arima (d2, order=c(2,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj202 <- cbind(pj, aic, sd);pj202})
try({pj203<- 0; mod <- Arima (d2, order=c(2,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj203 <- cbind(pj, aic, sd);pj203})
try({pj204<- 0; mod <- Arima (d2, order=c(2,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj204 <- cbind(pj, aic, sd);pj204})
try({pj205<- 0; mod <- Arima (d2, order=c(2,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj205 <- cbind(pj, aic, sd);pj205})
try({pj206<- 0; mod <- Arima (d2, order=c(2,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj206 <- cbind(pj, aic, sd);pj206})
try({pj207<- 0; mod <- Arima (d2, order=c(2,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj207 <- cbind(pj, aic, sd);pj207})
try({pj208<- 0; mod <- Arima (d2, order=c(2,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj208 <- cbind(pj, aic, sd);pj208})
try({pj209<- 0; mod <- Arima (d2, order=c(2,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj209 <- cbind(pj, aic, sd);pj209})
try({pj210<- 0; mod <- Arima (d2, order=c(2,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj210 <- cbind(pj, aic, sd);pj210})
try({pj211<- 0; mod <- Arima (d2, order=c(2,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj211 <- cbind(pj, aic, sd);pj211})
try({pj212<- 0; mod <- Arima (d2, order=c(2,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj212 <- cbind(pj, aic, sd);pj212})
try({pj213<- 0; mod <- Arima (d2, order=c(2,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj213 <- cbind(pj, aic, sd);pj213})
try({pj214<- 0; mod <- Arima (d2, order=c(2,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj214 <- cbind(pj, aic, sd);pj214})
try({pj215<- 0; mod <- Arima (d2, order=c(2,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj215 <- cbind(pj, aic, sd);pj215})
try({pj216<- 0; mod <- Arima (d2, order=c(2,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj216 <- cbind(pj, aic, sd);pj216})
try({pj217<- 0; mod <- Arima (d2, order=c(2,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj217 <- cbind(pj, aic, sd);pj217})
try({pj218<- 0; mod <- Arima (d2, order=c(2,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj218 <- cbind(pj, aic, sd);pj218})
try({pj219<- 0; mod <- Arima (d2, order=c(2,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj219 <- cbind(pj, aic, sd);pj219})
try({pj220<- 0; mod <- Arima (d2, order=c(2,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj220 <- cbind(pj, aic, sd);pj220})
try({pj221<- 0; mod <- Arima (d2, order=c(2,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj221 <- cbind(pj, aic, sd);pj221})
try({pj222<- 0; mod <- Arima (d2, order=c(2,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj222 <- cbind(pj, aic, sd);pj222})
try({pj223<- 0; mod <- Arima (d2, order=c(2,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj223 <- cbind(pj, aic, sd);pj223})
try({pj224<- 0; mod <- Arima (d2, order=c(2,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj224 <- cbind(pj, aic, sd);pj224})
try({pj225<- 0; mod <- Arima (d2, order=c(2,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj225 <- cbind(pj, aic, sd);pj225})
try({pj226<- 0; mod <- Arima (d2, order=c(2,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj226 <- cbind(pj, aic, sd);pj226})
try({pj227<- 0; mod <- Arima (d2, order=c(2,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj227 <- cbind(pj, aic, sd);pj227})
try({pj228<- 0; mod <- Arima (d2, order=c(2,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj228 <- cbind(pj, aic, sd);pj228})
try({pj229<- 0; mod <- Arima (d2, order=c(2,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj229 <- cbind(pj, aic, sd);pj229})
try({pj230<- 0; mod <- Arima (d2, order=c(2,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj230 <- cbind(pj, aic, sd);pj230})
try({pj231<- 0; mod <- Arima (d2, order=c(2,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj231 <- cbind(pj, aic, sd);pj231})
try({pj232<- 0; mod <- Arima (d2, order=c(2,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj232 <- cbind(pj, aic, sd);pj232})
try({pj233<- 0; mod <- Arima (d2, order=c(2,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj233 <- cbind(pj, aic, sd);pj233})
try({pj234<- 0; mod <- Arima (d2, order=c(2,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj234 <- cbind(pj, aic, sd);pj234})
try({pj235<- 0; mod <- Arima (d2, order=c(2,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj235 <- cbind(pj, aic, sd);pj235})
try({pj236<- 0; mod <- Arima (d2, order=c(2,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj236 <- cbind(pj, aic, sd);pj236})
try({pj237<- 0; mod <- Arima (d2, order=c(2,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj237 <- cbind(pj, aic, sd);pj237})
try({pj238<- 0; mod <- Arima (d2, order=c(2,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj238 <- cbind(pj, aic, sd);pj238})
try({pj239<- 0; mod <- Arima (d2, order=c(2,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj239 <- cbind(pj, aic, sd);pj239})
try({pj240<- 0; mod <- Arima (d2, order=c(2,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj240 <- cbind(pj, aic, sd);pj240})
try({pj241<- 0; mod <- Arima (d2, order=c(2,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj241 <- cbind(pj, aic, sd);pj241})
try({pj242<- 0; mod <- Arima (d2, order=c(2,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj242 <- cbind(pj, aic, sd);pj242})
try({pj243<- 0; mod <- Arima (d2, order=c(2,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj243 <- cbind(pj, aic, sd);pj243})
try({pj244<- 0; mod <- Arima (d2, order=c(2,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj244 <- cbind(pj, aic, sd);pj244})
try({pj245<- 0; mod <- Arima (d2, order=c(2,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj245 <- cbind(pj, aic, sd);pj245})
try({pj246<- 0; mod <- Arima (d2, order=c(2,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj246 <- cbind(pj, aic, sd);pj246})
try({pj247<- 0; mod <- Arima (d2, order=c(2,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj247 <- cbind(pj, aic, sd);pj247})
try({pj248<- 0; mod <- Arima (d2, order=c(2,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj248 <- cbind(pj, aic, sd);pj248})
try({pj249<- 0; mod <- Arima (d2, order=c(2,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj249 <- cbind(pj, aic, sd);pj249})
try({pj250<- 0; mod <- Arima (d2, order=c(2,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj250 <- cbind(pj, aic, sd);pj250})
try({pj251<- 0; mod <- Arima (d2, order=c(2,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj251 <- cbind(pj, aic, sd);pj251})
try({pj252<- 0; mod <- Arima (d2, order=c(2,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj252 <- cbind(pj, aic, sd);pj252})
try({pj253<- 0; mod <- Arima (d2, order=c(2,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj253 <- cbind(pj, aic, sd);pj253})
try({pj254<- 0; mod <- Arima (d2, order=c(2,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj254 <- cbind(pj, aic, sd);pj254})
try({pj255<- 0; mod <- Arima (d2, order=c(2,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj255 <- cbind(pj, aic, sd);pj255})
try({pj256<- 0; mod <- Arima (d2, order=c(2,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj256 <- cbind(pj, aic, sd);pj256})
try({pj257<- 0; mod <- Arima (d2, order=c(2,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj257 <- cbind(pj, aic, sd);pj257})
try({pj258<- 0; mod <- Arima (d2, order=c(2,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj258 <- cbind(pj, aic, sd);pj258})
try({pj259<- 0; mod <- Arima (d2, order=c(2,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj259 <- cbind(pj, aic, sd);pj259})
try({pj260<- 0; mod <- Arima (d2, order=c(2,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj260 <- cbind(pj, aic, sd);pj260})
try({pj261<- 0; mod <- Arima (d2, order=c(2,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj261 <- cbind(pj, aic, sd);pj261})
try({pj262<- 0; mod <- Arima (d2, order=c(2,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj262 <- cbind(pj, aic, sd);pj262})
try({pj263<- 0; mod <- Arima (d2, order=c(2,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj263 <- cbind(pj, aic, sd);pj263})
try({pj264<- 0; mod <- Arima (d2, order=c(2,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj264 <- cbind(pj, aic, sd);pj264})
try({pj265<- 0; mod <- Arima (d2, order=c(2,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj265 <- cbind(pj, aic, sd);pj265})
try({pj266<- 0; mod <- Arima (d2, order=c(2,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj266 <- cbind(pj, aic, sd);pj266})
try({pj267<- 0; mod <- Arima (d2, order=c(2,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj267 <- cbind(pj, aic, sd);pj267})
try({pj268<- 0; mod <- Arima (d2, order=c(2,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj268 <- cbind(pj, aic, sd);pj268})
try({pj269<- 0; mod <- Arima (d2, order=c(2,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj269 <- cbind(pj, aic, sd);pj269})
try({pj270<- 0; mod <- Arima (d2, order=c(2,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj270 <- cbind(pj, aic, sd);pj270})
try({pj271<- 0; mod <- Arima (d2, order=c(2,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj271 <- cbind(pj, aic, sd);pj271})
try({pj272<- 0; mod <- Arima (d2, order=c(2,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj272 <- cbind(pj, aic, sd);pj272})
try({pj273<- 0; mod <- Arima (d2, order=c(2,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj273 <- cbind(pj, aic, sd);pj273})
try({pj274<- 0; mod <- Arima (d2, order=c(2,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj274 <- cbind(pj, aic, sd);pj274})
try({pj275<- 0; mod <- Arima (d2, order=c(2,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj275 <- cbind(pj, aic, sd);pj275})
try({pj276<- 0; mod <- Arima (d2, order=c(2,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj276 <- cbind(pj, aic, sd);pj276})
try({pj277<- 0; mod <- Arima (d2, order=c(2,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj277 <- cbind(pj, aic, sd);pj277})
try({pj278<- 0; mod <- Arima (d2, order=c(2,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj278 <- cbind(pj, aic, sd);pj278})
try({pj279<- 0; mod <- Arima (d2, order=c(2,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj279 <- cbind(pj, aic, sd);pj279})
try({pj280<- 0; mod <- Arima (d2, order=c(2,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj280 <- cbind(pj, aic, sd);pj280})
try({pj281<- 0; mod <- Arima (d2, order=c(2,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj281 <- cbind(pj, aic, sd);pj281})
try({pj282<- 0; mod <- Arima (d2, order=c(2,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj282 <- cbind(pj, aic, sd);pj282})
try({pj283<- 0; mod <- Arima (d2, order=c(2,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj283 <- cbind(pj, aic, sd);pj283})
try({pj284<- 0; mod <- Arima (d2, order=c(2,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj284 <- cbind(pj, aic, sd);pj284})
try({pj285<- 0; mod <- Arima (d2, order=c(2,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj285 <- cbind(pj, aic, sd);pj285})
try({pj286<- 0; mod <- Arima (d2, order=c(2,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj286 <- cbind(pj, aic, sd);pj286})
try({pj287<- 0; mod <- Arima (d2, order=c(2,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj287 <- cbind(pj, aic, sd);pj287})
try({pj288<- 0; mod <- Arima (d2, order=c(2,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj288 <- cbind(pj, aic, sd);pj288})
try({pj289<- 0; mod <- Arima (d2, order=c(2,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj289 <- cbind(pj, aic, sd);pj289})
try({pj290<- 0; mod <- Arima (d2, order=c(2,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj290 <- cbind(pj, aic, sd);pj290})
try({pj291<- 0; mod <- Arima (d2, order=c(2,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj291 <- cbind(pj, aic, sd);pj291})
try({pj292<- 0; mod <- Arima (d2, order=c(2,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj292 <- cbind(pj, aic, sd);pj292})
try({pj293<- 0; mod <- Arima (d2, order=c(2,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj293 <- cbind(pj, aic, sd);pj293})
try({pj294<- 0; mod <- Arima (d2, order=c(2,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj294 <- cbind(pj, aic, sd);pj294})
try({pj295<- 0; mod <- Arima (d2, order=c(2,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj295 <- cbind(pj, aic, sd);pj295})
try({pj296<- 0; mod <- Arima (d2, order=c(2,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj296 <- cbind(pj, aic, sd);pj296})
try({pj297<- 0; mod <- Arima (d2, order=c(2,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj297 <- cbind(pj, aic, sd);pj297})
try({pj298<- 0; mod <- Arima (d2, order=c(2,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj298 <- cbind(pj, aic, sd);pj298})
try({pj299<- 0; mod <- Arima (d2, order=c(2,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj299 <- cbind(pj, aic, sd);pj299})
try({pj300<- 0; mod <- Arima (d2, order=c(2,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj300 <- cbind(pj, aic, sd);pj300})
try({pj301<- 0; mod <- Arima (d2, order=c(3,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj301 <- cbind(pj, aic, sd);pj301})
try({pj302<- 0; mod <- Arima (d2, order=c(3,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj302 <- cbind(pj, aic, sd);pj302})
try({pj303<- 0; mod <- Arima (d2, order=c(3,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj303 <- cbind(pj, aic, sd);pj303})
try({pj304<- 0; mod <- Arima (d2, order=c(3,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj304 <- cbind(pj, aic, sd);pj304})
try({pj305<- 0; mod <- Arima (d2, order=c(3,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj305 <- cbind(pj, aic, sd);pj305})
try({pj306<- 0; mod <- Arima (d2, order=c(3,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj306 <- cbind(pj, aic, sd);pj306})
try({pj307<- 0; mod <- Arima (d2, order=c(3,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj307 <- cbind(pj, aic, sd);pj307})
try({pj308<- 0; mod <- Arima (d2, order=c(3,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj308 <- cbind(pj, aic, sd);pj308})
try({pj309<- 0; mod <- Arima (d2, order=c(3,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj309 <- cbind(pj, aic, sd);pj309})
try({pj310<- 0; mod <- Arima (d2, order=c(3,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj310 <- cbind(pj, aic, sd);pj310})
try({pj311<- 0; mod <- Arima (d2, order=c(3,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj311 <- cbind(pj, aic, sd);pj311})
try({pj312<- 0; mod <- Arima (d2, order=c(3,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj312 <- cbind(pj, aic, sd);pj312})
try({pj313<- 0; mod <- Arima (d2, order=c(3,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj313 <- cbind(pj, aic, sd);pj313})
try({pj314<- 0; mod <- Arima (d2, order=c(3,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj314 <- cbind(pj, aic, sd);pj314})
try({pj315<- 0; mod <- Arima (d2, order=c(3,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj315 <- cbind(pj, aic, sd);pj315})
try({pj316<- 0; mod <- Arima (d2, order=c(3,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj316 <- cbind(pj, aic, sd);pj316})
try({pj317<- 0; mod <- Arima (d2, order=c(3,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj317 <- cbind(pj, aic, sd);pj317})
try({pj318<- 0; mod <- Arima (d2, order=c(3,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj318 <- cbind(pj, aic, sd);pj318})
try({pj319<- 0; mod <- Arima (d2, order=c(3,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj319 <- cbind(pj, aic, sd);pj319})
try({pj320<- 0; mod <- Arima (d2, order=c(3,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj320 <- cbind(pj, aic, sd);pj320})
try({pj321<- 0; mod <- Arima (d2, order=c(3,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj321 <- cbind(pj, aic, sd);pj321})
try({pj322<- 0; mod <- Arima (d2, order=c(3,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj322 <- cbind(pj, aic, sd);pj322})
try({pj323<- 0; mod <- Arima (d2, order=c(3,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj323 <- cbind(pj, aic, sd);pj323})
try({pj324<- 0; mod <- Arima (d2, order=c(3,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj324 <- cbind(pj, aic, sd);pj324})
try({pj325<- 0; mod <- Arima (d2, order=c(3,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj325 <- cbind(pj, aic, sd);pj325})
try({pj326<- 0; mod <- Arima (d2, order=c(3,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj326 <- cbind(pj, aic, sd);pj326})
try({pj327<- 0; mod <- Arima (d2, order=c(3,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj327 <- cbind(pj, aic, sd);pj327})
try({pj328<- 0; mod <- Arima (d2, order=c(3,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj328 <- cbind(pj, aic, sd);pj328})
try({pj329<- 0; mod <- Arima (d2, order=c(3,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj329 <- cbind(pj, aic, sd);pj329})
try({pj330<- 0; mod <- Arima (d2, order=c(3,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj330 <- cbind(pj, aic, sd);pj330})
try({pj331<- 0; mod <- Arima (d2, order=c(3,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj331 <- cbind(pj, aic, sd);pj331})
try({pj332<- 0; mod <- Arima (d2, order=c(3,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj332 <- cbind(pj, aic, sd);pj332})
try({pj333<- 0; mod <- Arima (d2, order=c(3,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj333 <- cbind(pj, aic, sd);pj333})
try({pj334<- 0; mod <- Arima (d2, order=c(3,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj334 <- cbind(pj, aic, sd);pj334})
try({pj335<- 0; mod <- Arima (d2, order=c(3,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj335 <- cbind(pj, aic, sd);pj335})
try({pj336<- 0; mod <- Arima (d2, order=c(3,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj336 <- cbind(pj, aic, sd);pj336})
try({pj337<- 0; mod <- Arima (d2, order=c(3,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj337 <- cbind(pj, aic, sd);pj337})
try({pj338<- 0; mod <- Arima (d2, order=c(3,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj338 <- cbind(pj, aic, sd);pj338})
try({pj339<- 0; mod <- Arima (d2, order=c(3,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj339 <- cbind(pj, aic, sd);pj339})
try({pj340<- 0; mod <- Arima (d2, order=c(3,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj340 <- cbind(pj, aic, sd);pj340})
try({pj341<- 0; mod <- Arima (d2, order=c(3,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj341 <- cbind(pj, aic, sd);pj341})
try({pj342<- 0; mod <- Arima (d2, order=c(3,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj342 <- cbind(pj, aic, sd);pj342})
try({pj343<- 0; mod <- Arima (d2, order=c(3,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj343 <- cbind(pj, aic, sd);pj343})
try({pj344<- 0; mod <- Arima (d2, order=c(3,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj344 <- cbind(pj, aic, sd);pj344})
try({pj345<- 0; mod <- Arima (d2, order=c(3,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj345 <- cbind(pj, aic, sd);pj345})
try({pj346<- 0; mod <- Arima (d2, order=c(3,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj346 <- cbind(pj, aic, sd);pj346})
try({pj347<- 0; mod <- Arima (d2, order=c(3,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj347 <- cbind(pj, aic, sd);pj347})
try({pj348<- 0; mod <- Arima (d2, order=c(3,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj348 <- cbind(pj, aic, sd);pj348})
try({pj349<- 0; mod <- Arima (d2, order=c(3,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj349 <- cbind(pj, aic, sd);pj349})
try({pj350<- 0; mod <- Arima (d2, order=c(3,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj350 <- cbind(pj, aic, sd);pj350})
try({pj351<- 0; mod <- Arima (d2, order=c(3,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj351 <- cbind(pj, aic, sd);pj351})
try({pj352<- 0; mod <- Arima (d2, order=c(3,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj352 <- cbind(pj, aic, sd);pj352})
try({pj353<- 0; mod <- Arima (d2, order=c(3,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj353 <- cbind(pj, aic, sd);pj353})
try({pj354<- 0; mod <- Arima (d2, order=c(3,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj354 <- cbind(pj, aic, sd);pj354})
try({pj355<- 0; mod <- Arima (d2, order=c(3,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj355 <- cbind(pj, aic, sd);pj355})
try({pj356<- 0; mod <- Arima (d2, order=c(3,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj356 <- cbind(pj, aic, sd);pj356})
try({pj357<- 0; mod <- Arima (d2, order=c(3,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj357 <- cbind(pj, aic, sd);pj357})
try({pj358<- 0; mod <- Arima (d2, order=c(3,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj358 <- cbind(pj, aic, sd);pj358})
try({pj359<- 0; mod <- Arima (d2, order=c(3,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj359 <- cbind(pj, aic, sd);pj359})
try({pj360<- 0; mod <- Arima (d2, order=c(3,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj360 <- cbind(pj, aic, sd);pj360})
try({pj361<- 0; mod <- Arima (d2, order=c(3,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj361 <- cbind(pj, aic, sd);pj361})
try({pj362<- 0; mod <- Arima (d2, order=c(3,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj362 <- cbind(pj, aic, sd);pj362})
try({pj363<- 0; mod <- Arima (d2, order=c(3,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj363 <- cbind(pj, aic, sd);pj363})
try({pj364<- 0; mod <- Arima (d2, order=c(3,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj364 <- cbind(pj, aic, sd);pj364})
try({pj365<- 0; mod <- Arima (d2, order=c(3,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj365 <- cbind(pj, aic, sd);pj365})
try({pj366<- 0; mod <- Arima (d2, order=c(3,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj366 <- cbind(pj, aic, sd);pj366})
try({pj367<- 0; mod <- Arima (d2, order=c(3,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj367 <- cbind(pj, aic, sd);pj367})
try({pj368<- 0; mod <- Arima (d2, order=c(3,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj368 <- cbind(pj, aic, sd);pj368})
try({pj369<- 0; mod <- Arima (d2, order=c(3,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj369 <- cbind(pj, aic, sd);pj369})
try({pj370<- 0; mod <- Arima (d2, order=c(3,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj370 <- cbind(pj, aic, sd);pj370})
try({pj371<- 0; mod <- Arima (d2, order=c(3,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj371 <- cbind(pj, aic, sd);pj371})
try({pj372<- 0; mod <- Arima (d2, order=c(3,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj372 <- cbind(pj, aic, sd);pj372})
try({pj373<- 0; mod <- Arima (d2, order=c(3,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj373 <- cbind(pj, aic, sd);pj373})
try({pj374<- 0; mod <- Arima (d2, order=c(3,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj374 <- cbind(pj, aic, sd);pj374})
try({pj375<- 0; mod <- Arima (d2, order=c(3,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj375 <- cbind(pj, aic, sd);pj375})
try({pj376<- 0; mod <- Arima (d2, order=c(3,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj376 <- cbind(pj, aic, sd);pj376})
try({pj377<- 0; mod <- Arima (d2, order=c(3,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj377 <- cbind(pj, aic, sd);pj377})
try({pj378<- 0; mod <- Arima (d2, order=c(3,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj378 <- cbind(pj, aic, sd);pj378})
try({pj379<- 0; mod <- Arima (d2, order=c(3,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj379 <- cbind(pj, aic, sd);pj379})
try({pj380<- 0; mod <- Arima (d2, order=c(3,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj380 <- cbind(pj, aic, sd);pj380})
try({pj381<- 0; mod <- Arima (d2, order=c(3,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj381 <- cbind(pj, aic, sd);pj381})
try({pj382<- 0; mod <- Arima (d2, order=c(3,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj382 <- cbind(pj, aic, sd);pj382})
try({pj383<- 0; mod <- Arima (d2, order=c(3,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj383 <- cbind(pj, aic, sd);pj383})
try({pj384<- 0; mod <- Arima (d2, order=c(3,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj384 <- cbind(pj, aic, sd);pj384})
try({pj385<- 0; mod <- Arima (d2, order=c(3,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj385 <- cbind(pj, aic, sd);pj385})
try({pj386<- 0; mod <- Arima (d2, order=c(3,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj386 <- cbind(pj, aic, sd);pj386})
try({pj387<- 0; mod <- Arima (d2, order=c(3,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj387 <- cbind(pj, aic, sd);pj387})
try({pj388<- 0; mod <- Arima (d2, order=c(3,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj388 <- cbind(pj, aic, sd);pj388})
try({pj389<- 0; mod <- Arima (d2, order=c(3,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj389 <- cbind(pj, aic, sd);pj389})
try({pj390<- 0; mod <- Arima (d2, order=c(3,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj390 <- cbind(pj, aic, sd);pj390})
try({pj391<- 0; mod <- Arima (d2, order=c(3,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj391 <- cbind(pj, aic, sd);pj391})
try({pj392<- 0; mod <- Arima (d2, order=c(3,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj392 <- cbind(pj, aic, sd);pj392})
try({pj393<- 0; mod <- Arima (d2, order=c(3,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj393 <- cbind(pj, aic, sd);pj393})
try({pj394<- 0; mod <- Arima (d2, order=c(3,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj394 <- cbind(pj, aic, sd);pj394})
try({pj395<- 0; mod <- Arima (d2, order=c(3,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj395 <- cbind(pj, aic, sd);pj395})
try({pj396<- 0; mod <- Arima (d2, order=c(3,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj396 <- cbind(pj, aic, sd);pj396})
try({pj397<- 0; mod <- Arima (d2, order=c(3,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj397 <- cbind(pj, aic, sd);pj397})
try({pj398<- 0; mod <- Arima (d2, order=c(3,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj398 <- cbind(pj, aic, sd);pj398})
try({pj399<- 0; mod <- Arima (d2, order=c(3,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj399 <- cbind(pj, aic, sd);pj399})
try({pj400<- 0; mod <- Arima (d2, order=c(3,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj400 <- cbind(pj, aic, sd);pj400})
try({pj401<- 0; mod <- Arima (d2, order=c(4,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj401 <- cbind(pj, aic, sd);pj401})
try({pj402<- 0; mod <- Arima (d2, order=c(4,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj402 <- cbind(pj, aic, sd);pj402})
try({pj403<- 0; mod <- Arima (d2, order=c(4,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj403 <- cbind(pj, aic, sd);pj403})
try({pj404<- 0; mod <- Arima (d2, order=c(4,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj404 <- cbind(pj, aic, sd);pj404})
try({pj405<- 0; mod <- Arima (d2, order=c(4,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj405 <- cbind(pj, aic, sd);pj405})
try({pj406<- 0; mod <- Arima (d2, order=c(4,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj406 <- cbind(pj, aic, sd);pj406})
try({pj407<- 0; mod <- Arima (d2, order=c(4,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj407 <- cbind(pj, aic, sd);pj407})
try({pj408<- 0; mod <- Arima (d2, order=c(4,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj408 <- cbind(pj, aic, sd);pj408})
try({pj409<- 0; mod <- Arima (d2, order=c(4,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj409 <- cbind(pj, aic, sd);pj409})
try({pj410<- 0; mod <- Arima (d2, order=c(4,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj410 <- cbind(pj, aic, sd);pj410})
try({pj411<- 0; mod <- Arima (d2, order=c(4,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj411 <- cbind(pj, aic, sd);pj411})
try({pj412<- 0; mod <- Arima (d2, order=c(4,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj412 <- cbind(pj, aic, sd);pj412})
try({pj413<- 0; mod <- Arima (d2, order=c(4,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj413 <- cbind(pj, aic, sd);pj413})
try({pj414<- 0; mod <- Arima (d2, order=c(4,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj414 <- cbind(pj, aic, sd);pj414})
try({pj415<- 0; mod <- Arima (d2, order=c(4,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj415 <- cbind(pj, aic, sd);pj415})
try({pj416<- 0; mod <- Arima (d2, order=c(4,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj416 <- cbind(pj, aic, sd);pj416})
try({pj417<- 0; mod <- Arima (d2, order=c(4,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj417 <- cbind(pj, aic, sd);pj417})
try({pj418<- 0; mod <- Arima (d2, order=c(4,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj418 <- cbind(pj, aic, sd);pj418})
try({pj419<- 0; mod <- Arima (d2, order=c(4,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj419 <- cbind(pj, aic, sd);pj419})
try({pj420<- 0; mod <- Arima (d2, order=c(4,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj420 <- cbind(pj, aic, sd);pj420})
try({pj421<- 0; mod <- Arima (d2, order=c(4,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj421 <- cbind(pj, aic, sd);pj421})
try({pj422<- 0; mod <- Arima (d2, order=c(4,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj422 <- cbind(pj, aic, sd);pj422})
try({pj423<- 0; mod <- Arima (d2, order=c(4,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj423 <- cbind(pj, aic, sd);pj423})
try({pj424<- 0; mod <- Arima (d2, order=c(4,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj424 <- cbind(pj, aic, sd);pj424})
try({pj425<- 0; mod <- Arima (d2, order=c(4,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj425 <- cbind(pj, aic, sd);pj425})
try({pj426<- 0; mod <- Arima (d2, order=c(4,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj426 <- cbind(pj, aic, sd);pj426})
try({pj427<- 0; mod <- Arima (d2, order=c(4,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj427 <- cbind(pj, aic, sd);pj427})
try({pj428<- 0; mod <- Arima (d2, order=c(4,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj428 <- cbind(pj, aic, sd);pj428})
try({pj429<- 0; mod <- Arima (d2, order=c(4,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj429 <- cbind(pj, aic, sd);pj429})
try({pj430<- 0; mod <- Arima (d2, order=c(4,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj430 <- cbind(pj, aic, sd);pj430})
try({pj431<- 0; mod <- Arima (d2, order=c(4,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj431 <- cbind(pj, aic, sd);pj431})
try({pj432<- 0; mod <- Arima (d2, order=c(4,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj432 <- cbind(pj, aic, sd);pj432})
try({pj433<- 0; mod <- Arima (d2, order=c(4,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj433 <- cbind(pj, aic, sd);pj433})
try({pj434<- 0; mod <- Arima (d2, order=c(4,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj434 <- cbind(pj, aic, sd);pj434})
try({pj435<- 0; mod <- Arima (d2, order=c(4,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj435 <- cbind(pj, aic, sd);pj435})
try({pj436<- 0; mod <- Arima (d2, order=c(4,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj436 <- cbind(pj, aic, sd);pj436})
try({pj437<- 0; mod <- Arima (d2, order=c(4,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj437 <- cbind(pj, aic, sd);pj437})
try({pj438<- 0; mod <- Arima (d2, order=c(4,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj438 <- cbind(pj, aic, sd);pj438})
try({pj439<- 0; mod <- Arima (d2, order=c(4,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj439 <- cbind(pj, aic, sd);pj439})
try({pj440<- 0; mod <- Arima (d2, order=c(4,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj440 <- cbind(pj, aic, sd);pj440})
try({pj441<- 0; mod <- Arima (d2, order=c(4,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj441 <- cbind(pj, aic, sd);pj441})
try({pj442<- 0; mod <- Arima (d2, order=c(4,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj442 <- cbind(pj, aic, sd);pj442})
try({pj443<- 0; mod <- Arima (d2, order=c(4,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj443 <- cbind(pj, aic, sd);pj443})
try({pj444<- 0; mod <- Arima (d2, order=c(4,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj444 <- cbind(pj, aic, sd);pj444})
try({pj445<- 0; mod <- Arima (d2, order=c(4,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj445 <- cbind(pj, aic, sd);pj445})
try({pj446<- 0; mod <- Arima (d2, order=c(4,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj446 <- cbind(pj, aic, sd);pj446})
try({pj447<- 0; mod <- Arima (d2, order=c(4,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj447 <- cbind(pj, aic, sd);pj447})
try({pj448<- 0; mod <- Arima (d2, order=c(4,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj448 <- cbind(pj, aic, sd);pj448})
try({pj449<- 0; mod <- Arima (d2, order=c(4,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj449 <- cbind(pj, aic, sd);pj449})
try({pj450<- 0; mod <- Arima (d2, order=c(4,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj450 <- cbind(pj, aic, sd);pj450})
try({pj451<- 0; mod <- Arima (d2, order=c(4,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj451 <- cbind(pj, aic, sd);pj451})
try({pj452<- 0; mod <- Arima (d2, order=c(4,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj452 <- cbind(pj, aic, sd);pj452})
try({pj453<- 0; mod <- Arima (d2, order=c(4,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj453 <- cbind(pj, aic, sd);pj453})
try({pj454<- 0; mod <- Arima (d2, order=c(4,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj454 <- cbind(pj, aic, sd);pj454})
try({pj455<- 0; mod <- Arima (d2, order=c(4,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj455 <- cbind(pj, aic, sd);pj455})
try({pj456<- 0; mod <- Arima (d2, order=c(4,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj456 <- cbind(pj, aic, sd);pj456})
try({pj457<- 0; mod <- Arima (d2, order=c(4,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj457 <- cbind(pj, aic, sd);pj457})
try({pj458<- 0; mod <- Arima (d2, order=c(4,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj458 <- cbind(pj, aic, sd);pj458})
try({pj459<- 0; mod <- Arima (d2, order=c(4,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj459 <- cbind(pj, aic, sd);pj459})
try({pj460<- 0; mod <- Arima (d2, order=c(4,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj460 <- cbind(pj, aic, sd);pj460})
try({pj461<- 0; mod <- Arima (d2, order=c(4,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj461 <- cbind(pj, aic, sd);pj461})
try({pj462<- 0; mod <- Arima (d2, order=c(4,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj462 <- cbind(pj, aic, sd);pj462})
try({pj463<- 0; mod <- Arima (d2, order=c(4,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj463 <- cbind(pj, aic, sd);pj463})
try({pj464<- 0; mod <- Arima (d2, order=c(4,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj464 <- cbind(pj, aic, sd);pj464})
try({pj465<- 0; mod <- Arima (d2, order=c(4,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj465 <- cbind(pj, aic, sd);pj465})
try({pj466<- 0; mod <- Arima (d2, order=c(4,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj466 <- cbind(pj, aic, sd);pj466})
try({pj467<- 0; mod <- Arima (d2, order=c(4,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj467 <- cbind(pj, aic, sd);pj467})
try({pj468<- 0; mod <- Arima (d2, order=c(4,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj468 <- cbind(pj, aic, sd);pj468})
try({pj469<- 0; mod <- Arima (d2, order=c(4,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj469 <- cbind(pj, aic, sd);pj469})
try({pj470<- 0; mod <- Arima (d2, order=c(4,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj470 <- cbind(pj, aic, sd);pj470})
try({pj471<- 0; mod <- Arima (d2, order=c(4,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj471 <- cbind(pj, aic, sd);pj471})
try({pj472<- 0; mod <- Arima (d2, order=c(4,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj472 <- cbind(pj, aic, sd);pj472})
try({pj473<- 0; mod <- Arima (d2, order=c(4,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj473 <- cbind(pj, aic, sd);pj473})
try({pj474<- 0; mod <- Arima (d2, order=c(4,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj474 <- cbind(pj, aic, sd);pj474})
try({pj475<- 0; mod <- Arima (d2, order=c(4,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj475 <- cbind(pj, aic, sd);pj475})
try({pj476<- 0; mod <- Arima (d2, order=c(4,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj476 <- cbind(pj, aic, sd);pj476})
try({pj477<- 0; mod <- Arima (d2, order=c(4,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj477 <- cbind(pj, aic, sd);pj477})
try({pj478<- 0; mod <- Arima (d2, order=c(4,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj478 <- cbind(pj, aic, sd);pj478})
try({pj479<- 0; mod <- Arima (d2, order=c(4,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj479 <- cbind(pj, aic, sd);pj479})
try({pj480<- 0; mod <- Arima (d2, order=c(4,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj480 <- cbind(pj, aic, sd);pj480})
try({pj481<- 0; mod <- Arima (d2, order=c(4,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj481 <- cbind(pj, aic, sd);pj481})
try({pj482<- 0; mod <- Arima (d2, order=c(4,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj482 <- cbind(pj, aic, sd);pj482})
try({pj483<- 0; mod <- Arima (d2, order=c(4,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj483 <- cbind(pj, aic, sd);pj483})
try({pj484<- 0; mod <- Arima (d2, order=c(4,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj484 <- cbind(pj, aic, sd);pj484})
try({pj485<- 0; mod <- Arima (d2, order=c(4,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj485 <- cbind(pj, aic, sd);pj485})
try({pj486<- 0; mod <- Arima (d2, order=c(4,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj486 <- cbind(pj, aic, sd);pj486})
try({pj487<- 0; mod <- Arima (d2, order=c(4,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj487 <- cbind(pj, aic, sd);pj487})
try({pj488<- 0; mod <- Arima (d2, order=c(4,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj488 <- cbind(pj, aic, sd);pj488})
try({pj489<- 0; mod <- Arima (d2, order=c(4,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj489 <- cbind(pj, aic, sd);pj489})
try({pj490<- 0; mod <- Arima (d2, order=c(4,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj490 <- cbind(pj, aic, sd);pj490})
try({pj491<- 0; mod <- Arima (d2, order=c(4,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj491 <- cbind(pj, aic, sd);pj491})
try({pj492<- 0; mod <- Arima (d2, order=c(4,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj492 <- cbind(pj, aic, sd);pj492})
try({pj493<- 0; mod <- Arima (d2, order=c(4,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj493 <- cbind(pj, aic, sd);pj493})
try({pj494<- 0; mod <- Arima (d2, order=c(4,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj494 <- cbind(pj, aic, sd);pj494})
try({pj495<- 0; mod <- Arima (d2, order=c(4,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj495 <- cbind(pj, aic, sd);pj495})
try({pj496<- 0; mod <- Arima (d2, order=c(4,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj496 <- cbind(pj, aic, sd);pj496})
try({pj497<- 0; mod <- Arima (d2, order=c(4,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj497 <- cbind(pj, aic, sd);pj497})
try({pj498<- 0; mod <- Arima (d2, order=c(4,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj498 <- cbind(pj, aic, sd);pj498})
try({pj499<- 0; mod <- Arima (d2, order=c(4,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj499 <- cbind(pj, aic, sd);pj499})
try({pj500<- 0; mod <- Arima (d2, order=c(4,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj500 <- cbind(pj, aic, sd);pj500})
try({pj501<- 0; mod <- Arima (d2, order=c(5,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj501 <- cbind(pj, aic, sd);pj501})
try({pj502<- 0; mod <- Arima (d2, order=c(5,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj502 <- cbind(pj, aic, sd);pj502})
try({pj503<- 0; mod <- Arima (d2, order=c(5,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj503 <- cbind(pj, aic, sd);pj503})
try({pj504<- 0; mod <- Arima (d2, order=c(5,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj504 <- cbind(pj, aic, sd);pj504})
try({pj505<- 0; mod <- Arima (d2, order=c(5,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj505 <- cbind(pj, aic, sd);pj505})
try({pj506<- 0; mod <- Arima (d2, order=c(5,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj506 <- cbind(pj, aic, sd);pj506})
try({pj507<- 0; mod <- Arima (d2, order=c(5,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj507 <- cbind(pj, aic, sd);pj507})
try({pj508<- 0; mod <- Arima (d2, order=c(5,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj508 <- cbind(pj, aic, sd);pj508})
try({pj509<- 0; mod <- Arima (d2, order=c(5,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj509 <- cbind(pj, aic, sd);pj509})
try({pj510<- 0; mod <- Arima (d2, order=c(5,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj510 <- cbind(pj, aic, sd);pj510})
try({pj511<- 0; mod <- Arima (d2, order=c(5,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj511 <- cbind(pj, aic, sd);pj511})
try({pj512<- 0; mod <- Arima (d2, order=c(5,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj512 <- cbind(pj, aic, sd);pj512})
try({pj513<- 0; mod <- Arima (d2, order=c(5,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj513 <- cbind(pj, aic, sd);pj513})
try({pj514<- 0; mod <- Arima (d2, order=c(5,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj514 <- cbind(pj, aic, sd);pj514})
try({pj515<- 0; mod <- Arima (d2, order=c(5,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj515 <- cbind(pj, aic, sd);pj515})
try({pj516<- 0; mod <- Arima (d2, order=c(5,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj516 <- cbind(pj, aic, sd);pj516})
try({pj517<- 0; mod <- Arima (d2, order=c(5,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj517 <- cbind(pj, aic, sd);pj517})
try({pj518<- 0; mod <- Arima (d2, order=c(5,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj518 <- cbind(pj, aic, sd);pj518})
try({pj519<- 0; mod <- Arima (d2, order=c(5,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj519 <- cbind(pj, aic, sd);pj519})
try({pj520<- 0; mod <- Arima (d2, order=c(5,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj520 <- cbind(pj, aic, sd);pj520})
try({pj521<- 0; mod <- Arima (d2, order=c(5,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj521 <- cbind(pj, aic, sd);pj521})
try({pj522<- 0; mod <- Arima (d2, order=c(5,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj522 <- cbind(pj, aic, sd);pj522})
try({pj523<- 0; mod <- Arima (d2, order=c(5,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj523 <- cbind(pj, aic, sd);pj523})
try({pj524<- 0; mod <- Arima (d2, order=c(5,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj524 <- cbind(pj, aic, sd);pj524})
try({pj525<- 0; mod <- Arima (d2, order=c(5,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj525 <- cbind(pj, aic, sd);pj525})
try({pj526<- 0; mod <- Arima (d2, order=c(5,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj526 <- cbind(pj, aic, sd);pj526})
try({pj527<- 0; mod <- Arima (d2, order=c(5,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj527 <- cbind(pj, aic, sd);pj527})
try({pj528<- 0; mod <- Arima (d2, order=c(5,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj528 <- cbind(pj, aic, sd);pj528})
try({pj529<- 0; mod <- Arima (d2, order=c(5,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj529 <- cbind(pj, aic, sd);pj529})
try({pj530<- 0; mod <- Arima (d2, order=c(5,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj530 <- cbind(pj, aic, sd);pj530})
try({pj531<- 0; mod <- Arima (d2, order=c(5,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj531 <- cbind(pj, aic, sd);pj531})
try({pj532<- 0; mod <- Arima (d2, order=c(5,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj532 <- cbind(pj, aic, sd);pj532})
try({pj533<- 0; mod <- Arima (d2, order=c(5,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj533 <- cbind(pj, aic, sd);pj533})
try({pj534<- 0; mod <- Arima (d2, order=c(5,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj534 <- cbind(pj, aic, sd);pj534})
try({pj535<- 0; mod <- Arima (d2, order=c(5,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj535 <- cbind(pj, aic, sd);pj535})
try({pj536<- 0; mod <- Arima (d2, order=c(5,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj536 <- cbind(pj, aic, sd);pj536})
try({pj537<- 0; mod <- Arima (d2, order=c(5,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj537 <- cbind(pj, aic, sd);pj537})
try({pj538<- 0; mod <- Arima (d2, order=c(5,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj538 <- cbind(pj, aic, sd);pj538})
try({pj539<- 0; mod <- Arima (d2, order=c(5,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj539 <- cbind(pj, aic, sd);pj539})
try({pj540<- 0; mod <- Arima (d2, order=c(5,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj540 <- cbind(pj, aic, sd);pj540})
try({pj541<- 0; mod <- Arima (d2, order=c(5,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj541 <- cbind(pj, aic, sd);pj541})
try({pj542<- 0; mod <- Arima (d2, order=c(5,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj542 <- cbind(pj, aic, sd);pj542})
try({pj543<- 0; mod <- Arima (d2, order=c(5,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj543 <- cbind(pj, aic, sd);pj543})
try({pj544<- 0; mod <- Arima (d2, order=c(5,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj544 <- cbind(pj, aic, sd);pj544})
try({pj545<- 0; mod <- Arima (d2, order=c(5,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj545 <- cbind(pj, aic, sd);pj545})
try({pj546<- 0; mod <- Arima (d2, order=c(5,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj546 <- cbind(pj, aic, sd);pj546})
try({pj547<- 0; mod <- Arima (d2, order=c(5,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj547 <- cbind(pj, aic, sd);pj547})
try({pj548<- 0; mod <- Arima (d2, order=c(5,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj548 <- cbind(pj, aic, sd);pj548})
try({pj549<- 0; mod <- Arima (d2, order=c(5,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj549 <- cbind(pj, aic, sd);pj549})
try({pj550<- 0; mod <- Arima (d2, order=c(5,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj550 <- cbind(pj, aic, sd);pj550})
try({pj551<- 0; mod <- Arima (d2, order=c(5,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj551 <- cbind(pj, aic, sd);pj551})
try({pj552<- 0; mod <- Arima (d2, order=c(5,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj552 <- cbind(pj, aic, sd);pj552})
try({pj553<- 0; mod <- Arima (d2, order=c(5,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj553 <- cbind(pj, aic, sd);pj553})
try({pj554<- 0; mod <- Arima (d2, order=c(5,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj554 <- cbind(pj, aic, sd);pj554})
try({pj555<- 0; mod <- Arima (d2, order=c(5,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj555 <- cbind(pj, aic, sd);pj555})
try({pj556<- 0; mod <- Arima (d2, order=c(5,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj556 <- cbind(pj, aic, sd);pj556})
try({pj557<- 0; mod <- Arima (d2, order=c(5,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj557 <- cbind(pj, aic, sd);pj557})
try({pj558<- 0; mod <- Arima (d2, order=c(5,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj558 <- cbind(pj, aic, sd);pj558})
try({pj559<- 0; mod <- Arima (d2, order=c(5,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj559 <- cbind(pj, aic, sd);pj559})
try({pj560<- 0; mod <- Arima (d2, order=c(5,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj560 <- cbind(pj, aic, sd);pj560})
try({pj561<- 0; mod <- Arima (d2, order=c(5,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj561 <- cbind(pj, aic, sd);pj561})
try({pj562<- 0; mod <- Arima (d2, order=c(5,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj562 <- cbind(pj, aic, sd);pj562})
try({pj563<- 0; mod <- Arima (d2, order=c(5,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj563 <- cbind(pj, aic, sd);pj563})
try({pj564<- 0; mod <- Arima (d2, order=c(5,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj564 <- cbind(pj, aic, sd);pj564})
try({pj565<- 0; mod <- Arima (d2, order=c(5,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj565 <- cbind(pj, aic, sd);pj565})
try({pj566<- 0; mod <- Arima (d2, order=c(5,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj566 <- cbind(pj, aic, sd);pj566})
try({pj567<- 0; mod <- Arima (d2, order=c(5,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj567 <- cbind(pj, aic, sd);pj567})
try({pj568<- 0; mod <- Arima (d2, order=c(5,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj568 <- cbind(pj, aic, sd);pj568})
try({pj569<- 0; mod <- Arima (d2, order=c(5,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj569 <- cbind(pj, aic, sd);pj569})
try({pj570<- 0; mod <- Arima (d2, order=c(5,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj570 <- cbind(pj, aic, sd);pj570})
try({pj571<- 0; mod <- Arima (d2, order=c(5,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj571 <- cbind(pj, aic, sd);pj571})
try({pj572<- 0; mod <- Arima (d2, order=c(5,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj572 <- cbind(pj, aic, sd);pj572})
try({pj573<- 0; mod <- Arima (d2, order=c(5,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj573 <- cbind(pj, aic, sd);pj573})
try({pj574<- 0; mod <- Arima (d2, order=c(5,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj574 <- cbind(pj, aic, sd);pj574})
try({pj575<- 0; mod <- Arima (d2, order=c(5,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj575 <- cbind(pj, aic, sd);pj575})
try({pj576<- 0; mod <- Arima (d2, order=c(5,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj576 <- cbind(pj, aic, sd);pj576})
try({pj577<- 0; mod <- Arima (d2, order=c(5,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj577 <- cbind(pj, aic, sd);pj577})
try({pj578<- 0; mod <- Arima (d2, order=c(5,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj578 <- cbind(pj, aic, sd);pj578})
try({pj579<- 0; mod <- Arima (d2, order=c(5,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj579 <- cbind(pj, aic, sd);pj579})
try({pj580<- 0; mod <- Arima (d2, order=c(5,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj580 <- cbind(pj, aic, sd);pj580})
try({pj581<- 0; mod <- Arima (d2, order=c(5,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj581 <- cbind(pj, aic, sd);pj581})
try({pj582<- 0; mod <- Arima (d2, order=c(5,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj582 <- cbind(pj, aic, sd);pj582})
try({pj583<- 0; mod <- Arima (d2, order=c(5,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj583 <- cbind(pj, aic, sd);pj583})
try({pj584<- 0; mod <- Arima (d2, order=c(5,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj584 <- cbind(pj, aic, sd);pj584})
try({pj585<- 0; mod <- Arima (d2, order=c(5,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj585 <- cbind(pj, aic, sd);pj585})
try({pj586<- 0; mod <- Arima (d2, order=c(5,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj586 <- cbind(pj, aic, sd);pj586})
try({pj587<- 0; mod <- Arima (d2, order=c(5,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj587 <- cbind(pj, aic, sd);pj587})
try({pj588<- 0; mod <- Arima (d2, order=c(5,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj588 <- cbind(pj, aic, sd);pj588})
try({pj589<- 0; mod <- Arima (d2, order=c(5,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj589 <- cbind(pj, aic, sd);pj589})
try({pj590<- 0; mod <- Arima (d2, order=c(5,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj590 <- cbind(pj, aic, sd);pj590})
try({pj591<- 0; mod <- Arima (d2, order=c(5,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj591 <- cbind(pj, aic, sd);pj591})
try({pj592<- 0; mod <- Arima (d2, order=c(5,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj592 <- cbind(pj, aic, sd);pj592})
try({pj593<- 0; mod <- Arima (d2, order=c(5,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj593 <- cbind(pj, aic, sd);pj593})
try({pj594<- 0; mod <- Arima (d2, order=c(5,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj594 <- cbind(pj, aic, sd);pj594})
try({pj595<- 0; mod <- Arima (d2, order=c(5,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj595 <- cbind(pj, aic, sd);pj595})
try({pj596<- 0; mod <- Arima (d2, order=c(5,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj596 <- cbind(pj, aic, sd);pj596})
try({pj597<- 0; mod <- Arima (d2, order=c(5,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj597 <- cbind(pj, aic, sd);pj597})
try({pj598<- 0; mod <- Arima (d2, order=c(5,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj598 <- cbind(pj, aic, sd);pj598})
try({pj599<- 0; mod <- Arima (d2, order=c(5,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj599 <- cbind(pj, aic, sd);pj599})
try({pj600<- 0; mod <- Arima (d2, order=c(5,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj600 <- cbind(pj, aic, sd);pj600})
try({pj601<- 0; mod <- Arima (d2, order=c(6,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj601 <- cbind(pj, aic, sd);pj601})
try({pj602<- 0; mod <- Arima (d2, order=c(6,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj602 <- cbind(pj, aic, sd);pj602})
try({pj603<- 0; mod <- Arima (d2, order=c(6,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj603 <- cbind(pj, aic, sd);pj603})
try({pj604<- 0; mod <- Arima (d2, order=c(6,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj604 <- cbind(pj, aic, sd);pj604})
try({pj605<- 0; mod <- Arima (d2, order=c(6,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj605 <- cbind(pj, aic, sd);pj605})
try({pj606<- 0; mod <- Arima (d2, order=c(6,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj606 <- cbind(pj, aic, sd);pj606})
try({pj607<- 0; mod <- Arima (d2, order=c(6,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj607 <- cbind(pj, aic, sd);pj607})
try({pj608<- 0; mod <- Arima (d2, order=c(6,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj608 <- cbind(pj, aic, sd);pj608})
try({pj609<- 0; mod <- Arima (d2, order=c(6,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj609 <- cbind(pj, aic, sd);pj609})
try({pj610<- 0; mod <- Arima (d2, order=c(6,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj610 <- cbind(pj, aic, sd);pj610})
try({pj611<- 0; mod <- Arima (d2, order=c(6,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj611 <- cbind(pj, aic, sd);pj611})
try({pj612<- 0; mod <- Arima (d2, order=c(6,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj612 <- cbind(pj, aic, sd);pj612})
try({pj613<- 0; mod <- Arima (d2, order=c(6,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj613 <- cbind(pj, aic, sd);pj613})
try({pj614<- 0; mod <- Arima (d2, order=c(6,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj614 <- cbind(pj, aic, sd);pj614})
try({pj615<- 0; mod <- Arima (d2, order=c(6,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj615 <- cbind(pj, aic, sd);pj615})
try({pj616<- 0; mod <- Arima (d2, order=c(6,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj616 <- cbind(pj, aic, sd);pj616})
try({pj617<- 0; mod <- Arima (d2, order=c(6,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj617 <- cbind(pj, aic, sd);pj617})
try({pj618<- 0; mod <- Arima (d2, order=c(6,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj618 <- cbind(pj, aic, sd);pj618})
try({pj619<- 0; mod <- Arima (d2, order=c(6,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj619 <- cbind(pj, aic, sd);pj619})
try({pj620<- 0; mod <- Arima (d2, order=c(6,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj620 <- cbind(pj, aic, sd);pj620})
try({pj621<- 0; mod <- Arima (d2, order=c(6,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj621 <- cbind(pj, aic, sd);pj621})
try({pj622<- 0; mod <- Arima (d2, order=c(6,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj622 <- cbind(pj, aic, sd);pj622})
try({pj623<- 0; mod <- Arima (d2, order=c(6,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj623 <- cbind(pj, aic, sd);pj623})
try({pj624<- 0; mod <- Arima (d2, order=c(6,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj624 <- cbind(pj, aic, sd);pj624})
try({pj625<- 0; mod <- Arima (d2, order=c(6,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj625 <- cbind(pj, aic, sd);pj625})
try({pj626<- 0; mod <- Arima (d2, order=c(6,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj626 <- cbind(pj, aic, sd);pj626})
try({pj627<- 0; mod <- Arima (d2, order=c(6,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj627 <- cbind(pj, aic, sd);pj627})
try({pj628<- 0; mod <- Arima (d2, order=c(6,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj628 <- cbind(pj, aic, sd);pj628})
try({pj629<- 0; mod <- Arima (d2, order=c(6,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj629 <- cbind(pj, aic, sd);pj629})
try({pj630<- 0; mod <- Arima (d2, order=c(6,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj630 <- cbind(pj, aic, sd);pj630})
try({pj631<- 0; mod <- Arima (d2, order=c(6,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj631 <- cbind(pj, aic, sd);pj631})
try({pj632<- 0; mod <- Arima (d2, order=c(6,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj632 <- cbind(pj, aic, sd);pj632})
try({pj633<- 0; mod <- Arima (d2, order=c(6,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj633 <- cbind(pj, aic, sd);pj633})
try({pj634<- 0; mod <- Arima (d2, order=c(6,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj634 <- cbind(pj, aic, sd);pj634})
try({pj635<- 0; mod <- Arima (d2, order=c(6,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj635 <- cbind(pj, aic, sd);pj635})
try({pj636<- 0; mod <- Arima (d2, order=c(6,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj636 <- cbind(pj, aic, sd);pj636})
try({pj637<- 0; mod <- Arima (d2, order=c(6,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj637 <- cbind(pj, aic, sd);pj637})
try({pj638<- 0; mod <- Arima (d2, order=c(6,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj638 <- cbind(pj, aic, sd);pj638})
try({pj639<- 0; mod <- Arima (d2, order=c(6,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj639 <- cbind(pj, aic, sd);pj639})
try({pj640<- 0; mod <- Arima (d2, order=c(6,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj640 <- cbind(pj, aic, sd);pj640})
try({pj641<- 0; mod <- Arima (d2, order=c(6,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj641 <- cbind(pj, aic, sd);pj641})
try({pj642<- 0; mod <- Arima (d2, order=c(6,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj642 <- cbind(pj, aic, sd);pj642})
try({pj643<- 0; mod <- Arima (d2, order=c(6,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj643 <- cbind(pj, aic, sd);pj643})
try({pj644<- 0; mod <- Arima (d2, order=c(6,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj644 <- cbind(pj, aic, sd);pj644})
try({pj645<- 0; mod <- Arima (d2, order=c(6,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj645 <- cbind(pj, aic, sd);pj645})
try({pj646<- 0; mod <- Arima (d2, order=c(6,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj646 <- cbind(pj, aic, sd);pj646})
try({pj647<- 0; mod <- Arima (d2, order=c(6,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj647 <- cbind(pj, aic, sd);pj647})
try({pj648<- 0; mod <- Arima (d2, order=c(6,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj648 <- cbind(pj, aic, sd);pj648})
try({pj649<- 0; mod <- Arima (d2, order=c(6,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj649 <- cbind(pj, aic, sd);pj649})
try({pj650<- 0; mod <- Arima (d2, order=c(6,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj650 <- cbind(pj, aic, sd);pj650})
try({pj651<- 0; mod <- Arima (d2, order=c(6,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj651 <- cbind(pj, aic, sd);pj651})
try({pj652<- 0; mod <- Arima (d2, order=c(6,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj652 <- cbind(pj, aic, sd);pj652})
try({pj653<- 0; mod <- Arima (d2, order=c(6,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj653 <- cbind(pj, aic, sd);pj653})
try({pj654<- 0; mod <- Arima (d2, order=c(6,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj654 <- cbind(pj, aic, sd);pj654})
try({pj655<- 0; mod <- Arima (d2, order=c(6,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj655 <- cbind(pj, aic, sd);pj655})
try({pj656<- 0; mod <- Arima (d2, order=c(6,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj656 <- cbind(pj, aic, sd);pj656})
try({pj657<- 0; mod <- Arima (d2, order=c(6,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj657 <- cbind(pj, aic, sd);pj657})
try({pj658<- 0; mod <- Arima (d2, order=c(6,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj658 <- cbind(pj, aic, sd);pj658})
try({pj659<- 0; mod <- Arima (d2, order=c(6,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj659 <- cbind(pj, aic, sd);pj659})
try({pj660<- 0; mod <- Arima (d2, order=c(6,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj660 <- cbind(pj, aic, sd);pj660})
try({pj661<- 0; mod <- Arima (d2, order=c(6,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj661 <- cbind(pj, aic, sd);pj661})
try({pj662<- 0; mod <- Arima (d2, order=c(6,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj662 <- cbind(pj, aic, sd);pj662})
try({pj663<- 0; mod <- Arima (d2, order=c(6,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj663 <- cbind(pj, aic, sd);pj663})
try({pj664<- 0; mod <- Arima (d2, order=c(6,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj664 <- cbind(pj, aic, sd);pj664})
try({pj665<- 0; mod <- Arima (d2, order=c(6,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj665 <- cbind(pj, aic, sd);pj665})
try({pj666<- 0; mod <- Arima (d2, order=c(6,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj666 <- cbind(pj, aic, sd);pj666})
try({pj667<- 0; mod <- Arima (d2, order=c(6,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj667 <- cbind(pj, aic, sd);pj667})
try({pj668<- 0; mod <- Arima (d2, order=c(6,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj668 <- cbind(pj, aic, sd);pj668})
try({pj669<- 0; mod <- Arima (d2, order=c(6,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj669 <- cbind(pj, aic, sd);pj669})
try({pj670<- 0; mod <- Arima (d2, order=c(6,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj670 <- cbind(pj, aic, sd);pj670})
try({pj671<- 0; mod <- Arima (d2, order=c(6,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj671 <- cbind(pj, aic, sd);pj671})
try({pj672<- 0; mod <- Arima (d2, order=c(6,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj672 <- cbind(pj, aic, sd);pj672})
try({pj673<- 0; mod <- Arima (d2, order=c(6,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj673 <- cbind(pj, aic, sd);pj673})
try({pj674<- 0; mod <- Arima (d2, order=c(6,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj674 <- cbind(pj, aic, sd);pj674})
try({pj675<- 0; mod <- Arima (d2, order=c(6,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj675 <- cbind(pj, aic, sd);pj675})
try({pj676<- 0; mod <- Arima (d2, order=c(6,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj676 <- cbind(pj, aic, sd);pj676})
try({pj677<- 0; mod <- Arima (d2, order=c(6,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj677 <- cbind(pj, aic, sd);pj677})
try({pj678<- 0; mod <- Arima (d2, order=c(6,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj678 <- cbind(pj, aic, sd);pj678})
try({pj679<- 0; mod <- Arima (d2, order=c(6,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj679 <- cbind(pj, aic, sd);pj679})
try({pj680<- 0; mod <- Arima (d2, order=c(6,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj680 <- cbind(pj, aic, sd);pj680})
try({pj681<- 0; mod <- Arima (d2, order=c(6,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj681 <- cbind(pj, aic, sd);pj681})
try({pj682<- 0; mod <- Arima (d2, order=c(6,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj682 <- cbind(pj, aic, sd);pj682})
try({pj683<- 0; mod <- Arima (d2, order=c(6,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj683 <- cbind(pj, aic, sd);pj683})
try({pj684<- 0; mod <- Arima (d2, order=c(6,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj684 <- cbind(pj, aic, sd);pj684})
try({pj685<- 0; mod <- Arima (d2, order=c(6,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj685 <- cbind(pj, aic, sd);pj685})
try({pj686<- 0; mod <- Arima (d2, order=c(6,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj686 <- cbind(pj, aic, sd);pj686})
try({pj687<- 0; mod <- Arima (d2, order=c(6,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj687 <- cbind(pj, aic, sd);pj687})
try({pj688<- 0; mod <- Arima (d2, order=c(6,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj688 <- cbind(pj, aic, sd);pj688})
try({pj689<- 0; mod <- Arima (d2, order=c(6,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj689 <- cbind(pj, aic, sd);pj689})
try({pj690<- 0; mod <- Arima (d2, order=c(6,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj690 <- cbind(pj, aic, sd);pj690})
try({pj691<- 0; mod <- Arima (d2, order=c(6,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj691 <- cbind(pj, aic, sd);pj691})
try({pj692<- 0; mod <- Arima (d2, order=c(6,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj692 <- cbind(pj, aic, sd);pj692})
try({pj693<- 0; mod <- Arima (d2, order=c(6,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj693 <- cbind(pj, aic, sd);pj693})
try({pj694<- 0; mod <- Arima (d2, order=c(6,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj694 <- cbind(pj, aic, sd);pj694})
try({pj695<- 0; mod <- Arima (d2, order=c(6,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj695 <- cbind(pj, aic, sd);pj695})
try({pj696<- 0; mod <- Arima (d2, order=c(6,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj696 <- cbind(pj, aic, sd);pj696})
try({pj697<- 0; mod <- Arima (d2, order=c(6,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj697 <- cbind(pj, aic, sd);pj697})
try({pj698<- 0; mod <- Arima (d2, order=c(6,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj698 <- cbind(pj, aic, sd);pj698})
try({pj699<- 0; mod <- Arima (d2, order=c(6,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj699 <- cbind(pj, aic, sd);pj699})
try({pj700<- 0; mod <- Arima (d2, order=c(6,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj700 <- cbind(pj, aic, sd);pj700})
try({pj701<- 0; mod <- Arima (d2, order=c(7,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj701 <- cbind(pj, aic, sd);pj701})
try({pj702<- 0; mod <- Arima (d2, order=c(7,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj702 <- cbind(pj, aic, sd);pj702})
try({pj703<- 0; mod <- Arima (d2, order=c(7,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj703 <- cbind(pj, aic, sd);pj703})
try({pj704<- 0; mod <- Arima (d2, order=c(7,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj704 <- cbind(pj, aic, sd);pj704})
try({pj705<- 0; mod <- Arima (d2, order=c(7,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj705 <- cbind(pj, aic, sd);pj705})
try({pj706<- 0; mod <- Arima (d2, order=c(7,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj706 <- cbind(pj, aic, sd);pj706})
try({pj707<- 0; mod <- Arima (d2, order=c(7,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj707 <- cbind(pj, aic, sd);pj707})
try({pj708<- 0; mod <- Arima (d2, order=c(7,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj708 <- cbind(pj, aic, sd);pj708})
try({pj709<- 0; mod <- Arima (d2, order=c(7,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj709 <- cbind(pj, aic, sd);pj709})
try({pj710<- 0; mod <- Arima (d2, order=c(7,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj710 <- cbind(pj, aic, sd);pj710})
try({pj711<- 0; mod <- Arima (d2, order=c(7,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj711 <- cbind(pj, aic, sd);pj711})
try({pj712<- 0; mod <- Arima (d2, order=c(7,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj712 <- cbind(pj, aic, sd);pj712})
try({pj713<- 0; mod <- Arima (d2, order=c(7,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj713 <- cbind(pj, aic, sd);pj713})
try({pj714<- 0; mod <- Arima (d2, order=c(7,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj714 <- cbind(pj, aic, sd);pj714})
try({pj715<- 0; mod <- Arima (d2, order=c(7,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj715 <- cbind(pj, aic, sd);pj715})
try({pj716<- 0; mod <- Arima (d2, order=c(7,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj716 <- cbind(pj, aic, sd);pj716})
try({pj717<- 0; mod <- Arima (d2, order=c(7,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj717 <- cbind(pj, aic, sd);pj717})
try({pj718<- 0; mod <- Arima (d2, order=c(7,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj718 <- cbind(pj, aic, sd);pj718})
try({pj719<- 0; mod <- Arima (d2, order=c(7,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj719 <- cbind(pj, aic, sd);pj719})
try({pj720<- 0; mod <- Arima (d2, order=c(7,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj720 <- cbind(pj, aic, sd);pj720})
try({pj721<- 0; mod <- Arima (d2, order=c(7,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj721 <- cbind(pj, aic, sd);pj721})
try({pj722<- 0; mod <- Arima (d2, order=c(7,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj722 <- cbind(pj, aic, sd);pj722})
try({pj723<- 0; mod <- Arima (d2, order=c(7,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj723 <- cbind(pj, aic, sd);pj723})
try({pj724<- 0; mod <- Arima (d2, order=c(7,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj724 <- cbind(pj, aic, sd);pj724})
try({pj725<- 0; mod <- Arima (d2, order=c(7,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj725 <- cbind(pj, aic, sd);pj725})
try({pj726<- 0; mod <- Arima (d2, order=c(7,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj726 <- cbind(pj, aic, sd);pj726})
try({pj727<- 0; mod <- Arima (d2, order=c(7,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj727 <- cbind(pj, aic, sd);pj727})
try({pj728<- 0; mod <- Arima (d2, order=c(7,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj728 <- cbind(pj, aic, sd);pj728})
try({pj729<- 0; mod <- Arima (d2, order=c(7,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj729 <- cbind(pj, aic, sd);pj729})
try({pj730<- 0; mod <- Arima (d2, order=c(7,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj730 <- cbind(pj, aic, sd);pj730})
try({pj731<- 0; mod <- Arima (d2, order=c(7,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj731 <- cbind(pj, aic, sd);pj731})
try({pj732<- 0; mod <- Arima (d2, order=c(7,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj732 <- cbind(pj, aic, sd);pj732})
try({pj733<- 0; mod <- Arima (d2, order=c(7,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj733 <- cbind(pj, aic, sd);pj733})
try({pj734<- 0; mod <- Arima (d2, order=c(7,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj734 <- cbind(pj, aic, sd);pj734})
try({pj735<- 0; mod <- Arima (d2, order=c(7,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj735 <- cbind(pj, aic, sd);pj735})
try({pj736<- 0; mod <- Arima (d2, order=c(7,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj736 <- cbind(pj, aic, sd);pj736})
try({pj737<- 0; mod <- Arima (d2, order=c(7,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj737 <- cbind(pj, aic, sd);pj737})
try({pj738<- 0; mod <- Arima (d2, order=c(7,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj738 <- cbind(pj, aic, sd);pj738})
try({pj739<- 0; mod <- Arima (d2, order=c(7,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj739 <- cbind(pj, aic, sd);pj739})
try({pj740<- 0; mod <- Arima (d2, order=c(7,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj740 <- cbind(pj, aic, sd);pj740})
try({pj741<- 0; mod <- Arima (d2, order=c(7,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj741 <- cbind(pj, aic, sd);pj741})
try({pj742<- 0; mod <- Arima (d2, order=c(7,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj742 <- cbind(pj, aic, sd);pj742})
try({pj743<- 0; mod <- Arima (d2, order=c(7,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj743 <- cbind(pj, aic, sd);pj743})
try({pj744<- 0; mod <- Arima (d2, order=c(7,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj744 <- cbind(pj, aic, sd);pj744})
try({pj745<- 0; mod <- Arima (d2, order=c(7,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj745 <- cbind(pj, aic, sd);pj745})
try({pj746<- 0; mod <- Arima (d2, order=c(7,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj746 <- cbind(pj, aic, sd);pj746})
try({pj747<- 0; mod <- Arima (d2, order=c(7,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj747 <- cbind(pj, aic, sd);pj747})
try({pj748<- 0; mod <- Arima (d2, order=c(7,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj748 <- cbind(pj, aic, sd);pj748})
try({pj749<- 0; mod <- Arima (d2, order=c(7,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj749 <- cbind(pj, aic, sd);pj749})
try({pj750<- 0; mod <- Arima (d2, order=c(7,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj750 <- cbind(pj, aic, sd);pj750})
try({pj751<- 0; mod <- Arima (d2, order=c(7,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj751 <- cbind(pj, aic, sd);pj751})
try({pj752<- 0; mod <- Arima (d2, order=c(7,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj752 <- cbind(pj, aic, sd);pj752})
try({pj753<- 0; mod <- Arima (d2, order=c(7,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj753 <- cbind(pj, aic, sd);pj753})
try({pj754<- 0; mod <- Arima (d2, order=c(7,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj754 <- cbind(pj, aic, sd);pj754})
try({pj755<- 0; mod <- Arima (d2, order=c(7,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj755 <- cbind(pj, aic, sd);pj755})
try({pj756<- 0; mod <- Arima (d2, order=c(7,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj756 <- cbind(pj, aic, sd);pj756})
try({pj757<- 0; mod <- Arima (d2, order=c(7,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj757 <- cbind(pj, aic, sd);pj757})
try({pj758<- 0; mod <- Arima (d2, order=c(7,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj758 <- cbind(pj, aic, sd);pj758})
try({pj759<- 0; mod <- Arima (d2, order=c(7,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj759 <- cbind(pj, aic, sd);pj759})
try({pj760<- 0; mod <- Arima (d2, order=c(7,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj760 <- cbind(pj, aic, sd);pj760})
try({pj761<- 0; mod <- Arima (d2, order=c(7,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj761 <- cbind(pj, aic, sd);pj761})
try({pj762<- 0; mod <- Arima (d2, order=c(7,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj762 <- cbind(pj, aic, sd);pj762})
try({pj763<- 0; mod <- Arima (d2, order=c(7,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj763 <- cbind(pj, aic, sd);pj763})
try({pj764<- 0; mod <- Arima (d2, order=c(7,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj764 <- cbind(pj, aic, sd);pj764})
try({pj765<- 0; mod <- Arima (d2, order=c(7,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj765 <- cbind(pj, aic, sd);pj765})
try({pj766<- 0; mod <- Arima (d2, order=c(7,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj766 <- cbind(pj, aic, sd);pj766})
try({pj767<- 0; mod <- Arima (d2, order=c(7,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj767 <- cbind(pj, aic, sd);pj767})
try({pj768<- 0; mod <- Arima (d2, order=c(7,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj768 <- cbind(pj, aic, sd);pj768})
try({pj769<- 0; mod <- Arima (d2, order=c(7,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj769 <- cbind(pj, aic, sd);pj769})
try({pj770<- 0; mod <- Arima (d2, order=c(7,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj770 <- cbind(pj, aic, sd);pj770})
try({pj771<- 0; mod <- Arima (d2, order=c(7,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj771 <- cbind(pj, aic, sd);pj771})
try({pj772<- 0; mod <- Arima (d2, order=c(7,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj772 <- cbind(pj, aic, sd);pj772})
try({pj773<- 0; mod <- Arima (d2, order=c(7,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj773 <- cbind(pj, aic, sd);pj773})
try({pj774<- 0; mod <- Arima (d2, order=c(7,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj774 <- cbind(pj, aic, sd);pj774})
try({pj775<- 0; mod <- Arima (d2, order=c(7,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj775 <- cbind(pj, aic, sd);pj775})
try({pj776<- 0; mod <- Arima (d2, order=c(7,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj776 <- cbind(pj, aic, sd);pj776})
try({pj777<- 0; mod <- Arima (d2, order=c(7,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj777 <- cbind(pj, aic, sd);pj777})
try({pj778<- 0; mod <- Arima (d2, order=c(7,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj778 <- cbind(pj, aic, sd);pj778})
try({pj779<- 0; mod <- Arima (d2, order=c(7,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj779 <- cbind(pj, aic, sd);pj779})
try({pj780<- 0; mod <- Arima (d2, order=c(7,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj780 <- cbind(pj, aic, sd);pj780})
try({pj781<- 0; mod <- Arima (d2, order=c(7,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj781 <- cbind(pj, aic, sd);pj781})
try({pj782<- 0; mod <- Arima (d2, order=c(7,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj782 <- cbind(pj, aic, sd);pj782})
try({pj783<- 0; mod <- Arima (d2, order=c(7,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj783 <- cbind(pj, aic, sd);pj783})
try({pj784<- 0; mod <- Arima (d2, order=c(7,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj784 <- cbind(pj, aic, sd);pj784})
try({pj785<- 0; mod <- Arima (d2, order=c(7,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj785 <- cbind(pj, aic, sd);pj785})
try({pj786<- 0; mod <- Arima (d2, order=c(7,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj786 <- cbind(pj, aic, sd);pj786})
try({pj787<- 0; mod <- Arima (d2, order=c(7,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj787 <- cbind(pj, aic, sd);pj787})
try({pj788<- 0; mod <- Arima (d2, order=c(7,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj788 <- cbind(pj, aic, sd);pj788})
try({pj789<- 0; mod <- Arima (d2, order=c(7,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj789 <- cbind(pj, aic, sd);pj789})
try({pj790<- 0; mod <- Arima (d2, order=c(7,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj790 <- cbind(pj, aic, sd);pj790})
try({pj791<- 0; mod <- Arima (d2, order=c(7,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj791 <- cbind(pj, aic, sd);pj791})
try({pj792<- 0; mod <- Arima (d2, order=c(7,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj792 <- cbind(pj, aic, sd);pj792})
try({pj793<- 0; mod <- Arima (d2, order=c(7,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj793 <- cbind(pj, aic, sd);pj793})
try({pj794<- 0; mod <- Arima (d2, order=c(7,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj794 <- cbind(pj, aic, sd);pj794})
try({pj795<- 0; mod <- Arima (d2, order=c(7,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj795 <- cbind(pj, aic, sd);pj795})
try({pj796<- 0; mod <- Arima (d2, order=c(7,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj796 <- cbind(pj, aic, sd);pj796})
try({pj797<- 0; mod <- Arima (d2, order=c(7,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj797 <- cbind(pj, aic, sd);pj797})
try({pj798<- 0; mod <- Arima (d2, order=c(7,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj798 <- cbind(pj, aic, sd);pj798})
try({pj799<- 0; mod <- Arima (d2, order=c(7,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj799 <- cbind(pj, aic, sd);pj799})
try({pj800<- 0; mod <- Arima (d2, order=c(7,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj800 <- cbind(pj, aic, sd);pj800})
try({pj801<- 0; mod <- Arima (d2, order=c(8,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj801 <- cbind(pj, aic, sd);pj801})
try({pj802<- 0; mod <- Arima (d2, order=c(8,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj802 <- cbind(pj, aic, sd);pj802})
try({pj803<- 0; mod <- Arima (d2, order=c(8,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj803 <- cbind(pj, aic, sd);pj803})
try({pj804<- 0; mod <- Arima (d2, order=c(8,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj804 <- cbind(pj, aic, sd);pj804})
try({pj805<- 0; mod <- Arima (d2, order=c(8,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj805 <- cbind(pj, aic, sd);pj805})
try({pj806<- 0; mod <- Arima (d2, order=c(8,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj806 <- cbind(pj, aic, sd);pj806})
try({pj807<- 0; mod <- Arima (d2, order=c(8,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj807 <- cbind(pj, aic, sd);pj807})
try({pj808<- 0; mod <- Arima (d2, order=c(8,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj808 <- cbind(pj, aic, sd);pj808})
try({pj809<- 0; mod <- Arima (d2, order=c(8,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj809 <- cbind(pj, aic, sd);pj809})
try({pj810<- 0; mod <- Arima (d2, order=c(8,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj810 <- cbind(pj, aic, sd);pj810})
try({pj811<- 0; mod <- Arima (d2, order=c(8,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj811 <- cbind(pj, aic, sd);pj811})
try({pj812<- 0; mod <- Arima (d2, order=c(8,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj812 <- cbind(pj, aic, sd);pj812})
try({pj813<- 0; mod <- Arima (d2, order=c(8,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj813 <- cbind(pj, aic, sd);pj813})
try({pj814<- 0; mod <- Arima (d2, order=c(8,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj814 <- cbind(pj, aic, sd);pj814})
try({pj815<- 0; mod <- Arima (d2, order=c(8,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj815 <- cbind(pj, aic, sd);pj815})
try({pj816<- 0; mod <- Arima (d2, order=c(8,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj816 <- cbind(pj, aic, sd);pj816})
try({pj817<- 0; mod <- Arima (d2, order=c(8,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj817 <- cbind(pj, aic, sd);pj817})
try({pj818<- 0; mod <- Arima (d2, order=c(8,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj818 <- cbind(pj, aic, sd);pj818})
try({pj819<- 0; mod <- Arima (d2, order=c(8,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj819 <- cbind(pj, aic, sd);pj819})
try({pj820<- 0; mod <- Arima (d2, order=c(8,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj820 <- cbind(pj, aic, sd);pj820})
try({pj821<- 0; mod <- Arima (d2, order=c(8,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj821 <- cbind(pj, aic, sd);pj821})
try({pj822<- 0; mod <- Arima (d2, order=c(8,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj822 <- cbind(pj, aic, sd);pj822})
try({pj823<- 0; mod <- Arima (d2, order=c(8,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj823 <- cbind(pj, aic, sd);pj823})
try({pj824<- 0; mod <- Arima (d2, order=c(8,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj824 <- cbind(pj, aic, sd);pj824})
try({pj825<- 0; mod <- Arima (d2, order=c(8,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj825 <- cbind(pj, aic, sd);pj825})
try({pj826<- 0; mod <- Arima (d2, order=c(8,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj826 <- cbind(pj, aic, sd);pj826})
try({pj827<- 0; mod <- Arima (d2, order=c(8,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj827 <- cbind(pj, aic, sd);pj827})
try({pj828<- 0; mod <- Arima (d2, order=c(8,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj828 <- cbind(pj, aic, sd);pj828})
try({pj829<- 0; mod <- Arima (d2, order=c(8,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj829 <- cbind(pj, aic, sd);pj829})
try({pj830<- 0; mod <- Arima (d2, order=c(8,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj830 <- cbind(pj, aic, sd);pj830})
try({pj831<- 0; mod <- Arima (d2, order=c(8,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj831 <- cbind(pj, aic, sd);pj831})
try({pj832<- 0; mod <- Arima (d2, order=c(8,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj832 <- cbind(pj, aic, sd);pj832})
try({pj833<- 0; mod <- Arima (d2, order=c(8,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj833 <- cbind(pj, aic, sd);pj833})
try({pj834<- 0; mod <- Arima (d2, order=c(8,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj834 <- cbind(pj, aic, sd);pj834})
try({pj835<- 0; mod <- Arima (d2, order=c(8,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj835 <- cbind(pj, aic, sd);pj835})
try({pj836<- 0; mod <- Arima (d2, order=c(8,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj836 <- cbind(pj, aic, sd);pj836})
try({pj837<- 0; mod <- Arima (d2, order=c(8,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj837 <- cbind(pj, aic, sd);pj837})
try({pj838<- 0; mod <- Arima (d2, order=c(8,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj838 <- cbind(pj, aic, sd);pj838})
try({pj839<- 0; mod <- Arima (d2, order=c(8,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj839 <- cbind(pj, aic, sd);pj839})
try({pj840<- 0; mod <- Arima (d2, order=c(8,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj840 <- cbind(pj, aic, sd);pj840})
try({pj841<- 0; mod <- Arima (d2, order=c(8,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj841 <- cbind(pj, aic, sd);pj841})
try({pj842<- 0; mod <- Arima (d2, order=c(8,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj842 <- cbind(pj, aic, sd);pj842})
try({pj843<- 0; mod <- Arima (d2, order=c(8,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj843 <- cbind(pj, aic, sd);pj843})
try({pj844<- 0; mod <- Arima (d2, order=c(8,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj844 <- cbind(pj, aic, sd);pj844})
try({pj845<- 0; mod <- Arima (d2, order=c(8,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj845 <- cbind(pj, aic, sd);pj845})
try({pj846<- 0; mod <- Arima (d2, order=c(8,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj846 <- cbind(pj, aic, sd);pj846})
try({pj847<- 0; mod <- Arima (d2, order=c(8,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj847 <- cbind(pj, aic, sd);pj847})
try({pj848<- 0; mod <- Arima (d2, order=c(8,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj848 <- cbind(pj, aic, sd);pj848})
try({pj849<- 0; mod <- Arima (d2, order=c(8,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj849 <- cbind(pj, aic, sd);pj849})
try({pj850<- 0; mod <- Arima (d2, order=c(8,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj850 <- cbind(pj, aic, sd);pj850})
try({pj851<- 0; mod <- Arima (d2, order=c(8,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj851 <- cbind(pj, aic, sd);pj851})
try({pj852<- 0; mod <- Arima (d2, order=c(8,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj852 <- cbind(pj, aic, sd);pj852})
try({pj853<- 0; mod <- Arima (d2, order=c(8,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj853 <- cbind(pj, aic, sd);pj853})
try({pj854<- 0; mod <- Arima (d2, order=c(8,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj854 <- cbind(pj, aic, sd);pj854})
try({pj855<- 0; mod <- Arima (d2, order=c(8,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj855 <- cbind(pj, aic, sd);pj855})
try({pj856<- 0; mod <- Arima (d2, order=c(8,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj856 <- cbind(pj, aic, sd);pj856})
try({pj857<- 0; mod <- Arima (d2, order=c(8,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj857 <- cbind(pj, aic, sd);pj857})
try({pj858<- 0; mod <- Arima (d2, order=c(8,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj858 <- cbind(pj, aic, sd);pj858})
try({pj859<- 0; mod <- Arima (d2, order=c(8,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj859 <- cbind(pj, aic, sd);pj859})
try({pj860<- 0; mod <- Arima (d2, order=c(8,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj860 <- cbind(pj, aic, sd);pj860})
try({pj861<- 0; mod <- Arima (d2, order=c(8,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj861 <- cbind(pj, aic, sd);pj861})
try({pj862<- 0; mod <- Arima (d2, order=c(8,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj862 <- cbind(pj, aic, sd);pj862})
try({pj863<- 0; mod <- Arima (d2, order=c(8,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj863 <- cbind(pj, aic, sd);pj863})
try({pj864<- 0; mod <- Arima (d2, order=c(8,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj864 <- cbind(pj, aic, sd);pj864})
try({pj865<- 0; mod <- Arima (d2, order=c(8,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj865 <- cbind(pj, aic, sd);pj865})
try({pj866<- 0; mod <- Arima (d2, order=c(8,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj866 <- cbind(pj, aic, sd);pj866})
try({pj867<- 0; mod <- Arima (d2, order=c(8,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj867 <- cbind(pj, aic, sd);pj867})
try({pj868<- 0; mod <- Arima (d2, order=c(8,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj868 <- cbind(pj, aic, sd);pj868})
try({pj869<- 0; mod <- Arima (d2, order=c(8,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj869 <- cbind(pj, aic, sd);pj869})
try({pj870<- 0; mod <- Arima (d2, order=c(8,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj870 <- cbind(pj, aic, sd);pj870})
try({pj871<- 0; mod <- Arima (d2, order=c(8,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj871 <- cbind(pj, aic, sd);pj871})
try({pj872<- 0; mod <- Arima (d2, order=c(8,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj872 <- cbind(pj, aic, sd);pj872})
try({pj873<- 0; mod <- Arima (d2, order=c(8,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj873 <- cbind(pj, aic, sd);pj873})
try({pj874<- 0; mod <- Arima (d2, order=c(8,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj874 <- cbind(pj, aic, sd);pj874})
try({pj875<- 0; mod <- Arima (d2, order=c(8,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj875 <- cbind(pj, aic, sd);pj875})
try({pj876<- 0; mod <- Arima (d2, order=c(8,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj876 <- cbind(pj, aic, sd);pj876})
try({pj877<- 0; mod <- Arima (d2, order=c(8,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj877 <- cbind(pj, aic, sd);pj877})
try({pj878<- 0; mod <- Arima (d2, order=c(8,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj878 <- cbind(pj, aic, sd);pj878})
try({pj879<- 0; mod <- Arima (d2, order=c(8,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj879 <- cbind(pj, aic, sd);pj879})
try({pj880<- 0; mod <- Arima (d2, order=c(8,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj880 <- cbind(pj, aic, sd);pj880})
try({pj881<- 0; mod <- Arima (d2, order=c(8,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj881 <- cbind(pj, aic, sd);pj881})
try({pj882<- 0; mod <- Arima (d2, order=c(8,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj882 <- cbind(pj, aic, sd);pj882})
try({pj883<- 0; mod <- Arima (d2, order=c(8,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj883 <- cbind(pj, aic, sd);pj883})
try({pj884<- 0; mod <- Arima (d2, order=c(8,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj884 <- cbind(pj, aic, sd);pj884})
try({pj885<- 0; mod <- Arima (d2, order=c(8,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj885 <- cbind(pj, aic, sd);pj885})
try({pj886<- 0; mod <- Arima (d2, order=c(8,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj886 <- cbind(pj, aic, sd);pj886})
try({pj887<- 0; mod <- Arima (d2, order=c(8,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj887 <- cbind(pj, aic, sd);pj887})
try({pj888<- 0; mod <- Arima (d2, order=c(8,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj888 <- cbind(pj, aic, sd);pj888})
try({pj889<- 0; mod <- Arima (d2, order=c(8,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj889 <- cbind(pj, aic, sd);pj889})
try({pj890<- 0; mod <- Arima (d2, order=c(8,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj890 <- cbind(pj, aic, sd);pj890})
try({pj891<- 0; mod <- Arima (d2, order=c(8,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj891 <- cbind(pj, aic, sd);pj891})
try({pj892<- 0; mod <- Arima (d2, order=c(8,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj892 <- cbind(pj, aic, sd);pj892})
try({pj893<- 0; mod <- Arima (d2, order=c(8,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj893 <- cbind(pj, aic, sd);pj893})
try({pj894<- 0; mod <- Arima (d2, order=c(8,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj894 <- cbind(pj, aic, sd);pj894})
try({pj895<- 0; mod <- Arima (d2, order=c(8,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj895 <- cbind(pj, aic, sd);pj895})
try({pj896<- 0; mod <- Arima (d2, order=c(8,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj896 <- cbind(pj, aic, sd);pj896})
try({pj897<- 0; mod <- Arima (d2, order=c(8,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj897 <- cbind(pj, aic, sd);pj897})
try({pj898<- 0; mod <- Arima (d2, order=c(8,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj898 <- cbind(pj, aic, sd);pj898})
try({pj899<- 0; mod <- Arima (d2, order=c(8,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj899 <- cbind(pj, aic, sd);pj899})
try({pj900<- 0; mod <- Arima (d2, order=c(8,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj900 <- cbind(pj, aic, sd);pj900})
try({pj901<- 0; mod <- Arima (d2, order=c(9,0,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj901 <- cbind(pj, aic, sd);pj901})
try({pj902<- 0; mod <- Arima (d2, order=c(9,0,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj902 <- cbind(pj, aic, sd);pj902})
try({pj903<- 0; mod <- Arima (d2, order=c(9,0,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj903 <- cbind(pj, aic, sd);pj903})
try({pj904<- 0; mod <- Arima (d2, order=c(9,0,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj904 <- cbind(pj, aic, sd);pj904})
try({pj905<- 0; mod <- Arima (d2, order=c(9,0,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj905 <- cbind(pj, aic, sd);pj905})
try({pj906<- 0; mod <- Arima (d2, order=c(9,0,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj906 <- cbind(pj, aic, sd);pj906})
try({pj907<- 0; mod <- Arima (d2, order=c(9,0,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj907 <- cbind(pj, aic, sd);pj907})
try({pj908<- 0; mod <- Arima (d2, order=c(9,0,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj908 <- cbind(pj, aic, sd);pj908})
try({pj909<- 0; mod <- Arima (d2, order=c(9,0,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj909 <- cbind(pj, aic, sd);pj909})
try({pj910<- 0; mod <- Arima (d2, order=c(9,0,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj910 <- cbind(pj, aic, sd);pj910})
try({pj911<- 0; mod <- Arima (d2, order=c(9,1,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj911 <- cbind(pj, aic, sd);pj911})
try({pj912<- 0; mod <- Arima (d2, order=c(9,1,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj912 <- cbind(pj, aic, sd);pj912})
try({pj913<- 0; mod <- Arima (d2, order=c(9,1,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj913 <- cbind(pj, aic, sd);pj913})
try({pj914<- 0; mod <- Arima (d2, order=c(9,1,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj914 <- cbind(pj, aic, sd);pj914})
try({pj915<- 0; mod <- Arima (d2, order=c(9,1,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj915 <- cbind(pj, aic, sd);pj915})
try({pj916<- 0; mod <- Arima (d2, order=c(9,1,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj916 <- cbind(pj, aic, sd);pj916})
try({pj917<- 0; mod <- Arima (d2, order=c(9,1,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj917 <- cbind(pj, aic, sd);pj917})
try({pj918<- 0; mod <- Arima (d2, order=c(9,1,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj918 <- cbind(pj, aic, sd);pj918})
try({pj919<- 0; mod <- Arima (d2, order=c(9,1,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj919 <- cbind(pj, aic, sd);pj919})
try({pj920<- 0; mod <- Arima (d2, order=c(9,1,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj920 <- cbind(pj, aic, sd);pj920})
try({pj921<- 0; mod <- Arima (d2, order=c(9,2,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj921 <- cbind(pj, aic, sd);pj921})
try({pj922<- 0; mod <- Arima (d2, order=c(9,2,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj922 <- cbind(pj, aic, sd);pj922})
try({pj923<- 0; mod <- Arima (d2, order=c(9,2,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj923 <- cbind(pj, aic, sd);pj923})
try({pj924<- 0; mod <- Arima (d2, order=c(9,2,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj924 <- cbind(pj, aic, sd);pj924})
try({pj925<- 0; mod <- Arima (d2, order=c(9,2,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj925 <- cbind(pj, aic, sd);pj925})
try({pj926<- 0; mod <- Arima (d2, order=c(9,2,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj926 <- cbind(pj, aic, sd);pj926})
try({pj927<- 0; mod <- Arima (d2, order=c(9,2,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj927 <- cbind(pj, aic, sd);pj927})
try({pj928<- 0; mod <- Arima (d2, order=c(9,2,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj928 <- cbind(pj, aic, sd);pj928})
try({pj929<- 0; mod <- Arima (d2, order=c(9,2,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj929 <- cbind(pj, aic, sd);pj929})
try({pj930<- 0; mod <- Arima (d2, order=c(9,2,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj930 <- cbind(pj, aic, sd);pj930})
try({pj931<- 0; mod <- Arima (d2, order=c(9,3,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj931 <- cbind(pj, aic, sd);pj931})
try({pj932<- 0; mod <- Arima (d2, order=c(9,3,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj932 <- cbind(pj, aic, sd);pj932})
try({pj933<- 0; mod <- Arima (d2, order=c(9,3,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj933 <- cbind(pj, aic, sd);pj933})
try({pj934<- 0; mod <- Arima (d2, order=c(9,3,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj934 <- cbind(pj, aic, sd);pj934})
try({pj935<- 0; mod <- Arima (d2, order=c(9,3,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj935 <- cbind(pj, aic, sd);pj935})
try({pj936<- 0; mod <- Arima (d2, order=c(9,3,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj936 <- cbind(pj, aic, sd);pj936})
try({pj937<- 0; mod <- Arima (d2, order=c(9,3,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj937 <- cbind(pj, aic, sd);pj937})
try({pj938<- 0; mod <- Arima (d2, order=c(9,3,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj938 <- cbind(pj, aic, sd);pj938})
try({pj939<- 0; mod <- Arima (d2, order=c(9,3,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj939 <- cbind(pj, aic, sd);pj939})
try({pj940<- 0; mod <- Arima (d2, order=c(9,3,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj940 <- cbind(pj, aic, sd);pj940})
try({pj941<- 0; mod <- Arima (d2, order=c(9,4,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj941 <- cbind(pj, aic, sd);pj941})
try({pj942<- 0; mod <- Arima (d2, order=c(9,4,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj942 <- cbind(pj, aic, sd);pj942})
try({pj943<- 0; mod <- Arima (d2, order=c(9,4,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj943 <- cbind(pj, aic, sd);pj943})
try({pj944<- 0; mod <- Arima (d2, order=c(9,4,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj944 <- cbind(pj, aic, sd);pj944})
try({pj945<- 0; mod <- Arima (d2, order=c(9,4,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj945 <- cbind(pj, aic, sd);pj945})
try({pj946<- 0; mod <- Arima (d2, order=c(9,4,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj946 <- cbind(pj, aic, sd);pj946})
try({pj947<- 0; mod <- Arima (d2, order=c(9,4,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj947 <- cbind(pj, aic, sd);pj947})
try({pj948<- 0; mod <- Arima (d2, order=c(9,4,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj948 <- cbind(pj, aic, sd);pj948})
try({pj949<- 0; mod <- Arima (d2, order=c(9,4,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj949 <- cbind(pj, aic, sd);pj949})
try({pj950<- 0; mod <- Arima (d2, order=c(9,4,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj950 <- cbind(pj, aic, sd);pj950})
try({pj951<- 0; mod <- Arima (d2, order=c(9,5,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj951 <- cbind(pj, aic, sd);pj951})
try({pj952<- 0; mod <- Arima (d2, order=c(9,5,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj952 <- cbind(pj, aic, sd);pj952})
try({pj953<- 0; mod <- Arima (d2, order=c(9,5,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj953 <- cbind(pj, aic, sd);pj953})
try({pj954<- 0; mod <- Arima (d2, order=c(9,5,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj954 <- cbind(pj, aic, sd);pj954})
try({pj955<- 0; mod <- Arima (d2, order=c(9,5,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj955 <- cbind(pj, aic, sd);pj955})
try({pj956<- 0; mod <- Arima (d2, order=c(9,5,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj956 <- cbind(pj, aic, sd);pj956})
try({pj957<- 0; mod <- Arima (d2, order=c(9,5,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj957 <- cbind(pj, aic, sd);pj957})
try({pj958<- 0; mod <- Arima (d2, order=c(9,5,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj958 <- cbind(pj, aic, sd);pj958})
try({pj959<- 0; mod <- Arima (d2, order=c(9,5,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj959 <- cbind(pj, aic, sd);pj959})
try({pj960<- 0; mod <- Arima (d2, order=c(9,5,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj960 <- cbind(pj, aic, sd);pj960})
try({pj961<- 0; mod <- Arima (d2, order=c(9,6,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj961 <- cbind(pj, aic, sd);pj961})
try({pj962<- 0; mod <- Arima (d2, order=c(9,6,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj962 <- cbind(pj, aic, sd);pj962})
try({pj963<- 0; mod <- Arima (d2, order=c(9,6,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj963 <- cbind(pj, aic, sd);pj963})
try({pj964<- 0; mod <- Arima (d2, order=c(9,6,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj964 <- cbind(pj, aic, sd);pj964})
try({pj965<- 0; mod <- Arima (d2, order=c(9,6,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj965 <- cbind(pj, aic, sd);pj965})
try({pj966<- 0; mod <- Arima (d2, order=c(9,6,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj966 <- cbind(pj, aic, sd);pj966})
try({pj967<- 0; mod <- Arima (d2, order=c(9,6,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj967 <- cbind(pj, aic, sd);pj967})
try({pj968<- 0; mod <- Arima (d2, order=c(9,6,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj968 <- cbind(pj, aic, sd);pj968})
try({pj969<- 0; mod <- Arima (d2, order=c(9,6,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj969 <- cbind(pj, aic, sd);pj969})
try({pj970<- 0; mod <- Arima (d2, order=c(9,6,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj970 <- cbind(pj, aic, sd);pj970})
try({pj971<- 0; mod <- Arima (d2, order=c(9,7,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj971 <- cbind(pj, aic, sd);pj971})
try({pj972<- 0; mod <- Arima (d2, order=c(9,7,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj972 <- cbind(pj, aic, sd);pj972})
try({pj973<- 0; mod <- Arima (d2, order=c(9,7,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj973 <- cbind(pj, aic, sd);pj973})
try({pj974<- 0; mod <- Arima (d2, order=c(9,7,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj974 <- cbind(pj, aic, sd);pj974})
try({pj975<- 0; mod <- Arima (d2, order=c(9,7,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj975 <- cbind(pj, aic, sd);pj975})
try({pj976<- 0; mod <- Arima (d2, order=c(9,7,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj976 <- cbind(pj, aic, sd);pj976})
try({pj977<- 0; mod <- Arima (d2, order=c(9,7,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj977 <- cbind(pj, aic, sd);pj977})
try({pj978<- 0; mod <- Arima (d2, order=c(9,7,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj978 <- cbind(pj, aic, sd);pj978})
try({pj979<- 0; mod <- Arima (d2, order=c(9,7,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj979 <- cbind(pj, aic, sd);pj979})
try({pj980<- 0; mod <- Arima (d2, order=c(9,7,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj980 <- cbind(pj, aic, sd);pj980})
try({pj981<- 0; mod <- Arima (d2, order=c(9,8,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj981 <- cbind(pj, aic, sd);pj981})
try({pj982<- 0; mod <- Arima (d2, order=c(9,8,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj982 <- cbind(pj, aic, sd);pj982})
try({pj983<- 0; mod <- Arima (d2, order=c(9,8,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj983 <- cbind(pj, aic, sd);pj983})
try({pj984<- 0; mod <- Arima (d2, order=c(9,8,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj984 <- cbind(pj, aic, sd);pj984})
try({pj985<- 0; mod <- Arima (d2, order=c(9,8,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj985 <- cbind(pj, aic, sd);pj985})
try({pj986<- 0; mod <- Arima (d2, order=c(9,8,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj986 <- cbind(pj, aic, sd);pj986})
try({pj987<- 0; mod <- Arima (d2, order=c(9,8,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj987 <- cbind(pj, aic, sd);pj987})
try({pj988<- 0; mod <- Arima (d2, order=c(9,8,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj988 <- cbind(pj, aic, sd);pj988})
try({pj989<- 0; mod <- Arima (d2, order=c(9,8,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj989 <- cbind(pj, aic, sd);pj989})
try({pj990<- 0; mod <- Arima (d2, order=c(9,8,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj990 <- cbind(pj, aic, sd);pj990})
try({pj991<- 0; mod <- Arima (d2, order=c(9,9,0)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj991 <- cbind(pj, aic, sd);pj991})
try({pj992<- 0; mod <- Arima (d2, order=c(9,9,1)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj992 <- cbind(pj, aic, sd);pj992})
try({pj993<- 0; mod <- Arima (d2, order=c(9,9,2)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj993 <- cbind(pj, aic, sd);pj993})
try({pj994<- 0; mod <- Arima (d2, order=c(9,9,3)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj994 <- cbind(pj, aic, sd);pj994})
try({pj995<- 0; mod <- Arima (d2, order=c(9,9,4)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj995 <- cbind(pj, aic, sd);pj995})
try({pj996<- 0; mod <- Arima (d2, order=c(9,9,5)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj996 <- cbind(pj, aic, sd);pj996})
try({pj997<- 0; mod <- Arima (d2, order=c(9,9,6)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj997 <- cbind(pj, aic, sd);pj997})
try({pj998<- 0; mod <- Arima (d2, order=c(9,9,7)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj998 <- cbind(pj, aic, sd);pj998})
try({pj999<- 0; mod <- Arima (d2, order=c(9,9,8)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj999 <- cbind(pj, aic, sd);pj999})
try({pj1000<- 0; mod <- Arima (d2, order=c(9,9,9)); sd <- sqrt (mod$sigma2); aic <- mod$aic; aic <- as.data.frame (aic); pj <- forecast (mod, h=ts); pj <- as.data.frame (pj$mean); pj<- t(pj); pj1000 <- cbind(pj, aic, sd);pj1000})

#Merge the 1,000 distinct ARIMA models into one out file
arimaname <- read_excel("Dataset.xlsx", sheet="ARIMA")
allarima <- rbind(get0("pj1"), 
                  get0("pj2"), 
                  get0("pj3"),       
                  get0("pj4"),
                  get0("pj5"),
                  get0("pj6"),
                  get0("pj7"),
                  get0("pj8"),
                  get0("pj9"),
                  get0("pj10"),
                  get0("pj11"),
                  get0("pj12"),
                  get0("pj13"),
                  get0("pj14"),
                  get0("pj15"),
                  get0("pj16"),
                  get0("pj17"),
                  get0("pj18"),
                  get0("pj19"),
                  get0("pj20"),
                  get0("pj21"),
                  get0("pj22"),
                  get0("pj23"),
                  get0("pj24"),
                  get0("pj25"),
                  get0("pj26"),
                  get0("pj27"),
                  get0("pj28"),
                  get0("pj29"),
                  get0("pj30"),
                  get0("pj31"),
                  get0("pj32"),
                  get0("pj33"),
                  get0("pj34"),
                  get0("pj35"),
                  get0("pj36"),
                  get0("pj37"),
                  get0("pj38"),
                  get0("pj39"),
                  get0("pj40"),
                  get0("pj41"),
                  get0("pj42"),
                  get0("pj43"),
                  get0("pj44"),
                  get0("pj45"),
                  get0("pj46"),
                  get0("pj47"),
                  get0("pj48"),
                  get0("pj49"),
                  get0("pj50"),
                  get0("pj51"),
                  get0("pj52"),
                  get0("pj53"),
                  get0("pj54"),
                  get0("pj55"),
                  get0("pj56"),
                  get0("pj57"),
                  get0("pj58"),
                  get0("pj59"),
                  get0("pj60"),
                  get0("pj61"),
                  get0("pj62"),
                  get0("pj63"),
                  get0("pj64"),
                  get0("pj65"),
                  get0("pj66"),
                  get0("pj67"),
                  get0("pj68"),
                  get0("pj69"),
                  get0("pj70"),
                  get0("pj71"),
                  get0("pj72"),
                  get0("pj73"),
                  get0("pj74"),
                  get0("pj75"),
                  get0("pj76"),
                  get0("pj77"),
                  get0("pj78"),
                  get0("pj79"),
                  get0("pj80"),
                  get0("pj81"),
                  get0("pj82"),
                  get0("pj83"),
                  get0("pj84"),
                  get0("pj85"),
                  get0("pj86"),
                  get0("pj87"),
                  get0("pj88"),
                  get0("pj89"),
                  get0("pj90"),
                  get0("pj91"),
                  get0("pj92"),
                  get0("pj93"),
                  get0("pj94"),
                  get0("pj95"),
                  get0("pj96"),
                  get0("pj97"),
                  get0("pj98"),
                  get0("pj99"),
                  get0("pj100"),
                  get0("pj101"),
                  get0("pj102"),
                  get0("pj103"),
                  get0("pj104"),
                  get0("pj105"),
                  get0("pj106"),
                  get0("pj107"),
                  get0("pj108"),
                  get0("pj109"),
                  get0("pj110"),
                  get0("pj111"),
                  get0("pj112"),
                  get0("pj113"),
                  get0("pj114"),
                  get0("pj115"),
                  get0("pj116"),
                  get0("pj117"),
                  get0("pj118"),
                  get0("pj119"),
                  get0("pj120"),
                  get0("pj121"),
                  get0("pj122"),
                  get0("pj123"),
                  get0("pj124"),
                  get0("pj125"),
                  get0("pj126"),
                  get0("pj127"),
                  get0("pj128"),
                  get0("pj129"),
                  get0("pj130"),
                  get0("pj131"),
                  get0("pj132"),
                  get0("pj133"),
                  get0("pj134"),
                  get0("pj135"),
                  get0("pj136"),
                  get0("pj137"),
                  get0("pj138"),
                  get0("pj139"),
                  get0("pj140"),
                  get0("pj141"),
                  get0("pj142"),
                  get0("pj143"),
                  get0("pj144"),
                  get0("pj145"),
                  get0("pj146"),
                  get0("pj147"),
                  get0("pj148"),
                  get0("pj149"),
                  get0("pj150"),
                  get0("pj151"),
                  get0("pj152"),
                  get0("pj153"),
                  get0("pj154"),
                  get0("pj155"),
                  get0("pj156"),
                  get0("pj157"),
                  get0("pj158"),
                  get0("pj159"),
                  get0("pj160"),
                  get0("pj161"),
                  get0("pj162"),
                  get0("pj163"),
                  get0("pj164"),
                  get0("pj165"),
                  get0("pj166"),
                  get0("pj167"),
                  get0("pj168"),
                  get0("pj169"),
                  get0("pj170"),
                  get0("pj171"),
                  get0("pj172"),
                  get0("pj173"),
                  get0("pj174"),
                  get0("pj175"),
                  get0("pj176"),
                  get0("pj177"),
                  get0("pj178"),
                  get0("pj179"),
                  get0("pj180"),
                  get0("pj181"),
                  get0("pj182"),
                  get0("pj183"),
                  get0("pj184"),
                  get0("pj185"),
                  get0("pj186"),
                  get0("pj187"),
                  get0("pj188"),
                  get0("pj189"),
                  get0("pj190"),
                  get0("pj191"),
                  get0("pj192"),
                  get0("pj193"),
                  get0("pj194"),
                  get0("pj195"),
                  get0("pj196"),
                  get0("pj197"),
                  get0("pj198"),
                  get0("pj199"),
                  get0("pj200"),
                  get0("pj201"),
                  get0("pj202"),
                  get0("pj203"),
                  get0("pj204"),
                  get0("pj205"),
                  get0("pj206"),
                  get0("pj207"),
                  get0("pj208"),
                  get0("pj209"),
                  get0("pj210"),
                  get0("pj211"),
                  get0("pj212"),
                  get0("pj213"),
                  get0("pj214"),
                  get0("pj215"),
                  get0("pj216"),
                  get0("pj217"),
                  get0("pj218"),
                  get0("pj219"),
                  get0("pj220"),
                  get0("pj221"),
                  get0("pj222"),
                  get0("pj223"),
                  get0("pj224"),
                  get0("pj225"),
                  get0("pj226"),
                  get0("pj227"),
                  get0("pj228"),
                  get0("pj229"),
                  get0("pj230"),
                  get0("pj231"),
                  get0("pj232"),
                  get0("pj233"),
                  get0("pj234"),
                  get0("pj235"),
                  get0("pj236"),
                  get0("pj237"),
                  get0("pj238"),
                  get0("pj239"),
                  get0("pj240"),
                  get0("pj241"),
                  get0("pj242"),
                  get0("pj243"),
                  get0("pj244"),
                  get0("pj245"),
                  get0("pj246"),
                  get0("pj247"),
                  get0("pj248"),
                  get0("pj249"),
                  get0("pj250"),
                  get0("pj251"),
                  get0("pj252"),
                  get0("pj253"),
                  get0("pj254"),
                  get0("pj255"),
                  get0("pj256"),
                  get0("pj257"),
                  get0("pj258"),
                  get0("pj259"),
                  get0("pj260"),
                  get0("pj261"),
                  get0("pj262"),
                  get0("pj263"),
                  get0("pj264"),
                  get0("pj265"),
                  get0("pj266"),
                  get0("pj267"),
                  get0("pj268"),
                  get0("pj269"),
                  get0("pj270"),
                  get0("pj271"),
                  get0("pj272"),
                  get0("pj273"),
                  get0("pj274"),
                  get0("pj275"),
                  get0("pj276"),
                  get0("pj277"),
                  get0("pj278"),
                  get0("pj279"),
                  get0("pj280"),
                  get0("pj281"),
                  get0("pj282"),
                  get0("pj283"),
                  get0("pj284"),
                  get0("pj285"),
                  get0("pj286"),
                  get0("pj287"),
                  get0("pj288"),
                  get0("pj289"),
                  get0("pj290"),
                  get0("pj291"),
                  get0("pj292"),
                  get0("pj293"),
                  get0("pj294"),
                  get0("pj295"),
                  get0("pj296"),
                  get0("pj297"),
                  get0("pj298"),
                  get0("pj299"),
                  get0("pj300"),
                  get0("pj301"),
                  get0("pj302"),
                  get0("pj303"),
                  get0("pj304"),
                  get0("pj305"),
                  get0("pj306"),
                  get0("pj307"),
                  get0("pj308"),
                  get0("pj309"),
                  get0("pj310"),
                  get0("pj311"),
                  get0("pj312"),
                  get0("pj313"),
                  get0("pj314"),
                  get0("pj315"),
                  get0("pj316"),
                  get0("pj317"),
                  get0("pj318"),
                  get0("pj319"),
                  get0("pj320"),
                  get0("pj321"),
                  get0("pj322"),
                  get0("pj323"),
                  get0("pj324"),
                  get0("pj325"),
                  get0("pj326"),
                  get0("pj327"),
                  get0("pj328"),
                  get0("pj329"),
                  get0("pj330"),
                  get0("pj331"),
                  get0("pj332"),
                  get0("pj333"),
                  get0("pj334"),
                  get0("pj335"),
                  get0("pj336"),
                  get0("pj337"),
                  get0("pj338"),
                  get0("pj339"),
                  get0("pj340"),
                  get0("pj341"),
                  get0("pj342"),
                  get0("pj343"),
                  get0("pj344"),
                  get0("pj345"),
                  get0("pj346"),
                  get0("pj347"),
                  get0("pj348"),
                  get0("pj349"),
                  get0("pj350"),
                  get0("pj351"),
                  get0("pj352"),
                  get0("pj353"),
                  get0("pj354"),
                  get0("pj355"),
                  get0("pj356"),
                  get0("pj357"),
                  get0("pj358"),
                  get0("pj359"),
                  get0("pj360"),
                  get0("pj361"),
                  get0("pj362"),
                  get0("pj363"),
                  get0("pj364"),
                  get0("pj365"),
                  get0("pj366"),
                  get0("pj367"),
                  get0("pj368"),
                  get0("pj369"),
                  get0("pj370"),
                  get0("pj371"),
                  get0("pj372"),
                  get0("pj373"),
                  get0("pj374"),
                  get0("pj375"),
                  get0("pj376"),
                  get0("pj377"),
                  get0("pj378"),
                  get0("pj379"),
                  get0("pj380"),
                  get0("pj381"),
                  get0("pj382"),
                  get0("pj383"),
                  get0("pj384"),
                  get0("pj385"),
                  get0("pj386"),
                  get0("pj387"),
                  get0("pj388"),
                  get0("pj389"),
                  get0("pj390"),
                  get0("pj391"),
                  get0("pj392"),
                  get0("pj393"),
                  get0("pj394"),
                  get0("pj395"),
                  get0("pj396"),
                  get0("pj397"),
                  get0("pj398"),
                  get0("pj399"),
                  get0("pj400"),
                  get0("pj401"),
                  get0("pj402"),
                  get0("pj403"),
                  get0("pj404"),
                  get0("pj405"),
                  get0("pj406"),
                  get0("pj407"),
                  get0("pj408"),
                  get0("pj409"),
                  get0("pj410"),
                  get0("pj411"),
                  get0("pj412"),
                  get0("pj413"),
                  get0("pj414"),
                  get0("pj415"),
                  get0("pj416"),
                  get0("pj417"),
                  get0("pj418"),
                  get0("pj419"),
                  get0("pj420"),
                  get0("pj421"),
                  get0("pj422"),
                  get0("pj423"),
                  get0("pj424"),
                  get0("pj425"),
                  get0("pj426"),
                  get0("pj427"),
                  get0("pj428"),
                  get0("pj429"),
                  get0("pj430"),
                  get0("pj431"),
                  get0("pj432"),
                  get0("pj433"),
                  get0("pj434"),
                  get0("pj435"),
                  get0("pj436"),
                  get0("pj437"),
                  get0("pj438"),
                  get0("pj439"),
                  get0("pj440"),
                  get0("pj441"),
                  get0("pj442"),
                  get0("pj443"),
                  get0("pj444"),
                  get0("pj445"),
                  get0("pj446"),
                  get0("pj447"),
                  get0("pj448"),
                  get0("pj449"),
                  get0("pj450"),
                  get0("pj451"),
                  get0("pj452"),
                  get0("pj453"),
                  get0("pj454"),
                  get0("pj455"),
                  get0("pj456"),
                  get0("pj457"),
                  get0("pj458"),
                  get0("pj459"),
                  get0("pj460"),
                  get0("pj461"),
                  get0("pj462"),
                  get0("pj463"),
                  get0("pj464"),
                  get0("pj465"),
                  get0("pj466"),
                  get0("pj467"),
                  get0("pj468"),
                  get0("pj469"),
                  get0("pj470"),
                  get0("pj471"),
                  get0("pj472"),
                  get0("pj473"),
                  get0("pj474"),
                  get0("pj475"),
                  get0("pj476"),
                  get0("pj477"),
                  get0("pj478"),
                  get0("pj479"),
                  get0("pj480"),
                  get0("pj481"),
                  get0("pj482"),
                  get0("pj483"),
                  get0("pj484"),
                  get0("pj485"),
                  get0("pj486"),
                  get0("pj487"),
                  get0("pj488"),
                  get0("pj489"),
                  get0("pj490"),
                  get0("pj491"),
                  get0("pj492"),
                  get0("pj493"),
                  get0("pj494"),
                  get0("pj495"),
                  get0("pj496"),
                  get0("pj497"),
                  get0("pj498"),
                  get0("pj499"),
                  get0("pj500"),
                  get0("pj501"),
                  get0("pj502"),
                  get0("pj503"),
                  get0("pj504"),
                  get0("pj505"),
                  get0("pj506"),
                  get0("pj507"),
                  get0("pj508"),
                  get0("pj509"),
                  get0("pj510"),
                  get0("pj511"),
                  get0("pj512"),
                  get0("pj513"),
                  get0("pj514"),
                  get0("pj515"),
                  get0("pj516"),
                  get0("pj517"),
                  get0("pj518"),
                  get0("pj519"),
                  get0("pj520"),
                  get0("pj521"),
                  get0("pj522"),
                  get0("pj523"),
                  get0("pj524"),
                  get0("pj525"),
                  get0("pj526"),
                  get0("pj527"),
                  get0("pj528"),
                  get0("pj529"),
                  get0("pj530"),
                  get0("pj531"),
                  get0("pj532"),
                  get0("pj533"),
                  get0("pj534"),
                  get0("pj535"),
                  get0("pj536"),
                  get0("pj537"),
                  get0("pj538"),
                  get0("pj539"),
                  get0("pj540"),
                  get0("pj541"),
                  get0("pj542"),
                  get0("pj543"),
                  get0("pj544"),
                  get0("pj545"),
                  get0("pj546"),
                  get0("pj547"),
                  get0("pj548"),
                  get0("pj549"),
                  get0("pj550"),
                  get0("pj551"),
                  get0("pj552"),
                  get0("pj553"),
                  get0("pj554"),
                  get0("pj555"),
                  get0("pj556"),
                  get0("pj557"),
                  get0("pj558"),
                  get0("pj559"),
                  get0("pj560"),
                  get0("pj561"),
                  get0("pj562"),
                  get0("pj563"),
                  get0("pj564"),
                  get0("pj565"),
                  get0("pj566"),
                  get0("pj567"),
                  get0("pj568"),
                  get0("pj569"),
                  get0("pj570"),
                  get0("pj571"),
                  get0("pj572"),
                  get0("pj573"),
                  get0("pj574"),
                  get0("pj575"),
                  get0("pj576"),
                  get0("pj577"),
                  get0("pj578"),
                  get0("pj579"),
                  get0("pj580"),
                  get0("pj581"),
                  get0("pj582"),
                  get0("pj583"),
                  get0("pj584"),
                  get0("pj585"),
                  get0("pj586"),
                  get0("pj587"),
                  get0("pj588"),
                  get0("pj589"),
                  get0("pj590"),
                  get0("pj591"),
                  get0("pj592"),
                  get0("pj593"),
                  get0("pj594"),
                  get0("pj595"),
                  get0("pj596"),
                  get0("pj597"),
                  get0("pj598"),
                  get0("pj599"),
                  get0("pj600"),
                  get0("pj601"),
                  get0("pj602"),
                  get0("pj603"),
                  get0("pj604"),
                  get0("pj605"),
                  get0("pj606"),
                  get0("pj607"),
                  get0("pj608"),
                  get0("pj609"),
                  get0("pj610"),
                  get0("pj611"),
                  get0("pj612"),
                  get0("pj613"),
                  get0("pj614"),
                  get0("pj615"),
                  get0("pj616"),
                  get0("pj617"),
                  get0("pj618"),
                  get0("pj619"),
                  get0("pj620"),
                  get0("pj621"),
                  get0("pj622"),
                  get0("pj623"),
                  get0("pj624"),
                  get0("pj625"),
                  get0("pj626"),
                  get0("pj627"),
                  get0("pj628"),
                  get0("pj629"),
                  get0("pj630"),
                  get0("pj631"),
                  get0("pj632"),
                  get0("pj633"),
                  get0("pj634"),
                  get0("pj635"),
                  get0("pj636"),
                  get0("pj637"),
                  get0("pj638"),
                  get0("pj639"),
                  get0("pj640"),
                  get0("pj641"),
                  get0("pj642"),
                  get0("pj643"),
                  get0("pj644"),
                  get0("pj645"),
                  get0("pj646"),
                  get0("pj647"),
                  get0("pj648"),
                  get0("pj649"),
                  get0("pj650"),
                  get0("pj651"),
                  get0("pj652"),
                  get0("pj653"),
                  get0("pj654"),
                  get0("pj655"),
                  get0("pj656"),
                  get0("pj657"),
                  get0("pj658"),
                  get0("pj659"),
                  get0("pj660"),
                  get0("pj661"),
                  get0("pj662"),
                  get0("pj663"),
                  get0("pj664"),
                  get0("pj665"),
                  get0("pj666"),
                  get0("pj667"),
                  get0("pj668"),
                  get0("pj669"),
                  get0("pj670"),
                  get0("pj671"),
                  get0("pj672"),
                  get0("pj673"),
                  get0("pj674"),
                  get0("pj675"),
                  get0("pj676"),
                  get0("pj677"),
                  get0("pj678"),
                  get0("pj679"),
                  get0("pj680"),
                  get0("pj681"),
                  get0("pj682"),
                  get0("pj683"),
                  get0("pj684"),
                  get0("pj685"),
                  get0("pj686"),
                  get0("pj687"),
                  get0("pj688"),
                  get0("pj689"),
                  get0("pj690"),
                  get0("pj691"),
                  get0("pj692"),
                  get0("pj693"),
                  get0("pj694"),
                  get0("pj695"),
                  get0("pj696"),
                  get0("pj697"),
                  get0("pj698"),
                  get0("pj699"),
                  get0("pj700"),
                  get0("pj701"),
                  get0("pj702"),
                  get0("pj703"),
                  get0("pj704"),
                  get0("pj705"),
                  get0("pj706"),
                  get0("pj707"),
                  get0("pj708"),
                  get0("pj709"),
                  get0("pj710"),
                  get0("pj711"),
                  get0("pj712"),
                  get0("pj713"),
                  get0("pj714"),
                  get0("pj715"),
                  get0("pj716"),
                  get0("pj717"),
                  get0("pj718"),
                  get0("pj719"),
                  get0("pj720"),
                  get0("pj721"),
                  get0("pj722"),
                  get0("pj723"),
                  get0("pj724"),
                  get0("pj725"),
                  get0("pj726"),
                  get0("pj727"),
                  get0("pj728"),
                  get0("pj729"),
                  get0("pj730"),
                  get0("pj731"),
                  get0("pj732"),
                  get0("pj733"),
                  get0("pj734"),
                  get0("pj735"),
                  get0("pj736"),
                  get0("pj737"),
                  get0("pj738"),
                  get0("pj739"),
                  get0("pj740"),
                  get0("pj741"),
                  get0("pj742"),
                  get0("pj743"),
                  get0("pj744"),
                  get0("pj745"),
                  get0("pj746"),
                  get0("pj747"),
                  get0("pj748"),
                  get0("pj749"),
                  get0("pj750"),
                  get0("pj751"),
                  get0("pj752"),
                  get0("pj753"),
                  get0("pj754"),
                  get0("pj755"),
                  get0("pj756"),
                  get0("pj757"),
                  get0("pj758"),
                  get0("pj759"),
                  get0("pj760"),
                  get0("pj761"),
                  get0("pj762"),
                  get0("pj763"),
                  get0("pj764"),
                  get0("pj765"),
                  get0("pj766"),
                  get0("pj767"),
                  get0("pj768"),
                  get0("pj769"),
                  get0("pj770"),
                  get0("pj771"),
                  get0("pj772"),
                  get0("pj773"),
                  get0("pj774"),
                  get0("pj775"),
                  get0("pj776"),
                  get0("pj777"),
                  get0("pj778"),
                  get0("pj779"),
                  get0("pj780"),
                  get0("pj781"),
                  get0("pj782"),
                  get0("pj783"),
                  get0("pj784"),
                  get0("pj785"),
                  get0("pj786"),
                  get0("pj787"),
                  get0("pj788"),
                  get0("pj789"),
                  get0("pj790"),
                  get0("pj791"),
                  get0("pj792"),
                  get0("pj793"),
                  get0("pj794"),
                  get0("pj795"),
                  get0("pj796"),
                  get0("pj797"),
                  get0("pj798"),
                  get0("pj799"),
                  get0("pj800"),
                  get0("pj801"),
                  get0("pj802"),
                  get0("pj803"),
                  get0("pj804"),
                  get0("pj805"),
                  get0("pj806"),
                  get0("pj807"),
                  get0("pj808"),
                  get0("pj809"),
                  get0("pj810"),
                  get0("pj811"),
                  get0("pj812"),
                  get0("pj813"),
                  get0("pj814"),
                  get0("pj815"),
                  get0("pj816"),
                  get0("pj817"),
                  get0("pj818"),
                  get0("pj819"),
                  get0("pj820"),
                  get0("pj821"),
                  get0("pj822"),
                  get0("pj823"),
                  get0("pj824"),
                  get0("pj825"),
                  get0("pj826"),
                  get0("pj827"),
                  get0("pj828"),
                  get0("pj829"),
                  get0("pj830"),
                  get0("pj831"),
                  get0("pj832"),
                  get0("pj833"),
                  get0("pj834"),
                  get0("pj835"),
                  get0("pj836"),
                  get0("pj837"),
                  get0("pj838"),
                  get0("pj839"),
                  get0("pj840"),
                  get0("pj841"),
                  get0("pj842"),
                  get0("pj843"),
                  get0("pj844"),
                  get0("pj845"),
                  get0("pj846"),
                  get0("pj847"),
                  get0("pj848"),
                  get0("pj849"),
                  get0("pj850"),
                  get0("pj851"),
                  get0("pj852"),
                  get0("pj853"),
                  get0("pj854"),
                  get0("pj855"),
                  get0("pj856"),
                  get0("pj857"),
                  get0("pj858"),
                  get0("pj859"),
                  get0("pj860"),
                  get0("pj861"),
                  get0("pj862"),
                  get0("pj863"),
                  get0("pj864"),
                  get0("pj865"),
                  get0("pj866"),
                  get0("pj867"),
                  get0("pj868"),
                  get0("pj869"),
                  get0("pj870"),
                  get0("pj871"),
                  get0("pj872"),
                  get0("pj873"),
                  get0("pj874"),
                  get0("pj875"),
                  get0("pj876"),
                  get0("pj877"),
                  get0("pj878"),
                  get0("pj879"),
                  get0("pj880"),
                  get0("pj881"),
                  get0("pj882"),
                  get0("pj883"),
                  get0("pj884"),
                  get0("pj885"),
                  get0("pj886"),
                  get0("pj887"),
                  get0("pj888"),
                  get0("pj889"),
                  get0("pj890"),
                  get0("pj891"),
                  get0("pj892"),
                  get0("pj893"),
                  get0("pj894"),
                  get0("pj895"),
                  get0("pj896"),
                  get0("pj897"),
                  get0("pj898"),
                  get0("pj899"),
                  get0("pj900"),
                  get0("pj901"),
                  get0("pj902"),
                  get0("pj903"),
                  get0("pj904"),
                  get0("pj905"),
                  get0("pj906"),
                  get0("pj907"),
                  get0("pj908"),
                  get0("pj909"),
                  get0("pj910"),
                  get0("pj911"),
                  get0("pj912"),
                  get0("pj913"),
                  get0("pj914"),
                  get0("pj915"),
                  get0("pj916"),
                  get0("pj917"),
                  get0("pj918"),
                  get0("pj919"),
                  get0("pj920"),
                  get0("pj921"),
                  get0("pj922"),
                  get0("pj923"),
                  get0("pj924"),
                  get0("pj925"),
                  get0("pj926"),
                  get0("pj927"),
                  get0("pj928"),
                  get0("pj929"),
                  get0("pj930"),
                  get0("pj931"),
                  get0("pj932"),
                  get0("pj933"),
                  get0("pj934"),
                  get0("pj935"),
                  get0("pj936"),
                  get0("pj937"),
                  get0("pj938"),
                  get0("pj939"),
                  get0("pj940"),
                  get0("pj941"),
                  get0("pj942"),
                  get0("pj943"),
                  get0("pj944"),
                  get0("pj945"),
                  get0("pj946"),
                  get0("pj947"),
                  get0("pj948"),
                  get0("pj949"),
                  get0("pj950"),
                  get0("pj951"),
                  get0("pj952"),
                  get0("pj953"),
                  get0("pj954"),
                  get0("pj955"),
                  get0("pj956"),
                  get0("pj957"),
                  get0("pj958"),
                  get0("pj959"),
                  get0("pj960"),
                  get0("pj961"),
                  get0("pj962"),
                  get0("pj963"),
                  get0("pj964"),
                  get0("pj965"),
                  get0("pj966"),
                  get0("pj967"),
                  get0("pj968"),
                  get0("pj969"),
                  get0("pj970"),
                  get0("pj971"),
                  get0("pj972"),
                  get0("pj973"),
                  get0("pj974"),
                  get0("pj975"),
                  get0("pj976"),
                  get0("pj977"),
                  get0("pj978"),
                  get0("pj979"),
                  get0("pj980"),
                  get0("pj981"),
                  get0("pj982"),
                  get0("pj983"),
                  get0("pj984"),
                  get0("pj985"),
                  get0("pj986"),
                  get0("pj987"),
                  get0("pj988"),
                  get0("pj989"),
                  get0("pj990"),
                  get0("pj991"),
                  get0("pj992"),
                  get0("pj993"),
                  get0("pj994"),
                  get0("pj995"),
                  get0("pj996"),
                  get0("pj997"),
                  get0("pj998"),
                  get0("pj999"),
                  get0("pj1000"))

allarima <- cbind(allarima, arimaname); out1000arima <- allarima[order(allarima$aic),]; out1000arima <- out1000arima %>%  filter(aic !='0')

#Select the 100 most fitted ARIMA and creating the scenarios
out1000arima <- out1000arima  %>% select(-aic, -sd, -ARIMA); out100arima <- head(out1000arima, 100); km <- kmeans(out100arima, 3)
kmcluster <- as.data.frame(km$cluster); colnames(kmcluster) <- c('km'); out100arima <- cbind (out100arima, kmcluster)
s1 <- filter(out100arima, km == '1'); s2 <- filter(out100arima, km == '2'); s3 <- filter(out100arima, km == '3'); s1 <- apply(s1, 2, sd, na.rm = TRUE); s2 <- apply(s2, 2, sd, na.rm = TRUE); s3 <- apply(s3, 2, sd, na.rm = TRUE)
kmcenters <- as.data.frame(km$centers); kmsize <- as.data.frame(km$size); colnames(kmsize) <- c('likelihood'); scenarios <- cbind (kmcenters, kmsize); scenarios <- rbind (scenarios, s1, s2, s3)
rownames(scenarios) <- c('scenario 1 projections', 'scenario 2 projections', 'scenario 3 projections', 'scenario 1 stddev', 'scenario 2 stddev', 'scenario 3 stddev'); scenarios <- scenarios[ order(row.names(scenarios)), ]
d3 <- as.data.frame (d1$Year); d2 <- as.data.frame(d2); d4 <- cbind (d3, d2); d4 <- as.data.frame(d4); colnames(d4) <- c('x', "y"); d4 <- cbind(d4, y2="0", y3="0")
d5 <- t(kmcenters); colnames(d5) <- c('y', "y2", "y3"); d5 <- as.data.frame (d5); d5$x <- row.names(d5); d5 <- d5 %>% relocate(x, .before=y)
d4$x <- as.numeric(d4$x); d4$y <- as.numeric(d4$y); d4$y2 <- as.numeric(d4$y2); d4$y3 <- as.numeric(d4$y3); 
d5$x <- as.numeric(d5$x); d5$y <- as.numeric(d5$y); d5$y2 <- as.numeric(d5$y2); d5$y3 <- as.numeric(d5$y3)
d6 <- rbind (d4, d5)

#Calculate the per year rates in the given ts
t <- d6[01:11, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t1 <- linearcoef/av_t*100
t <- d6[11:21, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t2 <- linearcoef/av_t*100
t <- d6[21:31, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t3 <- linearcoef/av_t*100
t <- d6[31:41, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t4 <- linearcoef/av_t*100
t <- d6[41:51, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t5 <- linearcoef/av_t*100
t <- d6[51:61, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t6 <- linearcoef/av_t*100
t <- d6[61:64, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t7 <- linearcoef/av_t*100
t <- d6[65:71, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t8_scenario1 <- linearcoef/av_t*100;
t <- d6[65:71, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y2 ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t8_scenario2 <- linearcoef/av_t*100
t <- d6[65:71, ]; t <- as.data.frame(t); av_t <- mean(t$y); t <- lm(y3 ~ x, d = t); t <- as.data.frame(t$coefficients); linearcoef <- t[2:2,]; rate_perc_t8_scenario3 <- linearcoef/av_t*100

rates <- rbind (rate_perc_t1, rate_perc_t2, rate_perc_t3, rate_perc_t4, rate_perc_t5,
                rate_perc_t6, rate_perc_t7, rate_perc_t8_scenario1, rate_perc_t8_scenario2, rate_perc_t8_scenario3)

rates <- as.data.frame(rates); rates$t <- row.names(rates); colnames(rates) <- c('rate', 't'); rates <- rates %>% relocate(t, .before=rate); rates <- t(rates); rates <- as.data.frame(rates)

#Print and export outs
out1000arima; write_xlsx(out1000arima, "1_China, 1000 ARIMAs.xlsx")
out100arima; write_xlsx(out100arima, "1_China, best 100 ARIMAs with k-mean group.xlsx")
scenarios; write_xlsx(scenarios, "1_China, scenarios.xlsx")
rates; write_xlsx(rates, "1_China, rates.xlsx")