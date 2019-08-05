validationSampler1(mdata,1:29,30:33,1:48)
create("cats")
setwd("./cats")
document()
document()
document()
setwd("..")
install("cats")
?cat_function
?cat_function()
?cat_function
library(cats)
?cat_function
getwd()
search()
detach("cats")
detach("package:cats")
?cat_function
library(cats)
?cat_function
cat_function(1)
cat_function(0)
?cat_function
?melt
ls("package:cats")
help(package = cats)
create("ST_auto_1")
source('~/.active-rstudio-document')
setwd("D:/Dropbox/Sandbox/ST_auto_1")
create("ST_auto_1")
library(devtools)
library(roxygen2)
create("ST_auto_1")
create("ST.auto.1")
setwd("./ST.auto.1")
document()
library(ST.auto.1)
?ST.auto.1
?dplyr
help(package = "ST.auto.1")
help(package = "dplyr")
devtools::use_package()
devtools::use_package('data.table')
devtools::use_package(data.table,ggplot2)
devtools::use_package("data.table","ggplot2")
devtools::use_package(c("data.table","ggplot2"))
devtools::use_package('ggplot2')
?.
macro_data <- readxl::read_excel("./Data/MacroDataAC.xlsx") # last 14 rows of predictions of MEVs are dummy, copy pasted from cells above
document()
document()
?.
devtools::install()
.?
?.
getwd()
help(package = "ST.auto.1")
library(ST.auto.1)
validationSampler(macroecodata)
library(ST.auto.1)
document()
devtools::install()
devtools::install()
ls()
ls
ls()
ls
ls()
library(ST.auto.1)
validationSampler()
getwd()
install(pkg = "ST.auto.1")
setwd("D:/Dropbox/Sandbox/ST_auto_1")
install(pkg = "ST.auto.1")
library(ST.auto.1)
install('ST.auto.1')
install(pkg = "ST.auto.1")
library(ST.auto.1)
document()
setwd("D:/Dropbox/Sandbox/ST_auto_1/ST.auto.1/R")
cd..
setwd("D:/Dropbox/Sandbox/ST_auto_1/ST.auto.1")
document()
install(pkg = "ST.auto.1")
setwd("D:/Dropbox/Sandbox/ST_auto_1")
install(pkg = "ST.auto.1")
library(ST.auto.1)
setwd("D:/Dropbox/Sandbox/Codes for  ADF automation")
readxl::read_excel("./Data/MacroDataAC.xlsx")
setwd("D:/Dropbox/Sandbox/ST_auto_1/ST.auto.1")
document()
setwd("..")
getwd()
install("ST.auto.1")
validationSampler(mdata,1:29,30:33,1:48)
validationSampler <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list(train_df,test_df,forecast_df), envir = .GlobalEnv)
}
validationSampler <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list("train_df","test_df","forecast_df"), envir = .GlobalEnv)
}
validationSampler(mdata,1:29,30:33,1:48)
validationSampler1 <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list("train_df","test_df","forecast_df"), envir = .GlobalEnv)
}
validationSampler1(mdata,1:29,30:33,1:48)
list2env(mdata)
list2env(list(mdata))
list2env(list(mdata=mdata))
validationSampler <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list(train_df=train_df,test_df=test_df,forecast_df=forecast_df), envir = .GlobalEnv)
}
validationSampler1(mdata,1:29,30:33,1:48)
validationSampler1 <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list(train_df=train_df,test_df=test_df,forecast_df=forecast_df), envir = .GlobalEnv)
}
validationSampler1(mdata,1:29,30:33,1:48)
rm(forecast_df)
rm(*df)
rm(test_df)
rm(train_df)
getwd()
setwd("ST.auto.1")
document()
setwd("../")
install()
setwd("ST.auto.1")
install()
library(ST.auto.1)
rm(validationSampler())
rm(validationSampler
)
rm(validationSampler1)
library(ST.auto.1)
validationSampler
validationSampler1(mdata,1:29,30:33,1:48)
validationSampler(mdata,1:29,30:33,1:48)
class(forecast_df)
allRegressionFormulae <- function(vars, no_of_vars){
no_of_vars <- 3
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(vars, x))
})
LHS <- "DR ~ "
allModelsList<-paste0(LHS,rbindlist(lapply(all_combinations,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
}
vec<-"ECI_yoy_ch_3QMA_lag_4 + avg_oil_pri_barrel_3QMA + Rl_est_Dub_q_yoy_ch_lag_1 + avg_oil_pri_barrel_3QMA_lag_2 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + Non_oil_ECI_yoy_ch_6QMA + avg_oil_pri_barrel + avg_oil_pri_barrel_3QMA_lag_1 + avg_oil_pri_barrel_6QMA_lag_1 + avg_oil_pri_barrel_lag_2 + avg_oil_pri_barrel_lag_3"
strsplit(vec,"[+]")
trimws(unlist(strsplit(vec,"[+]")))
vars<-trimws(unlist(strsplit(vec,"[+]")))
viewxl:::view_in_xl()
vars<-c("ECI_yoy_ch_3QMA_lag_4", "avg_oil_pri_barrel_3QMA", "Rl_est_Dub_q_yoy_ch_lag_1", "avg_oil_pri_barrel_3QMA_lag_2", "Non_oil_ECI_yoy_ch_3QMA_lag_3", "Non_oil_ECI_yoy_ch_6QMA", "avg_oil_pri_barrel", "avg_oil_pri_barrel_3QMA_lag_1", "avg_oil_pri_barrel_6QMA_lag_1", "avg_oil_pri_barrel_lag_2", "avg_oil_pri_barrel_lag_3")
paste0(vars,collapse="+")
paste0(vars,collapse=" + ")
RHS<-paste0(vars,collapse=" + ")
vars
allRegressionFormulae <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
}
allRegressionFormulae <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
return(allModelsList)
}
allRegressionFormulae("DR",vars,4)
allModelsResults<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allRegressionFormulae <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
allModelsResults<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allModelsResults <- lapply(allModelsList,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allModelsList
return(allModelsResults)
}
allModelsResults("DR",vars,4)
allModelsResultsF <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allModelsResults <- lapply(allModelsList,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allModelsList
return(allModelsResults)
}
allModelsResultsF("DR",vars,4)
t<_allModelsResultsF("DR",vars,4)
t<-allModelsResultsF("DR",vars,4)
t
t[[1]]
t[[2]]
length(t)
allModelsResults<-modelDeveloper("DR",vars,4)
allModelsResultsF <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
modelDeveloper <-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allModelsResults <- lapply(allModelsList,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allModelsList
return(allModelsResults)
}
source('D:/Dropbox/Sandbox/ST_auto_1/ST.auto.1/R/validationSampler.R', echo=TRUE)
allModelsResultsF <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
modelDeveloper <-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allModelsResults <- lapply(allModelsList,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allModelsList
return(allModelsResults)
}
allModelsResults<-modelDeveloper("DR",vars,4)
allModels <-modelDeveloper("DR",vars,4)
modelDeveloper <- function(LHS_vars, RHS_vars,no_of_vars){
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
LHS <- "DR ~ "
allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula)
allModelsResults <- lapply(allModelsList,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allModelsList
return(allModelsResults)
}
allModels <-modelDeveloper("DR",vars,4)
t<-c(a,b)
t<-c("a","b")
lapply(list(t))
list(t)
as.list(t)
RHS_vars<- vars
RHS_vars
no_of_vars<-3
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS
LHS <- c("DR ~ ","DR_1 ~")
RHS<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
RHS
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
RHS_all
lapply(as.list(LHS))
LHS<-c("DR ~ ","DR_1 ~")
LHS<-c("DR","DR_1")
LHS_all <- paste0(LHS," ~ ")
LHS_all
CJ(LHS_all,RHS_all)
unlist(CJ(LHS_all,RHS_all))
no_of_vars<-3
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS
LHS<-c("DR","DR_1")
LHS_all <- paste0(LHS," ~ ")
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
sapply(LHS_all, function(x) {paste0(x,RHS_all)})
allFormulae<-sapply(LHS_all, function(x) {paste0(x,RHS_all)})
viewxl:::view_in_xl()
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
length(RHS)
length(RHS)
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
nrow(RHS_all)
length(RHS_all)
allFormulae
allFormulae<-lapply(LHS_all, function(x) {paste0(x,RHS_all)})
View(allFormulae)
unlist(allFormulae)
allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
allFormulae
modelDeveloper <- function(LHS_vars, RHS_vars,no_of_vars){
# RHS <- lapply(1:no_of_vars, function(x) {
#   t(combn(RHS_vars, x))
# })
#
# LHS <- "DR ~ "
#
# RHS<- rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula
#
# lapply(as.list(LHS))
# allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula)
LHS_all <- paste0(LHS," ~ ")
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
allModelsResults <- lapply(allFormulae,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allFormulae
return(allModelsResults)
}
modelDeveloper("DR", vars,no_of_vars=3)
modelDeveloper <- function(LHS_vars, RHS_vars,no_of_vars){
# RHS <- lapply(1:no_of_vars, function(x) {
#   t(combn(RHS_vars, x))
# })
#
# LHS <- "DR ~ "
#
# RHS<- rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula
#
# lapply(as.list(LHS))
# allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula)
browser()
LHS_all <- paste0(LHS," ~ ")
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
allModelsResults <- lapply(allFormulae,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allFormulae
return(allModelsResults)
}
modelDeveloper("DR", vars,no_of_vars=3)
RHS_all
modelDeveloper("DR", vars,no_of_vars=3)
modelDeveloper("DR", vars,no_of_vars=3)
allFormulae
LHS
LHS_vars
LHS_all
modelDeveloper <- function(LHS_vars, RHS_vars,no_of_vars){
# RHS <- lapply(1:no_of_vars, function(x) {
#   t(combn(RHS_vars, x))
# })
#
# LHS <- "DR ~ "
#
# RHS<- rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula
#
# lapply(as.list(LHS))
# allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula)
browser()
LHS_all <- paste0(LHS_vars," ~ ")
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
allModelsResults <- lapply(allFormulae,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allFormulae
return(allModelsResults)
}
modelDeveloper("DR", vars,no_of_vars=3)
temp<-modelDeveloper("DR", vars,no_of_vars=3)
temp[[1]]
modelDeveloper <- function(LHS_vars, RHS_vars,no_of_vars){
# RHS <- lapply(1:no_of_vars, function(x) {
#   t(combn(RHS_vars, x))
# })
#
# LHS <- "DR ~ "
#
# RHS<- rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula
#
# lapply(as.list(LHS))
# allModelsList<-paste0(LHS,rbindlist(lapply(RHS,function(x){
#   x<-data.table(x)
#   x[,formula := do.call(paste, c(.SD,sep=" + "))]
# }), fill=TRUE)$formula)
browser()
LHS_all <- paste0(LHS_vars," ~ ")
RHS <- lapply(1:no_of_vars, function(x) {
t(combn(RHS_vars, x))
})
RHS_all<- rbindlist(lapply(RHS,function(x){
x<-data.table(x)
x[,formula := do.call(paste, c(.SD,sep=" + "))]
}), fill=TRUE)$formula
allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
allModelsResults <- lapply(allFormulae,
function(x) lm(as.formula(x), data = train_df)
)
names(allModelsResults)<- allFormulae
return(allModelsResults)
}
modelDeveloper("DR",vars,2)
getwd()
document()
install()
library(ST.auto.1)
savehistory("D:/Dropbox/Sandbox/ST_auto_1/ST.auto.1/history.Rhistory")
