# mdata<-readxl::read_excel("data/MacroDataAC.xlsx")
validationSampler(macrodata,1:29,30:33,1:48)



vars<-c("ECI_yoy_ch_3QMA_lag_4", "avg_oil_pri_barrel_3QMA", "Rl_est_Dub_q_yoy_ch_lag_1", "avg_oil_pri_barrel_3QMA_lag_2", "Non_oil_ECI_yoy_ch_3QMA_lag_3", "Non_oil_ECI_yoy_ch_6QMA", "avg_oil_pri_barrel", "avg_oil_pri_barrel_3QMA_lag_1", "avg_oil_pri_barrel_6QMA_lag_1", "avg_oil_pri_barrel_lag_2", "avg_oil_pri_barrel_lag_3")
allModels<-modelDeveloper("DR",vars, trainData = train_df,no_of_vars = 2)
allModelsDiagnostics<-modelDiagnostics(allModels)
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)
allModelEvaluated[FinalResults=="PASS",]



validationSampler(macrodata,1:29,30:33,1:48)
model_1<- list(LHS="DR", RHS=c("avg_oil_pri_barrel_lag_3","avg_oil_pri_barrel_lag_2"))

debugonce(reporter)

url_id <- "https://cran.r-project.org/src/contrib/Archive/rmarkdown/rmarkdown_1.11.tar.gz"
url_id
install.packages(url_id, repos=NULL, type="source")


