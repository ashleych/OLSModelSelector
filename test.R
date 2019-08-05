# mdata<-readxl::read_excel("data/MacroDataAC.xlsx")
# validationSampler(mdata,1:29,30:33,1:48)



vars<-c("ECI_yoy_ch_3QMA_lag_4", "avg_oil_pri_barrel_3QMA", "Rl_est_Dub_q_yoy_ch_lag_1", "avg_oil_pri_barrel_3QMA_lag_2", "Non_oil_ECI_yoy_ch_3QMA_lag_3", "Non_oil_ECI_yoy_ch_6QMA", "avg_oil_pri_barrel", "avg_oil_pri_barrel_3QMA_lag_1", "avg_oil_pri_barrel_6QMA_lag_1", "avg_oil_pri_barrel_lag_2", "avg_oil_pri_barrel_lag_3")
allModels<-modelDeveloper("DR",vars,2)
allModelsDiagnostics<-modelDiagnostics(allModels)
modelEvaluator(allModelsDiagnostics)
