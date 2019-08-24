library(readxl)
library(devtools)
#install_github("ashleych/OLSModelSelector")
library(ST.auto.1)
macrometa <- readxl::read_excel("macrometa1.xlsx")
macrodata <- readxl::read_excel("Macro_data_2.xlsx")



validationSampler(macrodata,22:52,53:56,22:56)

def_cols <- c("Date","DR", "DR_logit_FD", "DR_logit_FD_lag1", "DR_Log", "DR_logit")

vars<-colnames(macrodata)[!colnames(macrodata) %in% def_cols]

allModels <-
  modelDeveloper(
    LHS_vars = "DR_logit_FD",
    RHS_vars = vars,
    trainData = train_df,
    no_of_vars = 2
  )
beepr::beep()
#allModels[[1]]
options(error= recover)
allModelsDiagnostics <- modelDiagnostics(allModels)

str(allModelsDiagnostics)

allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)
