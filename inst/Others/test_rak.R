library(readxl)
library(devtools)
#install_github("ashleych/OLSModelSelector")
library(ST.auto.1)
wd <- getwd()
setwd("D:\\Dropbox\\Projects\\RAK\\RAK")
macrometa <- readxl::read_excel("macrometa1.xlsx")
macrodata <- read.csv("Macro_data_2.csv",stringsAsFactors =FALSE)
#test <- read.csv("Macro_data_2.csv",stringsAsFactors =FALSE)

#str(macro)
setwd(wd)
#(macrodata)
#glimpse(macrodata)




validationSampler(macrodata,22:52,53:56,22:80)

def_cols <- c("Date","DR", "DR_logit_FD", "DR_logit_FD_lag1", "DR_Log", "DR_logit")


vars<-colnames(macrodata)[!colnames(macrodata) %in% def_cols]



## select variables based on univariate regression, mention threshold that you want to use for selecting variables, currently 0.1
selectedVars <-
  variableSelector(
    LHS_vars = c("DR_logit_FD"),
    RHS_vars = vars,
    trainData = train_df
  )



allModels <-
  modelDeveloper(
    LHS_vars = c("DR_logit_FD"),
    RHS_vars = selectedVars,
    trainData = train_df,
    no_of_vars = 3
  )

selectedModelObjs <- modelFilter(allModels,adj.r.squaredThreshold =0.45)


#allModels[[1]]
system.time({
allModelsDiagnostics <- modelDiagnostics(selectedModelObjs)
})

allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)



models <-
  c(
    "DR_logit_FD ~ rl_est_ad+uae_ann_imp_yoy+uae_rl_cons_3qma",
    "DR_logit_FD ~ uae_ann_imp_yoy+rl_est_ad_log+uae_rl_cons_3qma",
    "DR_logit_FD ~ uae_ann_imp_yoy+rl_est_ad_3qma+uae_rl_cons_3qma"
  )


models <-
  c(
    "DR_logit_FD ~ avg_oil_pri_barr",
    "DR_logit_FD ~ rl_est_ad_yoy+uae_rl_cons",
    "DR_logit_FD ~ eibor+eibor_yoy",
    "DR_logit_FD ~ DR_logit_FD_lag1 + uae_ann_imp_yoy + rl_est_ad_log + uae_rl_cons_3qma",
    "DR_logit_FD ~ uae_rl_cons+rl_est_ad_yoy.1"

  )
call_rmd(models,report_title = "Portfolio RAKFIN")


#call_rmd("DR_logit_FD ~ avg_oil_pri_barr",report_title = "Portfolio RAKFIN")

## this will generate excel sheet
call_excel(models)
reporter(models[[1]],report_type = 'excel') -> f

f$report_predicted_df

#this has lagged variables
model1 <- c("DR_logit_FD ~ DR_logit_FD_lag1 + uae_ann_imp_yoy + rl_est_ad_log + uae_rl_cons_3qma")

model <-
  "DR_logit_FD ~ DR_logit_FD_lag1 +uae_ann_imp_yoy+rl_est_ad_log+uae_rl_cons_3qma"
call_rmd(model)

configDynamic("DR_logit_FD","DR_logit_FD_lag1",1,30)
rm(configDynamic_Df)


# untranform Configuration ------------------------------------------------



transformConfig<- data.table(stringsAsFactors=FALSE,
       varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit"),
   baseVarName = c("DR", "DR", "DR"),
         order = c(1L, 2L, 1L),
          type = c("logit", "difference", "logit"),
           lag = c(0L, 1L, 0L),
   differences = c(0L, 1L, 0L)
)


"DR_logit1" %in% colnames(macrodata)
