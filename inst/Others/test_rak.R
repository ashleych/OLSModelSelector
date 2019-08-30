library(readxl)
library(devtools)
#install_github("ashleych/OLSModelSelector")
library(ST.auto.1)
wd <- getwd()
setwd("D:\\Dropbox\\Projects\\RAK\\RAK")
macrometa <- readxl::read_excel("macrometa1.xlsx")
#macrodata <- readxl::read_excel("Macro_data_2.xlsx")
macrodata <- read.csv("Macro_data_2.csv",stringsAsFactors =FALSE)
#str(macro)
setwd(wd)
glimpse(macrodata)

validationSampler(macrodata,22:52,53:56,22:56)

def_cols <- c("Date","DR", "DR_logit_FD", "DR_logit_FD_lag1", "DR_Log", "DR_logit")
vars<-colnames(macrodata)[!colnames(macrodata) %in% def_cols]

#trial1_RHS <- c(vars,"DR_logit_FD_lag1")

allModels <-
  modelDeveloper(
    LHS_vars = c("DR_logit_FD"),
    RHS_vars = vars[1:50],
    trainData = train_df,
    no_of_vars = 2
  )
#allModels[[1]]
allModelsDiagnostics <- modelDiagnostics(allModels)
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
args(modelEvaluator)
table(allModelEvaluated$FinalResults)

setDT(temp)

# tstrsplit(as.data.table(temp),"[~]")
#
# temp<-allModelsDiagnostics[,tstrsplit(model,"[~]")]
# temp1<-temp[, V1 := trimws(V1)][V1=="DR_logit_FD",]
# #temp[,col1:= V1]
# setnames(temp,"V1","col1")
# length(vars)
#
# length(combn(seq_len(100),2))
#
# temp<-temp[,(tstrsplit(V2,"[+]",fill = NA,names=TRUE))]
#
# strsplit()
# combn(c("A","B","C"),2)
# length(combn(vars, 2, simplify = FALSE, FUN = paste0,
#       collapse = "+"))
# modelDeveloper
# #
# models <-c("DR_logit_FD ~ avg_oil_pri_barr+uae_rl_cons_yoy", "DR_logit_FD ~ avg_oil_pri_barr+rl_est_ad_yoy_fd", "DR_logit_FD ~ avg_oil_pri_barr+uae_rl_gov_cons_fd", "DR_logit_FD ~ uae_rl_gov_cons_fd+avg_oil_pri_barr_3qma", "DR_logit_FD ~ rl_est_ad_log+uae_rl_cons_3qma", "DR_logit_FD ~ DR_logit_FD_lag1", "DR_logit_FD ~ uae_rl_cons_qoq+uae_ann_imp_fd", "DR_logit_FD ~ uae_rl_cons_qoq+uae_ann_imp_qoq", "DR_logit_FD ~ uae_rl_cons_qoq+uae_ann_imp_log_fd", "DR_logit_FD ~ uae_rl_cons_qoq+uae_ann_imp_log_gr_qr", "DR_logit_FD ~ uae_ann_imp_qoq+uae_rl_cons_fd", "DR_logit_FD ~ uae_rl_cons_fd+uae_ann_imp_log_gr_qr", "DR_logit_FD ~ uae_rl_cons_fd+uae_ann_imp_fd")
#
# models <-"DR_logit_FD ~ avg_oil_pri_barr+uae_rl_cons_yoy"
# models <- "DR_logit_FD ~ avg_oil_pri_barr+uae_rl_gov_cons_yoy"

models <-
  c(
    "DR_logit_FD ~ avg_oil_pri_barr+uae_rl_cons_yoy",
    "DR_logit_FD ~ avg_oil_pri_barr+uae_rl_gov_cons_yoy",
    "DR_logit_FD ~ uae_rl_cons_qoq+uae_ann_imp_qoq"
  )


models <-
  c(
    "DR_logit_FD ~ avg_oil_pri_barr",
    "DR_logit_FD ~ rl_est_ad_yoy+uae_rl_cons",
    "DR_logit_FD ~ eibor+eibor_yoy","DR_logit_FD ~ uae_rl_cons+rl_est_ad_yoy.1")

call_rmd(models,report_title = "Portfolio RAKFIN")

cols <- trimws(unlist(strsplit(models,'[~+]')))
cols
cols %in% colnames(train_df)
