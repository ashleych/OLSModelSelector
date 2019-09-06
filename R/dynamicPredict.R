#' Dynamic forecasting function
#' @param selectedModel Model defined by means of LHS and RHS(to be passed as a list)
#' @param selectedModelObject Default value of FALSE, single model to be examined.
#' @param laggedRelation Identify the response variable, the laggedvariable name and number of lags to be used. this has to be colon ':' separated and quoted-  for example "DR_logit_FD :DR_logit_FD_lag1:1" shows response variable is DR_logit_FD, the lagged independent variable is DR_logit_FD_lag1 and 1 indicates the number of lags to consider
#' @param trainIndex Index of rows for which all independent variables are present including the lagged one, typically traindata or training plus test data. Default is 1:nrow(train_df)
#' @return dynamically forecast response variables
#' @export
#' @examples
#' model <-"DR_logit_FD ~ DR_logit_FD_lag1 +uae_ann_imp_yoy+rl_est_ad_log+uae_rl_cons_3qma"
#' laggedRelation <- "DR_logit_FD :DR_logit_FD_lag1:1"
#' reporter(model,laggedRelation="DR_logit_FD :DR_logit_FD_lag1:1",trainIndex=1:30)
#'
dynamicForecast <- function(selectedModel,
                            selectedModelObject,
                            laggedRelation,
                            trainIndex = nrow(train_df)) {
  # this will work with one lagged variable
  forecast_df1<- copy(forecast_df)
   laggedRelationSplit <- trimws(unlist(strsplit(laggedRelation,"[:]")))
  # autoDepVar is short for autocorrelated independent variable ie the laggedVar.
  noOfLags<-laggedRelationSplit[[3]]
  autoVar <- laggedRelationSplit[[2]]
  depVar <- laggedRelationSplit[[1]]
  for (i in 1:trainIndex) {
    forecast_df1$predicted_values[i] <-
      predict(selectedModelObject, newdata = forecast_df1[i,])

  }

  for (j in trainIndex:nrow(forecast_df1)) {
    lookBackIndex <- j - as.numeric(noOfLags)
    forecast_df1[[autoVar]][j] <-
      forecast_df1$predicted_values[lookBackIndex]
    forecast_df1$predicted_values[j] <-
      predict(selectedModelObject, newdata = forecast_df1[j, ])
  }

  used_vars <- trimws(unlist(strsplit(selectedModel, "[+,~]")))
  keep_vars <- c("Date", used_vars, "predicted_values")
  forecast_df1[, ..keep_vars]
  }

