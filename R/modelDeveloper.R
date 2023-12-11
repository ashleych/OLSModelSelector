
#' Generates all models using combination of dependent and independent variables provided
#'
#' @param LHS_vars LHS of the regression function of the form Y ~ X1 + X2. This is the vector of response variable. Example default rates, or PIT PDs for default rate forecasting
#' @param RHS_vars Right hand side of the regression equationof the form Y ~ X1 + X2. A vector of dependent variables,  macroeconomic variable names for instance. These should be available in the data provided to  validationSampler.
#' @param no_of_vars Maximum Number of dependent variables to be used in the model building.
#' @param trainData Training dataframe on which model is to be built
#' @param modelsNamesList List of Models to be built, if this is not NA, then the previous arguments are ignored
#' @param multiple Boolean to denote if a single model to be built using combinations of dependent variables or all combinations of dependent variables to be used. Default is \code{multiple = TRUE}
#' @param silent Default to FALSE. It is used to not run cat statements when call_rmd is called
#' @return All model objects.Note that this is not a dataframe but S3 objects. If you want to explore particular models, then one can use the subset operator
#' @export

modelDeveloper <- function(LHS_vars, RHS_vars,multiple=TRUE,no_of_vars,modelsNamesList=c(), trainData=train_df){
  
  # if(silent==FALSE){
  #   cat("Following checks are being done : \n")
  #   cat(
  #     "1. If the following data frames exist : macrodata,macrometa,test_df,train_df,forecast_df \n"
  #   )
  #   cat("2. Variable listed in Macrometa data is not available in macrodata \n")
  #   cat("3. Macrometa should contain columns Variable and Type \n")
  #   cat(
  #     "4. Type in macrometa should contain directions as -1 or 1. \n In case variables are not to be tested for sign, do not include it in the macrometa file \n"
  #   )
  #   cat("5. Macrodata should contain Date as a character field and in dmy format eg. 31/12/2018 \n")
  # }
  
  # check if the data is available before running
  df_names <- c("macrodata",'macrometa',"test_df","train_df","forecast_df")
  if (!all(sapply(df_names, function(x) {
    (exists(x) &&
     is.data.frame(get(x)))
  }))) {
    stop(
      'Please check if ALL of the following data frames exist : \n macrodata \n macrometa \n test_df \n train_df \n forecast_df. \n \n If not use ValidationSampler function'
    )
  }
  
  stopifnot(
    identical(sort(colnames(macrodata)), sort(colnames(train_df)))   ,
    identical(sort(colnames(macrodata)), sort(colnames(test_df))),
    identical(sort(colnames(macrodata)), sort(colnames(forecast_df))),
    identical(sort(colnames(macrodata)), sort(colnames(test_df)))
  )
  if (!all(macrometa$Variable %in% colnames(macrodata))) {
    print("Following variables missing:")
    print(macrometa$Variable[! macrometa$Variable %in% colnames(macrodata)])
    stop("Variable listed in Macrometa data is not available in macrodata")
  }
  
  if (!all(colnames(macrometa) %in% c("Variable", "Type"))) {
    stop("Macrometa should contain columns Variable and Type")
  }
  
  if (!all(unique(macrometa$Type) %in% c(1, -1))) {
    stop(
      "Type in macrometa should contain directions as -1 or 1. In case you dont want variables to be tested for sign, do not include it in the macrometa file"
    )
  }
  
  if (!is.character(macrodata$Date)) {
    stop("Macrodata should contain Date as a character field and in dmy format eg. 31/12/2018")
  }
  tryCatch(
    lubridate::dmy(macrodata$Date),
    error = function(e) {
      cat("Date in macrodata should be in dmy format eg. 31/12/2018")
    }
  )
  if(length(modelsNamesList)>0){
    
    models <-lapply(modelsNamesList,
                               function(x)
                                 lm(as.formula(x), data = train_df))
    
    names(models)<- modelsNamesList
    return(models)
  }
  
  
  LHS_all <- paste0(LHS_vars," ~ ")
  
  if (multiple== TRUE){len= 1} else {len=no_of_vars} # if multiple models are not needed, and we want to see a model with just the LHS and RHS
  RHS_all<-  unlist(sapply(len:no_of_vars, function(x) {
    unlist(combn(RHS_vars,x,simplify = FALSE, FUN=paste0,collapse="+"))
  }))
  
  allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))
  
  allModelObjects <- lapply(allFormulae,
                            function(x) lm(as.formula(x), data = trainData)
  )
  names(allModelObjects)<- allFormulae
  
  return(allModelObjects)
}
