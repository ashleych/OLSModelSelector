modelDeveloperTurbo <- function(LHS_var, RHS_vars,multiple=TRUE,no_of_vars,modelsNamesList=c(), trainData=train_df,pvalueThreshold=0.05, progressStep=10000){
  # To NOTE THAT THIS CREATES MODELS WITHOUT INTERCEPT, TO FORCE THSI TO CREATE MODEL WITH AN INTERCEPT ONE NEEDS TO ADD A COLUMN of 1s to the data and add that to the matrix
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
  
  
  LHS_all <- paste0(LHS_var)
  
  if (multiple== TRUE){len= 1} else {len=no_of_vars} # if multiple models are not needed, and we want to see a model with just the LHS and RHS

  RHS_all <- combn(RHS_vars,no_of_vars,simplify = FALSE)
 
  print(paste0("No of models created : ",length(RHS_all) ))
  print(paste0("Start of loop", format(Sys.time(), "%X")))
  pvals<-lapply(1:length(RHS_all), function(i){
    rhs<- RHS_all[i][[1]]

    if ((i %% progressStep)==0) {
      print(paste0("No of models checked : " ,as.integer(i/length(RHS_all)*100),"%" ,"---- Generated at ", format(Sys.time(), "%X")))

    }
    cols<-unlist(c(rhs,LHS_var))
    
    data=trainData[,..cols]
    data<-na.omit(data) #this is being done to avoid tbe warning: solve(): system is singular; attempting approx solution error which can happen becaise of teh presence of NAs
    # If we cbind a column of 1's then it will force the function to generate models with intercept,currently the implementation is without intercept

    pvalue<- tryCatch(
      {
        # Just to highlight: if you want to use more than one
        # R expression in the "try" part then you'll have to
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression
        # in case the "try" part was completed successfully
        # 
        # 

        mdl <- RcppArmadillo::fastLmPure(as.matrix(data[,..rhs]), as.matrix(data[,..LHS_var]))
        pvalue <- 2*pt(abs(mdl$coefficients/mdl$stderr), mdl$df.residual, lower.tail=FALSE)
        # The return value of `readLines()` is the actual value
        # that will be returned in case there is no condition
        # (e.g. warning or error).
      },
      error = function(cond) {
        # message(paste("URL does not seem to exist:", rhs))
        # message("Here's the original error message:")
        # message(conditionMessage(cond))
        # Choose a return value in case of error
        NULL
      },
      warning = function(cond) {
        # message(paste("URL caused a warning:", rhs))
        # message("Here's the original warning message:")
        # message(conditionMessage(cond))
        # Choose a return value in case of warning
        NULL
      }
    )
    
    # if (inherits(pvalue, "try-error")) {
    #   # Handle error case
    #   # You can add your specific actions for when there's an error
    #   pvalue<-c(9999999)
    # } else if (is.null(pvalue)) {
    #   pvalue<-c(9999999)
    #   # Handle warning case
    #   # You can add your specific actions for when there's a warning
    # } else {
    #   # Your successful result handling code here
    # 

    if(!is.null(pvalue)){
      return(max(pvalue))
    }
    return(NULL)
   
    
  })
  print(paste0("End of loop", format(Sys.time(), "%X")))
  



print(paste0("sum of nulls in pvals is: ", sum(is.null(pvals))))

# Assuming index_of_true contains the indices where the condition is TRUE
index_of_true <- which(pvals < pvalueThreshold)

# Select elements from RHS_All using the index_of_true
selectedRHS <- RHS_all[index_of_true]
  # selectedRHS<-RHS_all[pvals<pvalueThreshold]

  lhs<-paste0(LHS_var," ~ ")
  selectedRhs <-lapply(selectedRHS,function(x) paste0(x,collapse = " + "))
  
  selectedModels<- paste0(lhs, selectedRhs)
  print(paste0("No of models created : ",length(RHS_all)))
  print(paste0("No of models found significant :",length(selectedModels)))
  return(selectedModels)
  

}
