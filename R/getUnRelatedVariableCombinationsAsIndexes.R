#' Get Unrelated variables combinations, to avoid using variable combinations in the RHS of the formula that are transformations of the same variable, result is obtained as indices rather than names/
#'
#' @param baseVariables Names of the untransformed variables that you want to use in the RHS, so if there is oil_price and oil_price_ln, provide just oil_price, it will identify that oil_price_ln is a transformation using "like" operator
#' @param train_df Train df - this is used to get the list of all macrovariables that have a likeness to each variable in baseVariables
#' @param numberOfVariables Number of variables to be used in each formula. See strictMax for 'upto' vs 'exact' match
#' @param strictMax Default as TRUE, if false, it looks for upto numberoFvariables, if TRUE, it looks at exact number of variables in the combinations it generates
#' @param patternType default as "startsWith". If any other, then it uses %like% 
#' @return Returns a list of all formulae that do not have related variables in the same formula
#' 
#' @export
#' @examples
#' getUnRelatedVariableCombinationsAsIndexes(c("oil_price",'gdp_ratio'),train_df)
getUnRelatedVariableCombinationsAsIndexes <- function(baseVariables,train_df,numberOfVariables,strictMax=TRUE, patternType="startsWith",ncores=4) {


library(pbmcapply)
indexStart <- numberOfVariables
# allCombosOfBaseVars <-
#   pbmclapply(indexStart:numberOfVariables, function(x) {
#     combn(baseVariables, x, simplify = FALSE)
#   }, mc.cores = getOption("mc.cores", ncores), mc.preschedule = TRUE)
allCombosOfBaseVars <-
  pbmclapply(indexStart:numberOfVariables, function(x) {
    combn(baseVariables, x, simplify = FALSE)
  }, mc.cores = ncores, mc.preschedule = TRUE)
#allCombosOfBaseVars <- unlist(allCombosOfBaseVars, recursive = FALSE)
allCombosOfBaseVars <- allCombosOfBaseVars[[1]]

print(paste0(" Number of baseVar combinations :",length(allCombosOfBaseVars)))

# Get all related variables against named lists
relatedVarsList <- list()
for (b in baseVars) {
  relatedVarsList[[b]] <-
    grep(paste0("^", b), names(train_df), value = FALSE)
}

#For each base var combination get the related vars combinations possible, using expandgrid which gives a df
# combinations_df_list <-
#   list()# contains the dataframe with all combinations
# for (i in 1:length(allCombosOfBaseVars)) {
#   relatedVarsListForThatCombo <-
#     relatedVarsList[allCombosOfBaseVars[[i]]]
#   
#   combination <- expand.grid(relatedVarsListForThatCombo)
#   combinations_df_list <- c(combinations_df_list, list(combination))
# }
print("Generating the related var combination for each base var combination")

combinations_df_list <-
  pbmclapply(1:length(allCombosOfBaseVars), function(i) {
 
    relatedVarsListForThatCombo <-
      relatedVarsList[allCombosOfBaseVars[[i]]]

    combination <- unname(as.matrix(expand.grid(relatedVarsListForThatCombo)))
    return(combination)
    
  }, mc.cores = ncores, mc.preschedule = TRUE)

combinations_df_list_flat<- rbind(combinations_df_list)
# 
combinations_df_list_flat_1<-do.call(rbind,combinations_df_list_flat)
# remove names, convertinto matrix, to reduce memory footprint. this can be used to send to fastLmpure. Please note that the result doesnt contain the response variable
# combinations_df_list_flat <- unname(as.matrix( rbindlist(combinations_df_list, use.names = FALSE)))

return(combinations_df_list_flat_1)
}