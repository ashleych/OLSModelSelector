test_that("Test the above with very small set of columns - as a proper unit test ", {
  library(data.table)
  # Define the columns
  cols_df <- c("col1", "col2", "col3", "col4", "col5")
  num_rows <- 5
  
  # Create an empty dataframe with specified columns
  train_df <- data.frame(matrix(ncol = length(cols_df), nrow =   num_rows))
  colnames(train_df) <- cols_df
  
  for (col in cols_df) {
    train_df[[col]] <- runif(num_rows, min = 0, max = 100)  # Replace runif with rnorm for normal random values
  }
  
  create_lag_columns <- function(data,lagN) {
    x <- shift(data, n=1,type = 'lag')
    return(x)
  }
  

  train_df[paste0(cols_df, '_lag_',1)] <- lapply(train_df[cols_df], create_lag_columns, 1)
  train_df[paste0(cols_df, '_lag_',2)] <- lapply(train_df[cols_df], create_lag_columns, 2)
  


  # Function to create lag columns

  

    # first identify the baseVariables
  # After looking at the macrovariables one can see that these are the base variables, and all other variables are combinarions of these
  
  
  sortAndJoin <- function(formula){
    
    
    rhs<-strsplit(formula,"~")[[1]][[2]]
    rhs_split<-strsplit(rhs,"[+]")
    rhs_split<- sort(trimws(rhs_split[[1]]))
    rhs_join<-paste(rhs_split,collapse="+")
    return(rhs_join)
  }
  
  allFormulae<-ST.auto.1::getUnRelatedVariableCombinations("DR", cols_df, train_df, 3 )
  allFormulaeSorted<-unlist(lapply(allFormulae, sortAndJoin))
  
  # check if any duplicates
  
  expect_equal(length(allFormulae), length(unique(allFormulae)))
  
  
  allFormulae_2<-ST.auto.1::getUnRelatedVariableCombinations("DR", cols_df, train_df, 2 )
  allFormulaeSorted_2<-unlist(lapply(allFormulae_2, sortAndJoin))
  
  # check if any duplicates
  
  expect_equal(length(allFormulae_2), length(unique(allFormulae_2)))
  

  allFormulae_4<-ST.auto.1::getUnRelatedVariableCombinations("DR", cols_df, train_df, 4 )
  allFormulaeSorted_4<-unlist(lapply(allFormulae_4, sortAndJoin))
  browser()
  # check if any duplicates
  
  expect_equal(length(allFormulaeSorted_4), length(unique(allFormulaeSorted_4)))
  
})