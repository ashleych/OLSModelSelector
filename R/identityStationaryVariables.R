library(tseries)
conduct_tests <- function(Data1) {
  kpsslevel_result <- kpss.test(x = Data1, null = "Level")$p.value
  kpsstrend_result <-  kpss.test(x = Data1, null = "Trend")$p.value
  adf_result <- adf.test(Data1)$p.value
  return(list(KPSS_Test_Level = round(kpsslevel_result, 4),
              KPSS_Test_Trend = round(kpsstrend_result, 4),
              ADF_Test = round(adf_result, 4)))
}
newTests<- function(){
# Apply the function to each column in Data1
test_results <- lapply(train_df, function(column) {
  if (is.numeric(column)) {
    nonNAsSeries<- column[!is.na(column)]
    conduct_tests(nonNAsSeries)
  } else {
    NULL  # If the column is not numeric, return NULL
  }
})

# Remove NULL elements from the list
test_results <- test_results[sapply(test_results, function(x) !is.null(x))]
test_results <- do.call(rbind, test_results)



test_results
}