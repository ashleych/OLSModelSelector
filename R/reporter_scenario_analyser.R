#' Generates report details for each model defined by LHS and RHS
#' @param reporter_output output from reporter function ie. excel_details containing various diagnostic tests, scenario comparison
#' @return results_of_overall_scenario dataframe containing results of diagnostic tests in a summary form
#' @export
#' @examples
#'reporter_scenario_analyser(reporter_output)

reporter_scenario_analyser <-
  function(reporter_output, ...) {
    results_container <- c()
    for (ro in reporter_output) {
      colnames <-
        c(
          'Significance',
          'Multicollinearity',
          'R2',
          'MAPE',
          c(ro$report_selectedModelDiagnostics$results),'scenario_order'
        )
      
      Significance <-
        if (nrow(ro$report_summary$estimates_excel[Significance == "FAIL",]) >
            0) {
          "FAIL"
        } else {
          "PASS"
        }
      Multicollinearity <-
        if (nrow(ro$report_summary$estimates_excel[Multicollinearity == "FAIL",]) >
            0) {
          "FAIL"
        } else {
          "PASS"
        }
      scenario_order<-paste0(ro$asc_order_of_scenarios,collapse = ",")
      v <- c(
        Significance,
        Multicollinearity,
        ro$report_summary$otherStats_excel$R2,
        ro$report_summary$otherStats_excel$MAPE,
        ro$report_selectedModelDiagnostics$FINAL,
        scenario_order
        
      )
      
      results_container = append(results_container, list(v))
    }
    
    
    
    untransposed_results <- rbindlist(list(data.table(), results_container))
    results_of_overall_scenario <- data.table(t(untransposed_results)) #transposed results
    colnames(results_of_overall_scenario) <- colnames
    return (results_of_overall_scenario)
  }