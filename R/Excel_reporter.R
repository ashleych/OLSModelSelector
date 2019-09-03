#' Provides detailed diagnostic tests including tests for stationarity, autocorrelation for each model
#'
#' @param modelList model list in LHS~RHS form
#' @param output_file name of the output file without extension
#' @param output_dir Name of the output directory, default is current directory
#' @param report_title Name of the report
#' @return Results of various diagnostic tets of Stationarity, BP, DW, Tests of significance etc provided.
#' @export
#' @examples
#' models <-c("DR_logit_FD ~ rl_est_ad+uae_ann_imp_yoy+uae_rl_cons_3qma","DR_logit_FD ~ uae_ann_imp_yoy+rl_est_ad_log+uae_rl_cons_3qma","DR_logit_FD ~ uae_ann_imp_yoy+rl_est_ad_3qma+uae_rl_cons_3qma")
#' call_excel(models,output_file = "Report2019") # allModelObjects created by modelDeveloper()

call_excel <- function(model_list, output_file = NULL, output_dir = getwd(), report_title = "Corporate"){
# Code to output and format Excel sheets ----------------------------------
  if (is.null(output_file)) {
    file.name <- make.names(paste0(report_title, Sys.time(), '.xlsx'))
  }
  else
  {
    file.name <- make.names(paste0(output_file, '.xlsx'))
  }

  names(model_list) <- make.unique(strtrim(model_list, 28))

    lapply(seq_along(model_list), function(x) {

      sheetCreator(model=model_list[[x]], file.name=file.name,sheetName= names(model_list[x]))
    })

  # output_dir <- getwd()
  output_file <- file.name
  ## Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)

}


#'Creates and formats sheets in Excel using openxslx. Called internally by call_excel function
#'
#' Called in a loop depending on number of models
#' @param file.name Name of the excel file
#' @param model name of the model
#' @export
#' @examples

sheetCreator <- function(model,file.name, sheetName) {
  # this contains all the results to be shown in the Excel file
  excelDetails <- reporter(model,report_type = "excel")

  #check if files already exists in the current directory in which case, it loads it else it creates a new file
  if(! file.exists(file.name))
    wb = createWorkbook()
  else
    wb <- loadWorkbook(file = file.name)
  #wb <- createWorkbook()

  #sheetName <- paste0("Model ",sheetNo)

  addWorksheet(wb,sheetName)
  showGridLines(wb, sheetName, showGridLines = FALSE)

  ## Define some styles for headers, and the table body
  rColors <- c("darkorange",
               "gray47",
               "gray26",
               "white",
               "firebrick3",
               "darkgreen",
               "forestgreen")

  negStyle <- createStyle(fontColour = "firebrick3",fontName = "Arial", fontSize = 9)
  posStyle <- createStyle(fontColour = "darkgreen",fontName = "Arial", fontSize = 9)

  fullSheetStyle <- createStyle(fontName = "Arial", fontSize = 9, fontColour = "black",
                                numFmt = "GENERAL", border = NULL,
                                borderColour = getOption("openxlsx.borderColour", "black"),
                                borderStyle = getOption("openxlsx.borderStyle", "thin"), bgFill = NULL,
                                fgFill = NULL, halign = NULL, valign = NULL, textDecoration = NULL,
                                wrapText = FALSE, textRotation = NULL, indent = NULL)

  headerStyle <- createStyle(fontSize = 10, fontColour ='black' , halign = "center",
                             fgFill = "darkorange", border="TopBottom", borderColour = "#4F81BD")



  # Apply style to entire sheet
  addStyle(wb, sheetName, fullSheetStyle, rows = 1:400, cols=1:20,gridExpand = TRUE, stack = TRUE)

  paddingBtnTables <- 2 # two rows between each table
  startRowEstimatesTable <- 4
  startColumn <- 4
  estimatesTable <- excelDetails$report_summary$estimates_excel
  countRowestimatesTable <- nrow(estimatesTable)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    estimatesTable,
    startCol = startColumn,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = startRowEstimatesTable,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )

  setColWidths(wb, sheetName, cols=startColumn, widths = '22')

  conditionalFormatting(
    wb,
    sheetName,
    cols = startColumn + 6:8,
    rows = (startRowEstimatesTable + 1):(startRowEstimatesTable + countRowestimatesTable),
    type = 'contains',
    rule = "FAIL",
    style = negStyle
  )
  conditionalFormatting(
    wb,
    sheetName,
    cols = startColumn + 6:8,
    rows = (startRowEstimatesTable + 1):(startRowEstimatesTable + countRowestimatesTable),
    type = 'contains',
    rule = "PASS",
    style = posStyle
  )
  conditionalFormatting(
    wb,
    sheetName,
    cols = startColumn + 3,
    rows = (startRowEstimatesTable + 1):(startRowEstimatesTable + countRowestimatesTable),
    rule = "< 0.1",
    style = negStyle
  )



  # OtherEstimates ----------------------------------------------------------

  startRowOtherStatsTable <- startRowEstimatesTable + countRowestimatesTable + paddingBtnTables
  OtherStatsTable <- excelDetails$report_summary$otherStats_excel
  countRow_OtherStats <- nrow(OtherStatsTable)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    OtherStatsTable,
    startCol = startColumn +1 ,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = startRowOtherStatsTable,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )


  # Diagnostic Tests --------------------------------------------------------

  otherDiagnostics <- excelDetails$report_selectedModelDiagnostics

  startRowOtherDiagnostics <- startRowOtherStatsTable + countRow_OtherStats + paddingBtnTables
  countRowOtherDiagnostics <- nrow(otherDiagnostics)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    otherDiagnostics,
    startCol = startColumn ,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = startRowOtherDiagnostics,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )


  # Predicted Default Rates -------------------------------------------------

  predicted_df <- excelDetails$report_predicted_df

  startRowPredicted <- startRowOtherDiagnostics + countRowOtherDiagnostics + paddingBtnTables
  countRowOtherDiagnostics <- nrow(predicted_df)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    predicted_df ,
    startCol = startColumn ,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = startRowPredicted,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )


# Predicted plot ----------------------------------------------------------
print(excelDetails$report_pred_plot)
  insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 4,
             startCol = 15, fileType = "png", units = "in", dpi = 300)

  openxlsx::saveWorkbook(wb,file.name,overwrite = TRUE)
}







