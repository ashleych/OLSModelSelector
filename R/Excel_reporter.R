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

call_excel <- function(model_list, output_file = NULL, output_dir = getwd(), report_title = "Corporate",...){
#Check if additional scenarios are to be used

  sensitivity=c()
  scenarios<- list()
  scenario_names<-list()
  scenario_colors<-list()
input_args_list <- list(...)
if ("scenarios" %in% names(input_args_list)) {

  scenarios <- input_args_list$scenarios
  if ("scenario_names" %in% names(input_args_list)){
    scenario_names<- input_args_list$scenario_names
  } else{
    scenario_names<- paste0("scenario_",seq(length(scenarios)))
  }

}
# if ("scenario_colors" %in% names(input_args_list)) {
#   scenario_colors <- input_args_list$scenario_colors
# 
# }
# if ('sensitivity' %in% names(input_args_list)) {
#   sensitivity<- input_args_list$sensitivity
# }
# Code to output and format Excel sheets ----------------------------------
  if (is.null(output_file)) {
    file.name <- make.names(paste0(report_title, Sys.time(), '.xlsx'))
  }
  else
  {
    file.name <- make.names(paste0(output_file, Sys.time(),'.xlsx'))
  }

  names(model_list) <- make.unique(strtrim(model_list, 28))

  summary <-
    data.table(modelName = model_list, sheetName = names(model_list))

  dfSummaryWriter(file.name,dataFrame=summary)


  # lapply(seq_along(model_list), function(x) {
  #   sheetCreator(
  #     model = model_list[[x]],
  #     file.name = file.name,
  #     sheetName = names(model_list[x]),
  #     scenarios=scenarios,
  #     scenario_names=scenario_names,
  #     scenario_colors=scenario_colors,
  #     sensitivity=sensitivity,...
  #   )
  # })
  
  lapply(seq_along(model_list), function(x) {
    sheetCreator(
      model = model_list[[x]],
      file.name = file.name,
      sheetName = names(model_list[x]),...
    )
  })

    dfWriter(file.name,"macrodata",macrodata)
    dfWriter(file.name,"macrometa",macrometa)
    dfWriter(file.name,"trainingData",train_df)
    dfWriter(file.name,"TestingData",test_df)
    dfWriter(file.name,"Forecast",forecast_df)

    if (length(scenarios)>0){
      for(i in 1:length(scenarios)){
        dfWriter(file.name,scenario_names[[i]],scenarios[[i]])
      }
    }
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

sheetCreator <- function(model,file.name, sheetName,...) {
  input_arg_list <- list(...)
  # if ("scenarios" %in% names(input_arg_list)) {
  #   scenarios <- input_arg_list$scenarios
  #   scenario_names<-input_arg_list$scenario_names
  # } else {
  #   scenarios <- list()
  #   scenario_names<-list()
  # }
  # 
  # if ("scenario_colors" %in% names(input_arg_list) ){
  #   scenario_colors <- input_arg_list$scenario_colors
  # } else {
  #   scenario_colors <- list()
  # }
  # if ("sensitivity" %in% names(input_arg_list) ){
  #   sensitivity <- input_arg_list$sensitivity
  # } else {
  #   sensitivity <- list()
  # }
  # this contains all the results to be shown in the Excel file
  # excelDetails <- reporter(model,report_type = "excel",scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity)
  excelDetails <- reporter(model,report_type = "excel",...)
  
  #check if files already exists in the current directory in which case, it loads it else it creates a new file
  if(!file.exists(file.name))
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

  fullSheetStyle <-
    createStyle(
      fontName = "Arial",
      fontSize = 9,
      fontColour = "black",
      numFmt = "GENERAL",
      border = NULL,
      borderColour = getOption("openxlsx.borderColour", "black"),
      borderStyle = getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )

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

 ## Scenario Predictions Rates ----------------------
 ## Scenario section run only if scenario_list length >0
  if (length(excelDetails$scenario_list)>0){
  startRowScenarioPredictions <- startRowPredicted +nrow(predicted_df) + 10
  startColumn_ScenarioPredictions <- startColumn
  
  for (scenario in excelDetails$scenario_list) {
    openxlsx::writeData(wb = wb,sheet = sheetName,x=scenario@scenario_name,startRow = startRowScenarioPredictions-1,startCol =startColumn_ScenarioPredictions )
      openxlsx::writeDataTable(
        wb,
        sheetName,
        scenario@predictions ,
        startCol = startColumn_ScenarioPredictions ,
        tableStyle = "TableStyleLight1",
        withFilter = FALSE,
        startRow = startRowScenarioPredictions,
        headerStyle = headerStyle,
        colNames = TRUE,
        bandedRows = TRUE
      )
    startColumn_ScenarioPredictions<-startColumn_ScenarioPredictions+dim(scenario@predictions)[2]+4
  }
  
  endRowScenarioPredictions <- startRowScenarioPredictions + dim(scenario@predictions)[1]
  
  ## Scenario predictions Plots -----------------------------------------------
  print(excelDetails$selectedModelScenariosCharts)
  insertPlot(wb, sheetName, width = 20, height = 4, xy = NULL, startRow = endRowScenarioPredictions + 4,
             startCol = 10, fileType = "png", units = "in", dpi = 300)
  
  

  ## Scenario MEV Plots -----------------------------------------------
  # plot_grid<-do.call("grid.arrange", c(plotList, ncol=2))
  # print(excelDetails$scenarioMEVCharts)
  plot_grid <- do.call("grid.arrange", c(excelDetails$scenarioMEVCharts, ncol=2))
  
  insertPlot(wb, sheetName, width = 20, height = 10, xy = NULL, startRow = endRowScenarioPredictions + 30,
             startCol = 10, fileType = "png", units = "in", dpi = 300)
  
  }
  ## END OF IF check of scenario_list length >0 ----------------------
  
  ## END OF Scenario Predictions Rates ----------------------
  
  ##Start of sensitivity tables
  if (exists("endRowScenarioPredictions")){
    
    startRowSensitivity <- endRowScenarioPredictions + 100 # since some 50-60 rows are taken up by the charts below scenario tables
    
  } else {
    startRowSensitivity <-  startRowPredicted +nrow(predicted_df) + 10
  }
  startColumn_Sensitivity <- startColumn
  for (sensitivity_df in excelDetails$ModelSensitivities_df_list) {
    # sensitivity for which column 
    mev_name<- comment(sensitivity_df)
    openxlsx::writeData(wb = wb,sheet = sheetName,x=mev_name,startRow = startRowSensitivity-1,startCol =startColumn_Sensitivity )
    
    
    openxlsx::writeDataTable(
      wb,
      sheetName,
      sensitivity_df,
      startCol = startColumn_Sensitivity ,
      tableStyle = "TableStyleLight1",
      withFilter = FALSE,
      startRow = startRowSensitivity,
      headerStyle = headerStyle,
      colNames = TRUE,
      bandedRows = TRUE
    )
    
    # startRowSensitivity<-startRowSensitivity +dim(sensitivity_df)[1]+ 4
    startColumn_Sensitivity <- startColumn_Sensitivity +dim(sensitivity_df)[2]+ 4
  }
  

  
  
  ## End of sensitivity tables

# Predicted plot ----------------------------------------------------------
print(excelDetails$report_pred_plot)
  insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 4,
             startCol = 15, fileType = "png", units = "in", dpi = 300)


# Residual Plot ---------------------------------------------------------
print(excelDetails$residualsVsFittedPlot())
insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 24,
           startCol = 15, fileType = "png", units = "in", dpi = 300)

#   openxlsx::saveWorkbook(wb,file.name,overwrite = TRUE)
# }

# QQ Plot ---------------------------------------------------------
print(excelDetails$normalQQPlot())
insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 44,
           startCol = 15, fileType = "png", units = "in", dpi = 300)

# scale_locationPlot Plot ---------------------------------------------------------
print(excelDetails$scale_locationPlot())
insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 44,
           startCol = 21, fileType = "png", units = "in", dpi = 300)

# ACF Plot ---------------------------------------------------------
print(excelDetails$acfPlot())
insertPlot(wb, sheetName, width = 6, height = 4, xy = NULL, startRow = 24,
           startCol = 21, fileType = "png", units = "in", dpi = 300)

openxlsx::saveWorkbook(wb,file.name,overwrite = TRUE)
}


#'Creates and formats sheets in Excel using openxslx. Called internally by call_excel function
#'
#' Called in a loop depending on number of models
#' @param file.name Name of the excel file
#' @param sheetName name of the sheet
#' @param dataFrame Name of the dataframe to be written to the sheet
#' @export
#' @examples

dfWriter <- function(file.name,sheetName,dataFrame) {
  if(!file.exists(file.name))
    wb = createWorkbook()
  else
    wb <- loadWorkbook(file = file.name)
  #wb <- createWorkbook()

  fullSheetStyle <-
    createStyle(
      fontName = "Arial",
      fontSize = 9,
      fontColour = "black",
      numFmt = "GENERAL",
      border = NULL,
      borderColour = getOption("openxlsx.borderColour", "black"),
      borderStyle = getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
  headerStyle <-
    createStyle(
      fontSize = 10,
      fontColour = 'black' ,
      halign = "center",
      fgFill = "darkorange",
      border = "TopBottom",
      borderColour = "#4F81BD"
    )

  addWorksheet(wb,sheetName)
  showGridLines(wb, sheetName, showGridLines = FALSE)
  addStyle(wb, sheetName, fullSheetStyle, rows = 1:400, cols=1:400,gridExpand = TRUE, stack = TRUE)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    dataFrame,
    startCol = 1,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = 1,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )

  openxlsx::saveWorkbook(wb,file.name,overwrite = TRUE)
}

#'Creates summary sheet
#'
#' Called in a loop depending on number of models
#' @param file.name Name of the excel file
#' @param sheetName name of the sheet
#' @param dataFrame Name of the dataframe to be written to the sheet
#' @export
#' @examples

dfSummaryWriter <- function(file.name,dataFrame) {
  if(!file.exists(file.name))
    wb = createWorkbook()
  else
    wb <- loadWorkbook(file = file.name)
  #wb <- createWorkbook()

  fullSheetStyle <-
    createStyle(
      fontName = "Arial",
      fontSize = 9,
      fontColour = "black",
      numFmt = "GENERAL",
      border = NULL,
      borderColour = getOption("openxlsx.borderColour", "black"),
      borderStyle = getOption("openxlsx.borderStyle", "thin"),
      bgFill = NULL,
      fgFill = NULL,
      halign = NULL,
      valign = NULL,
      textDecoration = NULL,
      wrapText = FALSE,
      textRotation = NULL,
      indent = NULL
    )
  headerStyle <-
    createStyle(
      fontSize = 10,
      fontColour = 'black' ,
      halign = "center",
      fgFill = "darkorange",
      border = "TopBottom",
      borderColour = "#4F81BD"
    )

  #addWorksheet(wb,sheetName)

  sheetName <- "Navigation"
  addWorksheet(wb, sheetName)
  showGridLines(wb, sheetName, showGridLines = FALSE)
  addStyle(wb, sheetName, fullSheetStyle, rows = 1:400, cols=1:400,gridExpand = TRUE, stack = TRUE)
  startRow <- 10
  startCol <- 10
  # writeDataTable(wb, sheet = "Navigation", x = summary$modelName, startRow = startRow,startCol = startCol, colNames = True)

  openxlsx::writeDataTable(
    wb,
    sheetName,
    data.table(Models=dataFrame$modelName),
    startCol = startCol,
    tableStyle = "TableStyleLight1",
    withFilter = FALSE,
    startRow = startRow,
    headerStyle = headerStyle,
    colNames = TRUE,
    bandedRows = TRUE
  )
  setColWidths(wb, sheetName, cols=startCol, widths = '60')

  for (i in 1:nrow(dataFrame)) {
    writeFormula(
      wb,
      "Navigation",
      startRow = startRow + i ,
      startCol = startCol + 1,
      x = makeHyperlinkString(
        sheet = dataFrame$sheetName[[i]],
        row = 1,
        col = 2,
        text = "Click here"
      )
    )
  }
  openxlsx::saveWorkbook(wb,file.name,overwrite = TRUE)
}





