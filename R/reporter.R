#' Knit the rmarkdown into an HTML document
#'
#' @param modelList model list in LHS~RHS form
#' @param output_file name of the output file
#' @param output_dir Name of the output directory
#' @param report_title Name of the report
#' @export
#' @examples
#' models<-c("DR~avg_oil_pri_barrel_lag_3+avg_oil_pri_barrel_lag_2","DR~avg_oil_pri_barrel_lag_2","DR~Non_oil_ECI_yoy_ch_6QMA+avg_oil_pri_barrel_6QMA_lag_1")
#' validationSampler(macrodata,1:29,30:33,1:48)
#' call_rmd(models,report_title = "Retail")

call_rmd <- function(model_list, output_file = "report.html", output_dir = getwd(), report_title = "Corporate", ...) {


  ## Get directory of report markdown template
  report_dir <- system.file("rmdTemplate/selectedModelReport.rmd", package = "ST.auto.1")


  render(
    report_dir,
    output_file = output_file,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    params = list(model_list = model_list,
                  set_title = report_title)
  )


  ## Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)

}

