
# 
# macrodata[,DR_logit := logit(DR)]
# macrodata[,DR_logit_FD := DR_logit - shift(DR_logit, n = 1, type = 'lag')]
# validationSampler(macrodata,1:29,30:33,1:48)
# transformConfig  <-  data.table(stringsAsFactors=FALSE,
#                                 varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit"),
#                                 baseVarName = c("DR", "DR", "DR"),
#                                 order = c(1L, 2L, 1L),
#                                 type = c("logit", "difference", "logit"),
#                                 lag = c(0L, 1L, 0L),
#                                 differences = c(0L, 1L, 0L))

test_that("Testing sensitivity with transformations",{


  models <- c(" DR_logit_FD ~ avg_oil_pri_barrel_lag_3")

  scenario_colors=c()
  scenarios=c()
  scenario_names<- c()
  sensitivity=c(avg_oil_pri_barrel_lag_3=0.05,avg_oil_pri_barr=0.07)

  excelDetails <- reporter(models,scenarios=c(),scenario_names=c(),scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted')
  browser()
  expect_equal(excelDetails$modelName,models)
  expect_equal(excelDetails$report_summary$estimates_excel[,Estimates][1], 0.022112)
  expect_equal(excelDetails$report_summary$estimates_excel[,Estimates][2], -0.000171)
  
  expect_equal(excelDetails$report_summary$otherStats_excel$R2,0.000316)
  expect_equal(excelDetails$report_summary$otherStats_excel$adjR2,-0.038134)
  expect_equal(excelDetails$report_summary$otherStats_excel$MAPE,1.039405)
  
  
  expect_equal(excelDetails$report_predicted_df$Date,c( "31/03/2010","30/06/2010", "30/09/2010", "31/12/2010", "31/03/2011", "30/06/2011", "30/09/2011", "31/12/2011", "31/03/2012", "30/06/2012", "30/09/2012", "31/12/2012", "31/03/2013", "30/06/2013", "30/09/2013", "31/12/2013", "31/03/2014", "30/06/2014", "30/09/2014", "31/12/2014", "31/03/2015", "30/06/2015", "30/09/2015", "31/12/2015", "31/03/2016", "30/06/2016", "30/09/2016", "31/12/2016", "31/03/2017", "30/06/2017", "30/09/2017", "31/12/2017", "31/03/2018", "30/06/2018", "30/09/2018", "31/12/2018", "31/03/2019", "30/06/2019", "30/09/2019", "31/12/2019", "31/03/2020", "30/06/2020", "30/09/2020", "31/12/2020", "31/03/2021", "30/06/2021", "30/09/2021", "31/12/2021"))
  expect_equal(excelDetails$report_predicted_df$predicted_values,c(0.0120854340415657, 0.0104620316789221, 0.00936363000981021, 0.00908689428602153, 0.00870083086888422, 0.00898952430913292, 0.00734163709736091, 0.00418252229221091, 0.00206429823358146, 0.00275101280742746, 0.00342463082898025, 0.0018712665250128, 0.00359203885942033, 0.00338761886084949, 0.00330676190958878, 0.00289564424078071, 0.00458965406122645, 0.00327544408645911, 0.00345651807856775, 0.00363474498963463, 0.00337281406334771, 0.00471834187832719, 0.00906070119339284, 0.0129019752512632, 0.0115735298952025, 0.0135311789042261, 0.0146694396276353, 0.0163560470629961, 0.0143357623974636, 0.0142907786058324, 0.0137099752349178, 0.0129424037217688, 0.0136274100504063, 0.00898952430913292, 0.00734163709736091, 0.00418252229221091, 0.00206429823358146, 0.00275101280742746, 0.00342463082898025, 0.0018712665250128, 0.00359203885942033, 0.00338761886084949, 0.00330676190958878, 0.00289564424078071, 0.00458965406122645, 0.00327544408645911, 0.00345651807856775, 0.00327544408645911))
  expect_equal(excelDetails$report_predicted_df$predicted_values_transformed,c(0.0230550377710193, 0.0232918589061391, 0.0235058289243875, 0.0237153087550849, 0.0239175951736385, 0.0241283611143648, 0.0243018339259877, 0.0244012043420647, 0.0244503948598512, 0.0245160995100189, 0.0245981332631021, 0.0246430706460066, 0.0247295557169964, 0.0248113900027262, 0.0248915255495173, 0.0249619052208802, 0.0250738558171433, 0.0251540492406682, 0.0252389468962437, 0.0253285231606707, 0.0254119212240622, 0.0255290384523232, 0.0257554159514648, 0.026081141773244, 0.0263767380525082, 0.026726468309255, 0.0271107129701408, 0.0275454684916615, 0.0279320884367401, 0.0283227388049102, 0.0287024948321203, 0.0290655210176825, 0.0294525742631732, 0.0297106307358973, 0.0299230070998605, 0.030044654765772, 0.0301048708861222, 0.0301853004391994, 0.0302857150523341, 0.0303407196563573, 0.0304465764860094, 0.0305467368063701, 0.0306448142052752, 0.0307309483756326, 0.0308679532069217, 0.0309660892281943, 0.0310699779482759, 0.0311687355939711))
  expect_equal(excelDetails$report_predicted_df$DR,c(0.0230550377710193,0.0302614827147194, 0.0266152171762145, 0.0263970794720584, 0.0205159813330985, 0.0144808966702119, 0.0125526215078454, 0.00605585636933599, 0.00501704508908471, 0.00549868382568002, 0.00671528251403389, 0.00795537230172205, 0.00534619898291824, 0.00773628048780488, 0.00672400846474162, 0.00717620883081069, 0.00674351585014409, 0.0079463555004092, 0.00889904687803929, 0.0105156816825091, 0.0103572046358762, 0.0175547294506402, 0.0193396321440803, 0.0230583501006036, 0.0251624695369618, 0.0261583675511182, 0.0287639780602917, 0.0280755597821388, 0.0279320884367401, 0.0292564090888393, 0.025986369326348, 0.023807814705812, 0.022279689675751, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
})


# test_that("scenario_check",{
#   upturn=copy(macrodata)
#   
#   downturn=copy(macrodata)
#   
#   sen=1.05 #mult factor  to be applied to generate upturn and downturn
#   
#   mevs<-colnames(macrodata)[!colnames(macrodata) %in% "Date"] #identofy all the MEV columns
#   
#   upturn<-upturn[,c(mevs):=.SD*sen,.SDcols = mevs] # One needs to read scenario data from csv or excel. for now creating dummy data
#   
#   downturn<-downturn[,c(mevs):=.SD*0.95,.SDcols = mevs] # One needs to read scenario data from csv or excel, for now creating dummy data
#   
#   model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
#   call_excel(models,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn')) # 
#   excelDetails <- reporter(model,report_type = "excel",scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity)
#   
# })