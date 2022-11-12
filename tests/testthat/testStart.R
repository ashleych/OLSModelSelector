
# 
# macrodata[,DR_logit := logit(DR)]
# macrodata[,DR_logit_FD := DR_logit - shift(DR_logit, n = 1, type = 'lag')]
validationSampler(macrodata,1:29,30:33,1:48)
# transformConfig  <-  data.table(stringsAsFactors=FALSE,
#                                 varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit"),
#                                 baseVarName = c("DR", "DR", "DR"),
#                                 order = c(1L, 2L, 1L),
#                                 type = c("logit", "difference", "logit"),
#                                 lag = c(0L, 1L, 0L),
#                                 differences = c(0L, 1L, 0L))


dr_log_test_assertions <- function(excelDetails,base_variable='DR'){
  expect_equal(as.numeric(excelDetails$report_summary$estimates_excel[,Estimates][1]), -2.451137)
  expect_equal(as.numeric(excelDetails$report_summary$estimates_excel[,Estimates][2]), -0.021689)
  expect_equal(excelDetails$report_summary$estimates_excel[,Significance][2], "PASS")
  
  
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$R2), 0.806073,tolerance = 0.0001)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$adjR2),0.79889,tolerance = 0.0001)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$MAPE),0.04155,tolerance = 0.0001)
  
  
  expect_equal(excelDetails$report_predicted_df$Date,c( "31/03/2010","30/06/2010", "30/09/2010", "31/12/2010", "31/03/2011", "30/06/2011", "30/09/2011", "31/12/2011", "31/03/2012", "30/06/2012", "30/09/2012", "31/12/2012", "31/03/2013", "30/06/2013", "30/09/2013", "31/12/2013", "31/03/2014", "30/06/2014", "30/09/2014", "31/12/2014", "31/03/2015", "30/06/2015", "30/09/2015", "31/12/2015", "31/03/2016", "30/06/2016", "30/09/2016", "31/12/2016", "31/03/2017", "30/06/2017", "30/09/2017", "31/12/2017", "31/03/2018", "30/06/2018", "30/09/2018", "31/12/2018", "31/03/2019", "30/06/2019", "30/09/2019", "31/12/2019", "31/03/2020", "30/06/2020", "30/09/2020", "31/12/2020", "31/03/2021", "30/06/2021", "30/09/2021", "31/12/2021"))
  expect_equal(excelDetails$report_predicted_df$predicted_values,c(-3.72418997654454, -3.93030464185877, -4.06976279321991, -4.10489843944154, -4.15391483478775, -4.11726098163063, -4.32648435652881, -4.72758016970255, -4.99651968399153, -4.90933122855268, -4.82380557190901, -5.02102788166463, -4.80255067481198, -4.82850478240632, -4.83877075455896, -4.89096815346698, -4.67588883905007, -4.84274701009565, -4.81975701935805, -4.79712850718518, -4.83038446790656, -4.65955003915542, -4.10822403551367, -3.62051813152024, -3.78918369235312, -3.54063152834089, -3.39611268726704, -3.18197321386515, -3.43847789025209, -3.4441892402969, -3.5179307200213, -3.61538514609458, -3.52841357736081, -4.11726098163063, -4.32648435652881, -4.72758016970255, -4.99651968399153, -4.90933122855268, -4.82380557190901, -5.02102788166463, -4.80255067481198, -4.82850478240632, -4.83877075455896, -4.89096815346698, -4.67588883905007, -4.84274701009565, -4.81975701935805, -4.84274701009565))
  expect_equal(excelDetails$report_predicted_df$predicted_values_transformed,c(0.0241326405166435, 0.0196376891795849, 0.0170814398603102, 0.0164916936589308, 0.0157028220434878, 0.0162890694560539, 0.0132139213616802, 0.00884785546467308, 0.00676143803829908, 0.00737742049011937, 0.00803614673966761, 0.00659774152518729, 0.00820878238889466, 0.0079984717851515, 0.0079167797381417, 0.00751414410760428, 0.00931723992947729, 0.0078853631005577, 0.00806874745036639, 0.00825341267010835, 0.00798345129503413, 0.00947072289758407, 0.0164369400419452, 0.0267688031381724, 0.0226140543441297, 0.0289950101283277, 0.0335032547797198, 0.0415036786350646, 0.0321135284608363, 0.0319306396268427, 0.0296607481014005, 0.0269065602647412, 0.0293514427449001, 0.0162890694560539, 0.0132139213616802, 0.00884785546467308, 0.00676143803829908, 0.00737742049011937, 0.00803614673966761, 0.00659774152518729, 0.00820878238889466, 0.0079984717851515, 0.0079167797381417, 0.00751414410760428, 0.00931723992947729, 0.0078853631005577, 0.00806874745036639, 0.0078853631005577))
  
  expect_equal(as.numeric(unlist(excelDetails$report_predicted_df[,..base_variable])),c(0.0230550377710193,0.0302614827147194, 0.0266152171762145, 0.0263970794720584, 0.0205159813330985, 0.0144808966702119, 0.0125526215078454, 0.00605585636933599, 0.00501704508908471, 0.00549868382568002, 0.00671528251403389, 0.00795537230172205, 0.00534619898291824, 0.00773628048780488, 0.00672400846474162, 0.00717620883081069, 0.00674351585014409, 0.0079463555004092, 0.00889904687803929, 0.0105156816825091, 0.0103572046358762, 0.0175547294506402, 0.0193396321440803, 0.0230583501006036, 0.0251624695369618, 0.0261583675511182, 0.0287639780602917, 0.0280755597821388, 0.0279320884367401, 0.0292564090888393, 0.025986369326348, 0.023807814705812, 0.022279689675751, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  
}

test_that("Testing Log transform - where column names arent DR",{ 
  macrodata<- copy(ST.auto.1::macrodata)
  setnames(macrodata,'DR','def_rate')
  setnames(macrodata,'DR_log','def_rate_log')
  list2env(list(macrodata=macrodata), envir = .GlobalEnv) ##put this in the global environment

  validationSampler(macrodata,1:29,30:33,1:48)
  expect_error( reporter(models,untransform=TRUE,report_type = 'unformatted'))  #cos no transformconfig with the right column names

  untransform_config  <-  data.table(stringsAsFactors=FALSE,
                                     varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit",'def_rate_log'),
                                     baseVarName = c("DR", "DR", "DR","def_rate"),
                                     order = c(1L, 2L, 1L,1L),
                                     type = c("logit", "difference", "logit","log"),
                                     lag = c(0L, 1L, 0L,0L),
                                     differences = c(0L, 1L, 0L,0L))
  
  models <- c(" def_rate_log ~ avg_oil_pri_barrel_lag_3")
  excelDetails <- reporter(models,untransform=TRUE,report_type = 'unformatted',transformConfig=untransform_config)
  dr_log_test_assertions(excelDetails,base_variable = 'def_rate')
  rm(macrodata)
  })
test_that("Testing Log transform",{
  
  macrodata<- copy(ST.auto.1::macrodata)

  list2env(list(macrodata=macrodata), envir = .GlobalEnv) ##put this in the global environment
  
  validationSampler(macrodata,1:29,30:33,1:48)
  models <- c(" DR_log ~ avg_oil_pri_barrel_lag_3")
  scenario_colors=c()
  scenarios=c()
  scenario_names<- c()
  sensitivity=c()
  # excelDetails <- reporter(models,untransform=TRUE,scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted')
  
  excelDetails <- reporter(models,untransform=TRUE,report_type = 'unformatted')
  dr_log_test_assertions(excelDetails)
  expect_equal(excelDetails$modelName,models)
  
  
  
  untransform_config  <-  data.table(stringsAsFactors=FALSE,
                                     varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit",'DR_log'),
                                     baseVarName = c("DR", "DR", "DR","DR"),
                                     order = c(1L, 2L, 1L,1L),
                                     type = c("logit", "difference", "logit","log"),
                                     lag = c(0L, 1L, 0L,0L),
                                     differences = c(0L, 1L, 0L,0L))
  
  

  # #test with custom input for transform
  excelDetails_1 <- reporter(models,untransform=TRUE,transformConfig=untransform_config, scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted')
  # 
  dr_log_test_assertions(excelDetails_1)
  
  ## try with an incorrect transform logic
  untransform_config  <-  data.table(stringsAsFactors=FALSE,
                                     varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit",'DR_log1'), #here there is no value called DR_log1
                                     baseVarName = c("DR", "DR", "DR","DR"),
                                     order = c(1L, 2L, 1L,1L),
                                     type = c("logit", "difference", "logit","log1"),
                                     lag = c(0L, 1L, 0L,0L),
                                     differences = c(0L, 1L, 0L,0L))
  
  
  expect_error(reporter(models,untransform=TRUE,transformConfig=untransform_config, scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted'))

})

test_that("Testing sensitivity with transformations",{


  models <- c(" DR_logit_FD ~ avg_oil_pri_barrel_lag_3")

  scenario_colors=c()
  scenarios=c()
  scenario_names<- c()
  sensitivity=c(avg_oil_pri_barrel_lag_3=0.05,avg_oil_pri_barr=0.07)

  excelDetails <- reporter(models,scenarios=c(),scenario_names=c(),scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted',untransform=TRUE)
  expect_equal(excelDetails$modelName,models)
  expect_equal(as.numeric(excelDetails$report_summary$estimates_excel[,Estimates][1]), 0.022112)
  expect_equal(as.numeric(excelDetails$report_summary$estimates_excel[,Estimates][2]), -0.000171)
  
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$R2),0.000316)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$adjR2),-0.038134)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$MAPE),1.039405)
  
  
  expect_equal(excelDetails$report_predicted_df$Date,c( "31/03/2010","30/06/2010", "30/09/2010", "31/12/2010", "31/03/2011", "30/06/2011", "30/09/2011", "31/12/2011", "31/03/2012", "30/06/2012", "30/09/2012", "31/12/2012", "31/03/2013", "30/06/2013", "30/09/2013", "31/12/2013", "31/03/2014", "30/06/2014", "30/09/2014", "31/12/2014", "31/03/2015", "30/06/2015", "30/09/2015", "31/12/2015", "31/03/2016", "30/06/2016", "30/09/2016", "31/12/2016", "31/03/2017", "30/06/2017", "30/09/2017", "31/12/2017", "31/03/2018", "30/06/2018", "30/09/2018", "31/12/2018", "31/03/2019", "30/06/2019", "30/09/2019", "31/12/2019", "31/03/2020", "30/06/2020", "30/09/2020", "31/12/2020", "31/03/2021", "30/06/2021", "30/09/2021", "31/12/2021"))
  expect_equal(excelDetails$report_predicted_df$predicted_values,c(0.0120854340415657, 0.0104620316789221, 0.00936363000981021, 0.00908689428602153, 0.00870083086888422, 0.00898952430913292, 0.00734163709736091, 0.00418252229221091, 0.00206429823358146, 0.00275101280742746, 0.00342463082898025, 0.0018712665250128, 0.00359203885942033, 0.00338761886084949, 0.00330676190958878, 0.00289564424078071, 0.00458965406122645, 0.00327544408645911, 0.00345651807856775, 0.00363474498963463, 0.00337281406334771, 0.00471834187832719, 0.00906070119339284, 0.0129019752512632, 0.0115735298952025, 0.0135311789042261, 0.0146694396276353, 0.0163560470629961, 0.0143357623974636, 0.0142907786058324, 0.0137099752349178, 0.0129424037217688, 0.0136274100504063, 0.00898952430913292, 0.00734163709736091, 0.00418252229221091, 0.00206429823358146, 0.00275101280742746, 0.00342463082898025, 0.0018712665250128, 0.00359203885942033, 0.00338761886084949, 0.00330676190958878, 0.00289564424078071, 0.00458965406122645, 0.00327544408645911, 0.00345651807856775, 0.00327544408645911))
  expect_equal(excelDetails$report_predicted_df$predicted_values_transformed,c(0.0230550377710193, 0.0232918589061391, 0.0235058289243875, 0.0237153087550849, 0.0239175951736385, 0.0241283611143648, 0.0243018339259877, 0.0244012043420647, 0.0244503948598512, 0.0245160995100189, 0.0245981332631021, 0.0246430706460066, 0.0247295557169964, 0.0248113900027262, 0.0248915255495173, 0.0249619052208802, 0.0250738558171433, 0.0251540492406682, 0.0252389468962437, 0.0253285231606707, 0.0254119212240622, 0.0255290384523232, 0.0257554159514648, 0.026081141773244, 0.0263767380525082, 0.026726468309255, 0.0271107129701408, 0.0275454684916615, 0.0279320884367401, 0.0283227388049102, 0.0287024948321203, 0.0290655210176825, 0.0294525742631732, 0.0297106307358973, 0.0299230070998605, 0.030044654765772, 0.0301048708861222, 0.0301853004391994, 0.0302857150523341, 0.0303407196563573, 0.0304465764860094, 0.0305467368063701, 0.0306448142052752, 0.0307309483756326, 0.0308679532069217, 0.0309660892281943, 0.0310699779482759, 0.0311687355939711))
  expect_equal(excelDetails$report_predicted_df$DR,c(0.0230550377710193,0.0302614827147194, 0.0266152171762145, 0.0263970794720584, 0.0205159813330985, 0.0144808966702119, 0.0125526215078454, 0.00605585636933599, 0.00501704508908471, 0.00549868382568002, 0.00671528251403389, 0.00795537230172205, 0.00534619898291824, 0.00773628048780488, 0.00672400846474162, 0.00717620883081069, 0.00674351585014409, 0.0079463555004092, 0.00889904687803929, 0.0105156816825091, 0.0103572046358762, 0.0175547294506402, 0.0193396321440803, 0.0230583501006036, 0.0251624695369618, 0.0261583675511182, 0.0287639780602917, 0.0280755597821388, 0.0279320884367401, 0.0292564090888393, 0.025986369326348, 0.023807814705812, 0.022279689675751, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  expect_equal(excelDetails$ModelSensitivities_df_list[[1]][,predicted_base],excelDetails$report_predicted_df$predicted_values)
  
  expect_equal(excelDetails$ModelSensitivities_df_list[[1]][,predicted_transformed_base],excelDetails$report_predicted_df$predicted_values_transformed)
  expect_equal(nrow(excelDetails$ModelSensitivities_df_list[[1]]),nrow(excelDetails$report_predicted_df))
  expect_equal(excelDetails$ModelSensitivities_df_list[[1]]$predicted_transformed_up,c(0.0230550377710193, 0.0232786108257527, 0.0234778434647668, 0.023672023378515, 0.0238583276080441, 0.024053176773006, 0.0242086702897322, 0.0242864165034343, 0.0243115926229238, 0.024353921052711, 0.024413161522454, 0.0244336339865556, 0.0244972637408885, 0.0245559132237737, 0.0246126613163422, 0.0246591528902605, 0.0247486249714805, 0.024805011630218, 0.0248661332473699, 0.0249319506152978, 0.0249912353469718, 0.0250851856803708, 0.0252916372319919, 0.0256001619421311, 0.025877165751993, 0.0262094977552839, 0.0265768837985461, 0.0269957553305965, 0.0273645261058688, 0.0277369170131759, 0.0280975706695085, 0.0284404983138997, 0.0288076037275197, 0.0290416706916944, 0.0292284468877134, 0.0293218297029846, 0.0293520686739378, 0.0294029085104446, 0.0294740595266839, 0.0294986476496245, 0.0295750677744992, 0.0296455047013793, 0.0297136564231139, 0.029769489335919, 0.0298769355283066, 0.0299446476452146, 0.0300180439210194, 0.0300860659234619))
  })


