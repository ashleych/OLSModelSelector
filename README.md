# OLSModelSelector
Making Linear Regression Models Selection Easier

Installation Issues

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE) in case it fails to install because of warnings

Documentation branch being tested


ST.auto.1::validationSampler(macrodata,1:29,30:33,1:48)



upturn=copy(macrodata)
downturn=copy(macrodata)



sen=1.05 #mult factor  to be applied to generate upturn and downturn

mevs<-colnames(macrodata)[!colnames(macrodata) %in% "Date"] #identofy all the MEV columns
upturn<-upturn[,c(mevs):=.SD*sen,.SDcols = mevs] # One needs to read scenario data from csv or excel. for now creating dummy data
downturn<-downturn[,c(mevs):=.SD*0.95,.SDcols = mevs] # One needs to read scenario data from csv or excel, for now creating dummy data


#scenario results
model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
call_excel(model,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn')) # this will generate per model scenario results as well as some charts, if you dont procide scenario names,it will name the scenarios as scenario_1, scenario_2 etc




model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
call_excel(model,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn'),scenario_colors=c('green','red')) # provision for coresponding colors for each scenario, this is to avoid R allocating unintuitive colours to scenarios (eg green to downturn)
# please note in this the baseline is assumed to be the the details already in forecast_df, this by default is shaded gray. So to avoid confusion do not provide gray as an input colour
# If you need more colours, run colors() to see the list of all colors in R, ensure the strings are from that list. 
##TODO: to do a validation check for color validity

## Sensitivity analysis
# No need to provide scenarios as such here, which is just shown for comprehensiveness. Sensitivity analsys can be done without Sccenario analysis and vice versa
# user has to provide the list of MEVs that are to be sensitised in the format below
# the user has to provide the sensitivity value, and the system will do the upturn and downturn using that. F
# for eg, if user provides a value of 0.1, then the tool will multiply that baseline MEV from forecast_df by 1.1 and also by 0.9
call_excel(model,scenarios=list(upturn,downturn),sensitivity=c(ad_hot_occ=0.05,avg_oil_pri_barr=0.07))
##TODo: Graphs for sensitiviuty