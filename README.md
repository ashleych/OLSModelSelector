# OLSModelSelector
Making Linear Regression Models Selection Easier



## Set up the package
```
library(devtools)
install_github("ashleych/OLSModelSelector") - uncomment and install this package
```
## Input Data

Two files need to be input - macrodata and macrometa. Descriptions as below

### Macrodata description

Do note that the data contains forecast values also
```
#glimpse(macrodata)
glimpse(tail(macrodata)) # shows last 6 rows. Note the NAs in the DR column
```


### Macrometa description

Macrometa contains the variable names and the direction. The column names should mandatorily be Variable and Type. If other column names are used, the code would fail
```{r macrometa, echo=TRUE}
glimpse(macrometa)
```

## Creating Validation datasets
Run the validation sampler to split the data into training, test and forecast data.Row numbers to be provided and code would subset by index. FOr instance, in the below sample, rows 1 to 29 would be the training data set, 30 to 33, for out of time valdiation, and the entire data for forecasting. Please note that the MAPE is run on test and training data, and the prediction is run on the forecast data,once model is chosen. 
once validation sampler is run, it should produce three datasets in your global environment, test_df, train_df, forecast_df. it is not mandatory to create these via validation sampler. but if these data frames are not in the global enviroment, the other functions like modelDeveloper require that alterate test and trainig data be provided.

```{r echo=TRUE}
validationSampler(macrodata,1:29,30:33,1:48)

```

## Variable Selection

it is assumed variable selection has happened outside of the package via say lasso regression or stepwise.  lets assume the list below shows the variables of interest. These are only the independent variables
```
vars <-
  c(
    "ECI_yoy_ch_3QMA_lag_4",
    "avg_oil_pri_barrel_3QMA",
    "Rl_est_Dub_q_yoy_ch_lag_1",
    "avg_oil_pri_barrel_3QMA_lag_2",
    "Non_oil_ECI_yoy_ch_3QMA_lag_3",
    "Non_oil_ECI_yoy_ch_6QMA",
    "avg_oil_pri_barrel",
    "avg_oil_pri_barrel_3QMA_lag_1",
    "avg_oil_pri_barrel_6QMA_lag_1",
    "avg_oil_pri_barrel_lag_2",
    "avg_oil_pri_barrel_lag_3"
  )
```

## Models Development

Run model developer code. LHS_vars is the response variable. It generates all possible combinations of models. Provide no_of_vars to indicate number of variables that you see in the model. In CBUAE stress testing, typically we dont see more than 4 variables in a single model. This is merely to keep the number of models to a reasonable count. There is no constraint however.

```{r echo=TRUE, message=FALSE, warning=FALSE}
allModels <-
  modelDeveloper(
    LHS_vars = "DR",
    RHS_vars = vars,
    trainData = train_df,
    no_of_vars = 2
  )
```

## Models Diagnistics

Run modelDiagnostics to include all diagnostic tests. This computes the various statistics but doesnt classify it as pass or fail. In our modelling workflow, one can either take this into excel and then do further analysis or use MOdelEvaluator to classify each model as Pass or Fail

The biggest advantage is that each model and its stats are in a single row.
```
allModelsDiagnostics <- modelDiagnostics(allModels)
```
## Models Evaluation

Run modelEvaluator to check for various thresholds. It classifies various models into pass or fail.
```
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
glimpse(allModelEvaluated)
```
 
 

one can see how many models have passed

```
table(allModelEvaluated$FinalResults)
```


## Individual models analysis

One can select which models have passed

```
allModelEvaluated[FinalResults=="PASS",model]
```
TO analyse individual models copy the model name 
```
selectedModel <-
  "DR ~ ECI_yoy_ch_3QMA_lag_4+avg_oil_pri_barrel_3QMA_lag_1" 
selectedModelObject <- allModels[[selectedModel]] # selectedmodel is a string whereas selectedmodelobject is the actual model object

```

See results of individual models. This is not made pretty, code yet to be brought in for this
```
summary(selectedModelObject)
```
See diagnostics test in a pretty format

```
selectedModelDiagnostics(selectedModel, allModelEvaluated)
```

## Forecasting

Forecast values
```
selectedModelForecaster(selectedModel,selectedModelObject, allModelEvaluated)
```

## Charting

See chart of predicted values
```
p<- selectedModelCharter(selectedModel, selectedModelObject,allModelEvaluated)
p
```
For a more interactive chart use plotly
```
ggplotly(p)

```

## Export selected model to Excel.

```
selectedMOdelDf <- allModelsDiagnostics[model==selectedModel] 
write.csv(selectedMOdelDf,"selectedMOdelDf.csv") # saves this to current working directory
```

## Automatic report generation

```{r echo=TRUE, message=FALSE, warning=FALSE}
models <-
  c(
    "DR~avg_oil_pri_barrel_lag_3+avg_oil_pri_barrel_lag_2",
    "DR~avg_oil_pri_barrel_lag_2",
    "DR~Non_oil_ECI_yoy_ch_6QMA+avg_oil_pri_barrel_6QMA_lag_1"
  )
call_excel(models,report_title = "Portfolio A")

```


## Scenario Analysis
```

ST.auto.1::validationSampler(macrodata,1:29,30:33,1:48)

upturn=copy(macrodata)

downturn=copy(macrodata)

sen=1.05 #mult factor  to be applied to generate upturn and downturn

mevs<-colnames(macrodata)[!colnames(macrodata) %in% "Date"] #identofy all the MEV columns

upturn<-upturn[,c(mevs):=.SD*sen,.SDcols = mevs] # One needs to read scenario data from csv or excel. for now creating dummy data

downturn<-downturn[,c(mevs):=.SD*0.95,.SDcols = mevs] # One needs to read scenario data from csv or excel, for now creating dummy data
```

## Scenario results
```
model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
call_excel(model,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn')) # this will generate per model scenario results as well as some charts, if you dont procide scenario names,it will name the scenarios as scenario_1, scenario_2 etc
```


## scenario results where you want to control colours in the graph

```
model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
call_excel(model,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn'),scenario_colors=c('green','red')) # provision for coresponding colors for each scenario, this is to avoid R allocating unintuitive colours to scenarios (eg green to downturn)
```

please note in this the baseline is assumed to be the the details already in forecast_df, this by default is shaded gray. So to avoid confusion do not provide gray as an input colour
If you need more colours, run colors() to see the list of all colors in R, ensure the strings are from that list. 
TODO: to do a validation check for color validity

## Sensitivity analysis

 user has to provide the list of MEVs that are to be sensitised in the format below
 the user has to provide the sensitivity value, and the system will do the upturn and downturn using that. F
 for eg, if user provides a value of 0.1, then the tool will multiply that baseline MEV from forecast_df by 1.1 and also by 0.9
```
call_excel(model,scenarios=list(upturn,downturn),sensitivity=c(ad_hot_occ=0.05,avg_oil_pri_barr=0.07))

```
TODO: Graphs for sensitivity

 No need to provide scenarios as such here, which is just shown for comprehensiveness. Sensitivity analsys can be done without Sccenario analysis and vice versa
 
 
### Installation issues on Windows especially
Try running this in case it fails to install because of warnings
```
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE) 

```

