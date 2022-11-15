# Classes -----------------------------------------------------------------

# Difference Class --------------------------------------------------------
setGeneric(name="transform",
           def=function(theObject)
           {standardGeneric("transform")})


setGeneric(name = "untransform",
           def = function(theObject,x)
           {standardGeneric("untransform")})

differenceClass <- setClass(
  "differenceClass",
  slots = list(
    differenceData = 'numeric',
    differenceOrder = 'numeric',
    lag = 'numeric',
    undifferenceData = 'numeric',
    startData = 'numeric',
    inputData = 'numeric'
    #transformData = 'transformClass'
  )
)




## Differencing transform  ----------------------------------------------------------


setMethod(
  f = 'transform',
  signature = 'differenceClass',
  definition = function(theObject) {
    theObject@differenceData <-
      diff(theObject@inputData, lag = theObject@lag, differences=theObject@differenceOrder)
    theObject@startData <- theObject@inputData[1:(theObject@lag * theObject@differenceOrder)]
    return(theObject)
  }
)
## Differencing untransform  ----------------------------------------------------------
setMethod(
  f = 'untransform',
  signature = 'differenceClass',
  definition = function(theObject,x) {
    theObject@undifferenceData <-
      diffinv(x,differences = theObject@differenceOrder,lag = theObject@lag,xi=theObject@startData)
    return(theObject)
  }
)

# LogitClass --------------------------------------------------------------

logitClass <- setClass(
  'logitClass',
  slots = list(
    logitData = 'numeric',
    unlogitData = 'numeric',
    inputData = 'numeric'
  )
)

## logitTransform ----------------------------------------------------------

setMethod(f="transform",signature="logitClass",
          definition=function(theObject)
          { x <- theObject@inputData
          theObject@logitData <-
            log(x / (1 - x))
          return(theObject)
          })


## logitUntransform --------------------------------------------------------

setMethod(
  f = "untransform",
  signature = "logitClass",
  definition = function(theObject,x)
  {
    #x <- theObject@logitData
    theObject@unlogitData <-
      exp(x)/ (1 + exp(x))
    return(theObject)
  }
)



# Log Class --------------------

logClass <- setClass(
  'logClass',
  slots = list(
    logData = 'numeric',
    unlogData = 'numeric',
    inputData = 'numeric'
  )
)

## logTransform ----------------------------------------------------------

setMethod(f="transform",signature="logClass",
          definition=function(theObject)
          { 
            x <- theObject@inputData
          theObject@logData <-log(x)
          return(theObject)
          })


## log Untransform --------------------------------------------------------

setMethod(
  f = "untransform",
  signature = "logClass",
  definition = function(theObject,x)
  {
    theObject@unlogData <-exp(x)
    return(theObject)
  }
)


# multiply Class --------------------

multiplyClass <- setClass(
  'multiplyClass',
  slots = list(
    multiplyData = 'numeric',
    multiplyFactor= 'numeric',
    unmultiplyData = 'numeric',
    inputData = 'numeric'
  )
)

## multiplyTransform ----------------------------------------------------------

setMethod(f="transform",signature="multiplyClass",
          definition=function(theObject)
          { 
            # x <- theObject@inputData
            theObject@multiplyData <-theObject@inputData * theObject@multiplyFactor
            return(theObject)
          })


## multiply Untransform --------------------------------------------------------

setMethod(
  f = "untransform",
  signature = "multiplyClass",
  definition = function(theObject,x)
  {
    theObject@unmultiplyData <-x / theObject@multiplyFactor
    return(theObject)
  }
)


# TransformClass ----------------------------------------------------------

transformOrderClass <- setClass(
  'transformOrderClass',
  slots = list(
    order = 'numeric',
    type = 'character',
    lag = 'numeric',
    differences = 'numeric',
    multiplyFactor ='numeric'
  )
)



transformClass <- setClass(
  'transformClass',
  slots = list(
    baseData = 'numeric',
    # typically this will be the untrasformed response variable on which all transofmrations would be done
    inputData = 'numeric',
    # after each transform this holds the result of the transform, to be passed on to the next transform function
    logitObject = 'logitClass',
    differencesObject = 'differenceClass',
    multiplyObject = 'multiplyClass',
    logObject='logClass',
    outputData = 'numeric',
    transform_order_1 = 'transformOrderClass',
    transform_order_2 = 'transformOrderClass',
    transform_order_3 = 'transformOrderClass',
    transform_order_4 = 'transformOrderClass',
    tranformCount = 'numeric',
    transform_flag = 'logical',
    no_of_elements_to_be_removed_for_untransform='numeric'
  )
)


setGeneric(
  name = "SettransformOrder",
  def = function(theObject, orderList)
  {
    standardGeneric("SettransformOrder")
  }
)

setMethod(
  f = "SettransformOrder",
  signature = "transformClass",
  definition = function(theObject, orderList)
  {
    stopifnot(nrow(orderList)<6) # only 5 transformations set up
    
    for (i in 1:nrow(orderList)) {
      transform_order <- transformOrderClass(
        # order = orderList[[i]]$order,
        # type = orderList[[i]]$type,
        # # can be 3 strings - "log", "logit" or "differences"
        # lag = orderList[[i]]$lag,
        # differences = orderList[[i]]$differences
        order = orderList[i,order],
        type = orderList[i,type],
        # can be 3 strings - "log", "logit" or "differences"
        lag =orderList[i,lag],
        differences = orderList[i,differences],
        multiplyFactor =  orderList[i,multiplyFactor]
      )
      slot(theObject, paste0("transform_order_", orderList[i,order])) <- transform_order
      slot(theObject, "tranformCount") <- i
      
    }
    
    return(theObject)
  }
)

setMethod(
  f = "transform",
  signature = "transformClass",
  definition = function(theObject)
  {
    
    for (i in 1:theObject@tranformCount) {
      if (i == 1) {
        theObject@inputData <- theObject@baseData
      }
      
      transformation <- slot(theObject, paste0("transform_order_", i))
      if (transformation@type == "logit") {
        theObject@logitObject <- logitClass(inputData = theObject@inputData)
        theObject@logitObject <- transform(theObject@logitObject)
        theObject@inputData <- theObject@logitObject@logitData
      }
      if (transformation@type == "difference") {
        theObject@differencesObject <- differenceClass(inputData = theObject@inputData,
                                                       differenceOrder =transformation@differences,
                                                       lag= transformation@lag)
        
        theObject@differencesObject <-
          transform(theObject@differencesObject)
        theObject@inputData <- theObject@differencesObject@differenceData
        
      }
      
      if (transformation@type == "log") {
        theObject@logObject <- logClass(inputData = theObject@inputData)
        theObject@logObject <- transform(theObject@logObject)
        theObject@inputData <- theObject@logObject@logData
      }
      
      if (transformation@type == "multiply") {
        
        theObject@multiplyObject <- multiplyClass(inputData = theObject@inputData,multiplyFactor=transformation@multiplyFactor)
        theObject@multiplyObject <- transform(theObject@multiplyObject)
        theObject@inputData <- theObject@multiplyObject@multiplyData
      }

      
      
    }
    return(theObject)
  }
)

setMethod(
  f = "untransform",
  signature = "transformClass",
  definition = function(theObject,x)
  {
    
    for (i in theObject@tranformCount:1) {
      if (i == theObject@tranformCount) {
        theObject@inputData <- x
      }
      
      transformation <- slot(theObject, paste0("transform_order_", i))
      if (transformation@type == "logit") {
        # theObject@logitObject <- logitClass(inputData = theObject@inputData)
        theObject@logitObject@inputData <- theObject@inputData
        theObject@logitObject <- untransform(theObject@logitObject,theObject@inputData)
        theObject@inputData <- theObject@logitObject@unlogitData
      }
      if (transformation@type == "difference") {
        # theObject@differencesObject <- differenceClass(inputData = theObject@inputData,
        #                                                differenceOrder =transformation@differences,
        #                                                lag= transformation@lag)
        theObject@differencesObject@inputData <- theObject@inputData
        theObject@differencesObject <-
          untransform(theObject@differencesObject,theObject@inputData)
        theObject@inputData <- theObject@differencesObject@undifferenceData
        
      }
      
      if (transformation@type == "log") {
        # theObject@logitObject <- logitClass(inputData = theObject@inputData)
        theObject@logObject@inputData <- theObject@inputData
        theObject@logObject <- untransform(theObject@logObject,theObject@inputData)
        theObject@inputData <- theObject@logObject@unlogData
      }
      
      if (transformation@type == "multiply") {
        # theObject@logitObject <- logitClass(inputData = theObject@inputData)
        theObject@multiplyObject@inputData <- theObject@inputData
        theObject@multiplyObject <- untransform(theObject@multiplyObject,theObject@inputData)
        theObject@inputData <- theObject@multiplyObject@unmultiplyData
      }
      
    }
    return(theObject)
  }
)


#https://stats.stackexchange.com/questions/188595/convert-double-differenced-forecast-into-actual-value 

