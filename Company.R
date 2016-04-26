# Methods 
# - initialize
# - plot
# - show
# - summary 
# - [

setMethod("initialize", 'Company', 
  def = function(.Object, path, ...){
    # Storing the path
    .Object@path <- path

    # Reading data
    all_content = readLines(path)
    skip_first = all_content[-1]
    data = read.csv(textConnection(skip_first), sep=";", dec=",")
    data$Date             	<- as.Date((data$Date),origin = "1899-12-30")
    data                  	<- data[ order(data$Date), ]
    .Object@timeseries    	<- data
    .Object@misc$datapoints <- length(data$Date)

    # Getting the name
    tick <- regmatches(path,regexpr("^[^-]*",path))
    tick <- regmatches(tick,regexpr("[^/]*$",path))
    .Object@tick <- tick

    # Calculating daily changes
    .Object@diffTimeseries <- data.frame(
      Date = data$Date[-1],
      change = diff(data$Closing.price))
    .Object@diffTimeseries$ROI <- .Object@diffTimeseries$change / 
      .Object@timeseries$Closing.price[-.Object@misc$datapoints]

    	return(.Object)
  }
)

setValidity('Company', 
  function(object){
      if(!file.exists(object@path)) stop("File does not exists.")
      if(!file_ext(object@path)=='csv') stop("Files need to be .csv.")
    }
  )

setMethod('plot', 'Company', 
  def = function(x,y='',...){
    # Plots anything from the timeseries against date 
    if(y == ''){
      tempDf <- data.frame(x = x@timeseries$Date, y = x@timeseries$Closing.price)
      ylab = 'Closing Price'
    } else {
      tempDf <- data.frame(x = x@timeseries$Date)
      tempDf$y <- x@timeseries[y]
      ylab = y
    }
    pl = ggplot(tempDf, aes(x=x, y= y)) + geom_line() + labs(x='Date', y = ylab)+ 
      ggtitle(x@tick)
    print(pl, newPage =T)
    }
)

setMethod('show', 'Company', 
  def = function(object){
    # To not show everything
    cat('Information about company ', object@tick, ' ranging from ', 
      object@timeseries$Date[1] , ' to ', tail(object@timeseries$Date,n=1),'.\n',
      sep = '')
    if(tail(object@diffTimeseries$change,n=1) > 0){
      cat('Last closing price at: ', tail(object@timeseries$Closing.price, n=1),
        ' which is up ', tail(object@diffTimeseries$change,n=1), ' points since the day before.' ,sep = '')
    } else {
      cat('Last closing price at: ', tail(object@timeseries$Closing.price, n=1),
        ' which is down ', abs(tail(object@diffTimeseries$change,n=1)), ' points since the day before.\n' ,sep = '')
    }
  }
)

setMethod('summary', 'Company', 
  def = function(object){
    # Shows the latest run of the company, a little more information than show
    cat('Information about company ', object@tick, ' ranging from ', 
      object@timeseries$Date[1] , ' to ', tail(object@timeseries$Date,n=1),'.\n',
      sep = '')
    print(tail(object@timeseries))
  }
)

setMethod("[", 'Company', 
  def = function(x, i, j, drop){
    # The "get" operator is nice to have so the user don't have to use @
    if(i=="timeseries") return(x@timeseries)
    if(i=="diffTimeseries") return(x@diffTimeseries) 
    if(i=="tick") return(x@tick)
  }
)
