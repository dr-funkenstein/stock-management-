# Methods 
# -initialize
# -invest 
# -show
# - [
# and all for Company since inheritance 

setMethod("initialize", 'AcquiredCompany', 
  def = function(.Object, path, date, amount,...){
    .Object = callNextMethod(.Object,...,path)
    data = .Object@timeseries
    # Currently the proram does not handle going short directly, 
    # must first go long
    .Object@events <- data.frame(Date =date, Amount = amount, Type = 'Buy', 
      Price = amount * data$Closing.price[data$Date == date], Total = amount,
      stringsAsFactors=FALSE)
    return(.Object)
  }
)


setMethod('invest', 'AcquiredCompany', 
  def = function(object, type, amount, date){
    # Adds an event to event and does the needed calculations 
    price = amount * 
          object@timeseries$Closing.price[object@timeseries$Date == date]
    if(type == 'Buy'){

      total <- tail(object@events$total, n = 1) + amount

      cat("You now own ", total, " Stocks.\n", sep='')
      object@events <- rbind(object@events, c(date, amount, type, 
        price, total))
    } else if (type == 'Sell'){

      total <- tail(object@events$Total, n = 1) - amount

      price <- - price  # Selling comes with a negative price

      if(total < 0) {
        cat("You are now short " ,total, " Stocks.\n", sep='')
      } else if (total > 0){
        cat("You have " ,total, " Stocks left.\n", sep='')
      } else {
        cat("You have sold of all of your holdings in ", object@tick)
      }

      object@events <- rbind(object@events, c(date, amount, type, 
        price, total))
    } else {
      stop ('Can only buy or sell stocks')
    }
    return(object)
  }
)

setMethod('show', 'AcquiredCompany',
  def = function(object){
    # Showing the new information that is added in this layer
    callNextMethod()
    cat('You own ', tail(object@events$Total,n=1), 
      ' Stocks with a current market value of ',  
      as.numeric(tail(object@events$Total,n=1)) * 
      tail(object@timeseries$Closing.price,n=1 ), 
      '.\n' ,sep ='')
  }
)

setMethod('[','AcquiredCompany',
  function(x, i, j, drop){
    # The only new thing that is added since Company is events
    if(i == 'events') return(x@events)
    callNextMethod()
  }
)
