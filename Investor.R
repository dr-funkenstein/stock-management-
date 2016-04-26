# Methods
# - initialize 
# - invest
# - setCompany
# - [


setMethod('initialize', 'Investor',
	def = function(.Object, paths, date, amount,...){
		.Object@companies <- mapply(AcquiredCompany, paths, date, amount)

		ticks <- sapply(.Object@companies, function(x) {x@tick})
		names(.Object@companies) <- ticks

		.Object@misc$ticks <- unname(ticks)

		.Object@history <- data.frame(
			t(sapply(seq_along(paths), function(i){.Object@companies[[i]]['events']})),
			row.names = ticks
		)
		
		.Object@current <- data.frame(amount, row.names = ticks)

		return(.Object)
	}
)

setValidity('Investor', 
	function(object){
		if ( any(! sapply(object@companies, class) == "AcquiredCompany" )){
			stop("All companies must be of class AcquiredCompany!")
		}
	}
)

setMethod('[', 'Investor',
	def = function(x,i,j,drop){
		if (length(i) > 1 && all(is.element(i, x@misc$ticks))) {
			return(sapply(i, function(l){x@companies[[l]]}))
		}
		if (substr(i, 1,1)=='-'){
			ticks <- x@misc$ticks[!x@misc$ticks == substring(i,2)]
			return(sapply(ticks, function(l){ x@companies[[l]] }))
		}
		if (is.element(i, x@misc$ticks)) return(x@companies[[i]])
		if (i == 'history') 		return(x@history)
		if (i == 'ticks') 			return(x@misc$ticks)
		if (i == 'current')			return(x@current)
		if (i == 'companies')   	return(x@companies)
	}
)

setReplaceMethod('setCompany', 'Investor', 
	def = function(object, value){
	# Value should be a list consisting of a tick and a AcquiredCompany
		temp <-  object[paste('-',value$tick,sep='')]
		temp[[value$tick]] <- value$AcquiredCompany
		object@companies <- temp
		validObject(object)
		return(object)
	}
)

setMethod('invest', 'Investor',
	def = function(object, tick, date, amount, action){
		setCompany(object) <- list(tick = tick, 
			AcquiredCompany = invest(object[tick], action, amount, date))

		object@history <- rbind(object@history, 
			tick = tail(object[tick]['events'] ,n=1) )

		object@current[[tick,'amount']] <- object@current[[tick,'amount']] + 
			ifelse(action=='Buy',amount, - amount )

		return(object)
	}
)

setMethod('show', 'Investor',
	def = function(object){
		cat('Currently holding the following investments \n')
		print(object@current)
	}
)