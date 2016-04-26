# This is a test example for an object-oriented approach for a investor - company management system. The base consists of three classes, Investor, AcquiredCompany and Company. 

# Company: This is the first building block, it consists of historical stock prises and some basic information about the company. This class has methods that handles data, updating, writing, but also some plotting functions and analysis of data, if one wishes to have that. 

# AcquiredCompany: This is an extension of company and also contains information about when an Investor bought it, how much and so forth. Inherits all methods from Company. 

# Investor: Owns multiple AcquiredCompany and keeps score on investments. Has methods regarding buying new companies, selling companies, and a plotting function. 

# Loading libraries 
library(ggplot2)


# Setting up the three classes
Company <- setClass(Class = 'Company', slot = c(tick ='character' ,timeseries = 'data.frame', diffTimeseries = 'data.frame', path = 'character', misc = 'list'))

AcquiredCompany <- setClass(Class = 'AcquiredCompany', slot = c(events = 'data.frame'), contains = 'Company')

Investor <- setClass(Class = 'Investor', slot = c(companies = 'list', history = 'data.frame', current = 'data.frame', misc = 'list'))

# Adding new generics to namespace
setGeneric('invest', def = function(object, ...){
	standardGeneric('invest')
})
setGeneric('setCompany<-', def = function(object, value){
	standardGeneric('setCompany<-')
})



source('./Company.R')
source('./AcquiredCompany.R')
source('./Investor.R')

testCompany = Company('./Stocks/AAK-2000-01-01-2015-01-01.csv')

testAcquiredCompany = AcquiredCompany(path = './Stocks/AAK-2000-01-01-2015-01-01.csv', date = '2013-05-23', amount = 100)

path <- "./Stocks"
files <- list.files(path, pattern='*.csv')
files <- paste(path,'/',files, sep='')

amount <- rep(c(200,100,50),3)
dates = rep(c('2014-11-03', '2014-10-17', '2014-01-24'),3)

testInvestor = Investor(files, dates, amount)