library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(shinythemes)


df = read.csv('countries-aggregated.csv',
              header = TRUE, na.strings=c("","NA"), sep = ",")

df$Active = df$Confirmed - df$Recovered - df$Deaths


cases = aggregate(list(cases=df$Confirmed), by=list(Date=as.Date(df$Date)), FUN=sum)

deaths = aggregate(list(deaths=df$Deaths), by=list(Date=as.Date(df$Date)), FUN=sum)

recovered = aggregate(list(recovered=df$Recovered), by=list(Date=as.Date(df$Date)), FUN=sum, na.rm=TRUE)

cases$NewCases <- ave(cases$cases, FUN=function(x) c(0, diff(x)))
deaths$NewDeaths <- ave(deaths$deaths, FUN=function(x) c(0, diff(x)))
recovered$NewRec <- ave(recovered$recovered, FUN=function(x) c(0, diff(x)))


newdf = cbind(Date=as.Date(cases$Date), cases[2:3], deaths[2:3], recovered[2:3])

newdf$Active = newdf$cases - newdf$deaths - newdf$recovered


dfz = as.data.frame(cbind(Date = as.Date(newdf$Date), "Confirmed cases" = newdf$cases, "Deaths" = newdf$deaths, "Recovered" = newdf$recovered, "Active cases" = newdf$Active))

dfz$Date = as.Date(newdf$Date)

Molten <- melt(dfz, id.vars = "Date")

dfz2 = as.data.frame(cbind(Date = as.Date(newdf$Date), "New Cases" = newdf$NewCases, "Deaths" = newdf$NewDeaths, "Recovered" = newdf$NewRec))

dfz2$Date = as.Date(newdf$Date)

Molten2 <- melt(dfz2, id.vars = "Date")