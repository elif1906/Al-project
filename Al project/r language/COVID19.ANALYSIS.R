install.packages("covid19.analytics")

library(covid19.analytics)



data <- covid19.data(case='aggregated')

ts.confirmed <- covid19.data(case = "ts-confirmed")

ts.all <- covid19.data(case = "ts-all")

report.summary(Nentries = 8, graphical.output = T)

options(scipen = 999)
tots.per.location(ts.confirmed, geo.loc = 'Turkey')

tots.per.location(ts.confirmed, geo.loc = c('Turkey', 'US','China'))

growth.rate(ts.confirmed, geo.loc = 'Turkey')

totals.plt(ts.all)

totals.plt(ts.all, c('Turkey'))

live.map(ts.confirmed)

generate.SIR.model(ts.confirmed, 'Turkey', tot.population = 1352600000)