# lnn ---------------------------------------------------------------------

library(RSQLite)
library(tidyverse)


cn <- DBI::dbConnect(RSQLite::SQLite(), "TemperaturaOleo_SmartSub.db")

tempOleo <- DBI::dbReadTable(cn, "cargaOleo") %>% as_tibble()
DBI::dbDisconnect(cn)

#organizando
tempOleo$Tempo <- as.POSIXct(tempOleo$Tempo, format = "%d/%m/%y %H:%M", tz ="")

ts <- ts(tempOleo$Toleo[1:(24*60*2)], start = c(0,1), frequency = 60)

autoplot(ts)

#demora um pouco - modelo auto-regressivo de rede neural
model <- forecast::nnetar(ts)

nnetforecast <- forecast::forecast(model, h = 200, PI = FALSE)
nnetforecast$mean
autoplot(nnetforecast)


#comparando predito com o real
pred <- as.vector(nnetforecast$mean)
real <- tempOleo$Toleo[2881:3080]

ts.plot(ts(real), ts(pred), gpars = list(col = c("black", "blue")))

(pred-real) %>% plot()
