# library(ISwR)
library(RODBC)
library(RWeka)
library(rJava)
library(foreign)
library(readr)

library(RSQLite)

# Busca no SQL (Temperaturas é o nome do banco). Para um banco extenso, incuir um select para o intervalo desejado
# As previsões ficam melhores com uma ordem superior a 20000 amostras. Usar o script:
# minhacon <- odbcConnect("Data source name", uid="ID", pwd=" Senha")
# Toleo <- sqlFetch(minhacon, Temperaturas)

cn <- DBI::dbConnect(RSQLite::SQLite(), "TemperaturaOleo_SmartSub.db")

tempOleo <- DBI::dbReadTable(cn, "cargaOleo")

Toleo <- tempOleo

#Criando a variável Toleo, incluindo uma coluna apenas com a hora da estampa de tempo, transformando a estampa de...
#... tempo de character para data

Toleo <- read.csv2("cargaoleo.csv")

Toleo[["Hora"]] <- format(strptime(Toleo$Tempo, "%d/%m/%y %H:%M"),"%H")
Toleo[[5]] = as.numeric(Toleo[[5]])
Toleo$Tempo <- strptime(Toleo$Tempo, "%d/%m/%y %H:%M")

#Criando o vetor base para predição sem a estampa de tempo, apenas variáveis e as horas, incluindo Corrente à 5...
#... minutos atrás e Tamb à 5 minutos atrás

tamanho = dim.data.frame(Toleo)[1]
colunas5minatras <- data.frame(Cor5minatras = Toleo[[2]], Tamb5minatras = Toleo[[3]])
colunas5minatras$Cor5minatras[1:4] = NA
colunas5minatras$Tamb5minatras[1:4] = NA
colunas5minatras$Cor5minatras[5:tamanho] = Toleo$Corrente[1:(tamanho-4)]
vbasepredicao <- data.frame(Toleo = Toleo[[1]], Corrente = Toleo[[2]], Cor5minatras = colunas5minatras[[1]], Tamb = Toleo[[3]], Tamb5minatras = colunas5minatras[[2]], Hora = Toleo[[5]])
vbasepredicao$Hora = as.numeric(vbasepredicao$Hora)

#Criando uma variável com o número de horas que antecipam a predição para auxiliar à consulta do resultado

horas <- 2
minutos <- horas*60

#fazer a regressão numérica para desenhar as relações entre as variáveis poder predizer o futuro

predicao <- RWeka::M5P (data = vbasepredicao[5:tamanho,1:6])






# Inicializando bibliotecas -----------------------------------------------
library(RSQLite)
library(tidyverse)


# conectando ao banco e pegando a tabela -----------------------------------------------------
cn <- DBI::dbConnect(RSQLite::SQLite(), "TemperaturaOleo_SmartSub.db")

tempOleo <- DBI::dbReadTable(cn, "cargaOleo") %>% as.tibble()

DBI::dbDisconnect(cn)


# tranformação dos dados --------------------------------------


basePredicao <- tempOleo %>%
  dplyr::mutate(Tempo = as.POSIXct(Tempo, format = "%d/%m/%y %H:%M"),
                lagCorrente = dplyr::lag(Corrente, 5),
                lagTamb = dplyr::lag(Tamb,5)) %>%
  group_by("hora" = lubridate::hour(Tempo)) %>%
  select(Toleo, Corrente, lagCorrente, Tamb, lagTamb, hora)



# fazendo o modelo ----------------------------------------------------------------

modelo <- RWeka::M5P(Toleo ~ ., data = basePredicao[6:(0.8*nrow(basePredicao)),])


# Prevendo ----------------------------------------------------------------

predicao <- predict(modelo, newdata = basePredicao[(0.8*nrow(basePredicao)):nrow(basePredicao),-1])



# comparação entre real e predito -----------------------------------------

compara <- tibble(real = basePredicao[(0.8*nrow(basePredicao)):nrow(basePredicao),1][[1]],
                  predito = predicao)
tabela <- table(basePredicao[(0.8*nrow(basePredicao)):nrow(basePredicao),1][[1]],
                predicao)


boxplot(compara)

compara %>%
  transmute(Erro = real - predito) %>%
  t() %>%
  as.vector() %>%
  hist()

compara %>%
  transmute(Erro = real - predito) %>%
  t() %>%
  as.vector() %>%
  mean()

compara %>%
  transmute(Erro = real - predito) %>%
  t() %>%
  as.vector() %>%
  median()
