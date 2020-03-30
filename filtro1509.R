library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)



# Carrega o filtro 1509 ---------------------------------------------------
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/FundRenova/DialogoSocial/Filtros/1509")

colunas <- c(1:3, 5, 17:18, 27, 30, 42, 44, 50, 52)

filtro1 <- read.xlsx("filtro_1509_20200318111524.xlsx", cols = colunas)
filtro2 <- read.xlsx("filtro_1509_20200318113027.xlsx", cols = colunas)
filtro3 <- read.xlsx("filtro_1509_20200318113104.xlsx", cols = colunas)
filtro4 <- read.xlsx("filtro_1509_20200318113123.xlsx", cols = colunas)
filtro5 <- read.xlsx("filtro_1509_20200318113138.xlsx", cols = colunas)
filtro6 <- read.xlsx("filtro_1509_20200318113204.xlsx", cols = colunas)
filtro7 <- read.xlsx("filtro_1509_20200318113222.xlsx", cols = colunas)
filtro8 <- read.xlsx("filtro_1509_20200318113239.xlsx", cols = colunas)
filtro9 <- read.xlsx("filtro_1509_20200318113253.xlsx", cols = colunas)
filtro10 <- read.xlsx("filtro_1509_20200318113338.xlsx", cols = colunas)
filtro11 <- read.xlsx("filtro_1509_20200318113603.xlsx", cols = colunas)


resposta <- rbind(filtro1, filtro2, filtro3, filtro4, filtro5, filtro6, filtro7, filtro8, filtro9, filtro10, filtro11)
rm(filtro1, filtro2, filtro3, filtro4, filtro5, filtro6, filtro7, filtro8, filtro9, filtro10, filtro11)

resposta <- resposta[c("idManifestacao",
                       "protocolo",
                       "datareg",
                       "data_en",
                       "dataconclusao",
                       "statusManifestacao",
                       "uf",
                       "municipio",
                       "manifestacaoNatureza",
                       "manifestacaoAssuntoTema",
                       "localidadedemandadesc",
                       "manifencaminhamentotipo_en")]


backup <- resposta
resposta <- backup

###########################################################################
# Tratamento das variáveis ------------------------------------------------

### Variáveis de data
resposta$datareg <- as.Date(resposta$datareg, origin = "1899-12-30")
resposta$data_en <- as.Date(resposta$data_en, origin = "1899-12-30")
resposta$dataconclusao <- as.Date(resposta$dataconclusao, origin = "1899-12-30")


### Cálculo de dias
resposta <- add_column(resposta, "dias" = as.numeric(""), .after = "dataconclusao")
resposta$dias <- ifelse(!is.na(resposta$dataconclusao), ymd(resposta$dataconclusao) - ymd(resposta$datareg),
                        ifelse(is.na(resposta$data_en), ymd("2020-02-29") - ymd(resposta$datareg),
                               ymd(resposta$data_en) - ymd(resposta$datareg)))

resposta <- add_column(resposta, "diasEnc" = as.numeric(""), .after = "dias")
resposta$diasEnc <- ifelse(!is.na(resposta$data_en), ymd(resposta$data_en) - ymd(resposta$datareg),
                           ifelse(is.na(resposta$dataconclusao), ymd("2020-02-29") - ymd(resposta$datareg),
                                  ymd(resposta$dataconclusao) - ymd(resposta$datareg)))


### Quebra variável de assunto/tema em duas: Assunto e Tema
resposta <- separate(resposta, manifestacaoAssuntoTema,
                     into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = "-",
                     extra = "drop", fill = "right",
                     remove = TRUE)


### Remove espaços extras das variáveis Assunto e Tema criadas
resposta$ManifestacaoAssunto <- str_squish(str_trim(resposta$ManifestacaoAssunto))
resposta$ManifestacaoTema <- str_squish(str_trim(resposta$ManifestacaoTema))

