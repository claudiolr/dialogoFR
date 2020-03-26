library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
require(usethis)





# Carregamento dos filtros ------------------------------------------------
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/FundRenova/DialogoSocial/Filtros/1332")
filtro1332_1 <- read.xlsx("filtro_1332_1.xlsx")
filtro1332_2 <- read.xlsx("filtro_1332_2.xlsx")
filtro1332_3 <- read.xlsx("filtro_1332_3.xlsx")
filtro1332_4 <- read.xlsx("filtro_1332_4.xlsx")
filtro1332_5 <- read.xlsx("filtro_1332_5.xlsx")
filtro1332_6 <- read.xlsx("filtro_1332_6.xlsx")

filtro1332 <- rbind(filtro1332_1, filtro1332_2, filtro1332_3,
                       filtro1332_4, filtro1332_5, filtro1332_6)
rm(filtro1332_1, filtro1332_2, filtro1332_3,
   filtro1332_4, filtro1332_5, filtro1332_6)



# Tratamento das variáveis ------------------------------------------------

### Transforma variáveis string em categóricas (as.factor)
filtro1332$localidadedemandadesc <- as.factor(filtro1332$localidadedemandadesc)
filtro1332$statusPostoAtendimento <- as.factor(filtro1332$statusPostoAtendimento)
filtro1332$statusManifestacao <- as.factor(filtro1332$statusManifestacao)

### Transforma variáveis string em datas (origem 30/12/1899 se deve ao fato de a variável original conter horas)
filtro1332$dataregistro <- as.Date(filtro1332$dataregistro, origin = "1899-12-30")
filtro1332$DataRegistroAnoMes <- format(filtro1332$dataregistro, "%Y/%m")
filtro1332$DataRegistroAnoMes <- as.factor(filtro1332$DataRegistroAnoMes)


filtro1332$dataConclusao <- as.Date(filtro1332$dataConclusao, origin = "1899-12-30")
filtro1332$dataConclusao[filtro1332$idManifestacao == 546873] <- "2018-11-08"
filtro1332$ndias[filtro1332$idManifestacao == 546873] <- 71


filtro1332$DataConclusaoAnoMes <- format(filtro1332$dataConclusao, "%Y/%m")
filtro1332$DataConclusaoAnoMes <- as.factor(filtro1332$DataConclusaoAnoMes)


filtro1332$dataLimiteConclusao <- as.Date(filtro1332$dataLimiteConclusao, origin = "1899-12-30")



### Quebra variável de assunto/tema em duas: Assunto e Tema
filtro1332 <- separate(filtro1332, manifestacaoAssuntoTema,
                          into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = "-",
                          extra = "drop", fill = "right",
                          remove = TRUE)


### Remove espaços extras das variáveis Assunto e Tema criadas
filtro1332$ManifestacaoAssunto <- str_squish(str_trim(filtro1332$ManifestacaoAssunto))
filtro1332$ManifestacaoTema <- str_squish(str_trim(filtro1332$ManifestacaoTema))


### Cria municipiosRecod a partir dos municípios do escopo e alguns outros importantes
filtro1332$municipioRecod <- ifelse(filtro1332$munici %in% c("Acaiaca",
                                                                "Aimorés",
                                                                "Alpercata",
                                                                "Aracruz",
                                                                "Baixo Guandu",
                                                                "Barra Longa",
                                                                "Belo Oriente",
                                                                "Bom Jesus do Galho",
                                                                "Bugre",
                                                                "Caratinga",
                                                                "Colatina",
                                                                "Conceição da Barra",
                                                                "Conselheiro Pena",
                                                                "Córrego Novo",
                                                                "Dionísio",
                                                                "Fernandes Tourinho",
                                                                "Fundão",
                                                                "Galileia",
                                                                "Governador Valadares",
                                                                "Iapu",
                                                                "Ipaba",
                                                                "Ipatinga",
                                                                "Itueta",
                                                                "Linhares",
                                                                "Mariana",
                                                                "Marilândia",
                                                                "Marliéria",
                                                                "Naque",
                                                                "Ouro Preto",
                                                                "Pancas",
                                                                "Periquito",
                                                                "Pingo D’Água",
                                                                "Ponte Nova",
                                                                "Raul Soares",
                                                                "Resplendor",
                                                                "Rio Casca",
                                                                "Rio Doce",
                                                                "Santa Cruz do Escalvado",
                                                                "Santana do Paraíso",
                                                                "São Domingos do Prata",
                                                                "São José do Goiabal",
                                                                "São Mateus",
                                                                "São Pedro dos Ferros",
                                                                "Sem-Peixe",
                                                                "Serra",
                                                                "Sobrália",
                                                                "Sooretama",
                                                                "Timóteo",
                                                                "Tumiritinga",
                                                                "Vitória"), filtro1332$municipio, "Demais municípios")

### Cria categoria com agrupamento do status das manifestações: finalizada e não finalizada
filtro1332$ManifestacaoFinalizada <- ifelse(filtro1332$statusManifestacao %in% c("Respondida",
                                                                                 "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                                 "Respondida no ato"), "Finalizada", "Não finalizada")
filtro1332$ManifestacaoFinalizada <- as.factor(filtro1332$ManifestacaoFinalizada)

