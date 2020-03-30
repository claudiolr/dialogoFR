library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Carregamento dos filtros ------------------------------------------------
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/DialogoSocial/Filtros/327")
filtro327 <- read.xlsx("filtro_327_20200323134054.xlsx")



# Tratamento das variáveis ------------------------------------------------
### Variáveis de data
filtro327$datareg <- as.Date(filtro327$datareg, origin = "1899-12-30")
filtro327$datainsercao <- as.Date(filtro327$datainsercao, origin = "1899-12-30")
filtro327$prazo <- as.Date(filtro327$prazo, origin = "1899-12-30")
filtro327$prazoajust <- as.Date(filtro327$prazoajust, origin = "1899-12-30")
filtro327$dataLimiteConclusao <- as.Date(filtro327$dataLimiteConclusao, origin = "1899-12-30")
filtro327$dataconclusao <- as.Date(filtro327$dataconclusao, origin = "1899-12-30")
# ndias = dataconclusao - datereg
filtro327$ndias <- ifelse(is.na(filtro327$dataconclusao), ymd("2020-03-31") - ymd(filtro327$datareg),
                          ymd(filtro327$dataconclusao) - ymd(filtro327$datareg))
filtro327$ultimoEncaData <- as.Date(filtro327$ultimoEncaData, origin = "1899-12-30")


### Quebra variável de assunto/tema em duas: Assunto e Tema
filtro327 <- separate(filtro327, manifestacaoAssuntoTema,
                      into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = "-",
                      extra = "drop", fill = "right",
                      remove = TRUE)


### Remove espaços extras das variáveis Assunto e Tema criadas
filtro327$ManifestacaoAssunto <- str_squish(str_trim(filtro327$ManifestacaoAssunto))
filtro327$ManifestacaoTema <- str_squish(str_trim(filtro327$ManifestacaoTema))


### Cria categoria com agrupamento do status das manifestações: finalizada e não finalizada
filtro327$ManifestacaoFinalizada <- ifelse(filtro327$statusManifestacao %in% c("Respondida",
                                                                               "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                               "Respondida no ato"), "Finalizada", "Não finalizada")
filtro327$ManifestacaoFinalizada <- as.factor(filtro327$ManifestacaoFinalizada)


### Cria municipiosRecod a partir dos municípios do escopo e alguns outros importantes
filtro327$municipioRecod <- ifelse(filtro327$municipio %in% c("Acaiaca",
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
                                                              "Vitória"), filtro327$municipio, "Demais municípios")

