library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Carregamento dos filtros ------------------------------------------------
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/FundRenova/DialogoSocial/Filtros/129")
filtro129 <- read.xlsx("filtro_129.xlsx")



# Tratamento das variáveis ------------------------------------------------
### Transforma variáveis string em categóricas (as.factor)
filtro129$localidadedemandadesc <- as.factor(filtro129$localidadedemandadesc)
filtro129$statusManifestacao <- as.factor(filtro129$statusManifestacao)


### Transforma variáveis string em datas (origem 30/12/1899 se deve ao fato de a variável original conter horas)
filtro129$dataregistro <- as.Date(filtro129$dataregistro, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
filtro129$DataRegistroAnoMes <- format(filtro129$dataregistro, "%Y/%m")
filtro129$DataRegistroAnoMes <- as.factor(filtro129$DataRegistroAnoMes)


### Quebra variável de assunto/tema em duas: Assunto e Tema
filtro129 <- separate(filtro129, manifestacaoAssuntoTema,
                  into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = "-",
                  extra = "drop", fill = "right",
                  remove = TRUE)


### Remove espaços extras das variáveis Assunto e Tema criadas
filtro129$ManifestacaoAssunto <- str_squish(str_trim(filtro129$ManifestacaoAssunto))
filtro129$ManifestacaoTema <- str_squish(str_trim(filtro129$ManifestacaoTema))


### Cria categoria com agrupamento do status das manifestações: finalizada e não finalizada
filtro129$ManifestacaoFinalizada <- ifelse(filtro129$statusManifestacao %in% c("Respondida",
                                                                                       "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                                       "Respondida no ato"), "Finalizada", "Não finalizada")
filtro129$ManifestacaoFinalizada <- as.factor(filtro129$ManifestacaoFinalizada)


### Cria municipiosRecod a partir dos municípios do escopo e alguns outros importantes
filtro129$municipioRecod <- ifelse(filtro129$municipio %in% c("Acaiaca",
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
                                                      "Vitória"), filtro129$municipio, "Demais municípios")