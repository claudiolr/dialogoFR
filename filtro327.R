library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Carregamento dos filtros ------------------------------------------------
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/OrganizacaoInformacao/FiltrosSGS/327")
filtro327 <- read.xlsx("filtro_327_20200329201522.xlsx")



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
                      remove = FALSE)


### Remove espaços extras das variáveis Assunto e Tema criadas
filtro327$ManifestacaoAssunto <- str_squish(str_trim(filtro327$ManifestacaoAssunto))
filtro327$ManifestacaoTema <- str_squish(str_trim(filtro327$ManifestacaoTema))


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


########## Filtro327.R
# [1] "datareg"                              [2] "idManifestacao"                         [3] "protocolo"
# [4] "statusPostoAtendimento"               [5] "idStatusManifestacao"                   [6] "statusManifestacao"
# [7] "datainsercao"                         [8] "horainsercao"                           [9] "FormaRecebimento"
# [10] "recebimentoManifestacao_codPessoa"   [11] "pessoarecebimentomanif"                [12] "operadorinseriu"
# [13] "nomeoperadorinseriu"                 [14] "unidadeinseriu"                        [15] "manifestante_codPessoa"
# [16] "manifestante"                        [17] "manifestanteapelido"                   [18] "idFamilia"
# [19] "CategoriaAcidenteInformada"          [20] "constamodatingidos"                    [21] "uf"
# [22] "municipio"                           [23] "comunidade"                            [24] "localidademanifestante"
# [25] "localTrabalho"                       [26] "areatrabalho"                          [27] "cargo"
# [28] "CATEGORIA"                           [29] "telefones"                             [30] "email"
# [31] "prazo"                               [32] "prazoajust"                            [33] "statusPrazo"
# [34] "ultimoEncaDE"                        [35] "ultimoEncaPARA"                        [36] "UltimoEncaForma"
# [37] "ultimoencaData"                      [38] "ultimoencaTipo"                        [39] "ultimoEncaminhamento"
# [40] "idManifestacaoNatureza"              [41] "manifestacaoNatureza"                  [42] "idManifestacaoTema"
# [43] "idManifestacaoAssunto"               [44] "manifestacaoAssuntoTema"               [45] "ManifestacaoAssunto"
# [46] "ManifestacaoTema"                    [47] "manifestacaoAssunto"                   [48] "idManifestacaoSubtema"
# [49] "manifestacaoSubtema"                 [50] "manifestacaoCriticidade"               [51] "abrangencia"
# [52] "idAcao"                              [53] "analistaResponsavel_codPessoa"         [54] "analistaResponsavel"
# [55] "resumo"                              [56] "endereco"                              [57] "dataLimiteConclusao"
# [58] "dataconclusao"                       [59] "ndias"                                 [60] "responsavelconclusao"
# [61] "Unidade_conclusao"                   [62] "operadoralteracaofinal"                [63] "nomeoperadoralteracaofinal"
# [64] "resumoconclusao"                     [65] "avaliacaoTratamento"                   [66] "avaliacaoTratamentoMotivo"
# [67] "localidadedemanda"                   [68] "localidadedemandadesc"                 [69] "localidadedemanda_UF"
# [70] "territorio"                          [71] "MODULO_ATINGIDO"                       [72] "municipio_origem"
# [73] "comunidade_origem"                   [74] "formapreferidaretorno"                 [75] "respostajuridico"
# [76] "DATAHORA_EXP"                        [77] "latitude"                              [78] "longitude"
# [79] "numero_contato_manifestante"         [80] "requerAcaoFutura"                      [81] "responsavelPelaDemanda"
# [82] "statusdemanda"                       [83] "ultimoEncaminhamentoEnc"               [84] "ultimoEncaDataEnc"
# [85] "ultimoEncaDEEnc"                     [86] "ultimoEncaPARAEnc"                     [87] "ultimoEncaData"                   
# [88] "municipioRecod"        