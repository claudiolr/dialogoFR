library(tidyverse)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
require(plotly, quietly = TRUE)


options(scipen = 999)

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
# [85] "ultimoEncaDEEnc"                     [86] "ultimoEncaPARAEnc"                     [87] "municipioRecod"


manifestacoes <- filtro327[c(2, 3, 6, 13, 14, 21, 22, 70, 87, 1, 58, 59, 9, 44, 45, 46, 49, 82, 34, 35, 37, 38, 39, 72, 60, 68, 81)]


manifestacoes %>% 
     filter(datareg >= ymd("2020-01-20") & datareg<= ymd("2020-03-31"),
            unidadeinseriu %in% c("CIA -  Aimorés",
                                  "CIA -  Baixo Guandu",
                                  "CIA -  Belo Oriente",
                                  "CIA - Aracruz",
                                  "CIA - Baguari",
                                  "CIA - Barra Longa",
                                  "CIA - Colatina",
                                  "CIA - Governador Valadares",
                                  "CIA - Linhares",
                                  "CIA - Maria Ortiz",
                                  "CIA - Mariana",
                                  "CIA - Mauá",
                                  "CIA - Naque",
                                  "CIA - Periquito",
                                  "CIA - Povoação",
                                  "CIA - Regência",
                                  "CIA - Resplendor",
                                  "CIA - Rio Doce e Sta Cruz do Escalvado",
                                  "CIA - São Mateus",
                                  "CIA - Tumiritinga")) %>% 
     mutate(datareg = format(datareg, "%b-%Y")) %>%
     # group_by(datareg) %>% 
     summarise(Total = n())


# Carrega dados da área de transferência (copie a tabela e execute o código abaixo)
atendentes <- read.table(file = "clipboard", header = TRUE, sep = "\t")
nomes <- atendentes$NOME.SGS


manifestacoes %>% 
     filter(ultimoencaData >= ymd("2020-01-20") & ultimoencaData <= ymd("2020-03-31"),
            ultimoEncaDE %in% nomes) %>% 
     mutate(ultimoencaData = format(ultimoencaData, "%b-%Y")) %>%
     # group_by(ultimoencaTipo) %>% 
     summarise(Total = n())




manifestacoes %>% 
     filter(ultimoencaData >= ymd("2020-01-20") & ultimoencaData <= ymd("2020-03-31"),
            ultimoEncaDE %in% nomes) %>% 
     mutate(ultimoencaData = format(ultimoencaData, "%b-%Y")) %>%
     # group_by(responsavelconclusao) %>% 
     summarise(Total = n())


manifestacoes %>% 
     filter(ultimoencaData >= ymd("2020-01-20") & ultimoencaData <= ymd("2020-03-31"),
            responsavelconclusao %in% nomes,
            statusManifestacao == "Respondida") %>% 
     mutate(ultimoencaData = format(ultimoencaData, "%b-%Y")) %>%
     # group_by(responsavelconclusao) %>% 
     summarise(Total = n())


manifestacoes %>% 
     filter(ultimoencaData >= ymd("2020-01-20") & ultimoencaData <= ymd("2020-03-31"),
            responsavelconclusao %in% nomes) %>% 
     mutate(ultimoencaData = format(ultimoencaData, "%b-%Y")) %>%
     group_by(statusdemanda) %>%
     summarise(Total = n())

