library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)



########## Filtro237.R
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
# [34] "UltimoEncaForma"                     [35] "ultimoencaTipo"                        [36] "idManifestacaoNatureza"
# [37] "manifestacaoNatureza"                [38] "idManifestacaoTema"                    [39] "idManifestacaoAssunto"
# [40] "manifestacaoAssuntoTema"             [41] "ManifestacaoAssunto"                   [42] "ManifestacaoTema"
# [43] "manifestacaoAssunto"                 [44] "idManifestacaoSubtema"                 [45] "manifestacaoSubtema"
# [46] "manifestacaoCriticidade"             [47] "abrangencia"                           [48] "idAcao"
# [49] "analistaResponsavel_codPessoa"       [50] "analistaResponsavel"                   [51] "resumo"
# [52] "endereco"                            [53] "dataLimiteConclusao"                   [54] "dataconclusao"
# [55] "ndias"                               [56] "responsavelconclusao"                  [57] "Unidade_conclusao"
# [58] "operadoralteracaofinal"              [59] "nomeoperadoralteracaofinal"            [60] "resumoconclusao"
# [61] "avaliacaoTratamento"                 [62] "avaliacaoTratamentoMotivo"             [63] "localidadedemanda"
# [64] "localidadedemandadesc"               [65] "localidadedemanda_UF"                  [66] "territorio"
# [67] "MODULO_ATINGIDO"                     [68] "municipio_origem"                      [69] "comunidade_origem"
# [70] "formapreferidaretorno"               [71] "respostajuridico"                      [72] "DATAHORA_EXP"
# [73] "latitude"                            [74] "longitude"                             [75] "numero_contato_manifestante"
# [76] "requerAcaoFutura"                    [77] "responsavelPelaDemanda"                [78] "StatusDemanda"
# [79] "ultimoEncaminhamento"                [80] "ultimoEncaData"                        [81] "ultimoEncaDE"
# [82] "ultimoEncaPARA"                      [83] "municipioRecod"                   

cenario <- filtro327[c(2, 3, 6, 21, 22, 83, 1, 54, 55, 9, 40, 41, 42, 45, 76, 78, 68, 64)]


# Manifestações
# 1.Total de manifestações registradas de nov/15 até 31/03
# 2. Total de manifestações registradas em março/20
cenario %>% 
     mutate(MesAtual = ifelse(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31"), 1, 0)) %>% 
     summarise(Total = n(),
               MesAtual = sum(MesAtual))
     


# 3. Status de todas as manifestações (nov/15 até 31/03) – status simplificado (respondidas x não respondidas)
cenario %>% 
     mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     group_by(statusManifestacao) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# 4. Quantidade e % de manifestações por assunto/tema (PG + detalhamento) no último mês (março/20)
cenario %>% 
     mutate(AssuntoTema = str_replace_all(manifestacaoAssuntoTema, c("PG001 Levantamento e Cadastro" = "PG01",
                                                                     "PG002 Ressarcimento e Indenização" = "PG02",
                                                                     "PG003 Proteção e Recuperação da Qualidade de Vida dos Povos Indígenas" = "PG03",
                                                                     "PG004 Proteção e Qualidade de Vida de Outras Comunidades Tradicionais" = "PG04",
                                                                     "PG005 Proteção Social" = "PG05",
                                                                     "PG006 Diálogo, Comunicação e Participação Social" = "PG06",
                                                                     "PG007 Assistência aos Animais" = "PG07",
                                                                     "PG008 Reconstrução de Vilas" = "PG08",
                                                                     "PG009 Recuperação da UHE Risoleta Neves" = "PG09",
                                                                     "PG010 Recuperação das Demais Comunidades e Infraestruturas Impactadas" = "PG10",
                                                                     "PG011 Reintegração da Comunidade Escolar" = "PG11",
                                                                     "PG012 Memória Histórica, Cultural e Artística" = "PG12",
                                                                     "PG013 Turismo, Cultura, Esporte e Lazer" = "PG13",
                                                                     "PG014 Saúde Física e Mental da População Impactada" = "PG14",
                                                                     "PG015 Tecnologias Socioeconômicas" = "PG15",
                                                                     "PG016 Retomada das Atividades Aquícolas e Pesqueiras" = "PG16",
                                                                     "PG017 Retomada das Atividades Agropecuárias" = "PG17",
                                                                     "PG018 Diversificação Econômica Regional" = "PG18",
                                                                     "PG019 Micro e Pequenos Negócios" = "PG19",
                                                                     "PG020 Estímulo à Contratação Local" = "PG20",
                                                                     "PG021 Auxílio Financeiro Emergencial" = "PG21",
                                                                     "PG023 Manejo dos Rejeitos" = "PG23",
                                                                     "PG024 Contenção de Rejeitos e Tratamento de Rios" = "PG24",
                                                                     "PG025 Recuperação da Área Ambiental 1" = "PG25",
                                                                     "PG026 Recuperação de APPs" = "PG26",
                                                                     "PG027 Recuperação de Nascentes" = "PG27",
                                                                     "PG028 Conservação da Biodiversidade" = "PG28",
                                                                     "PG029 Recuperação da Fauna Silvestre" = "PG29",
                                                                     "PG030 Fauna e Flora Terrestre Ameaçadas de Extinção" = "PG30",
                                                                     "PG031 Coleta e Tratamento de Esgoto e Destinação de Resíduos Sólidos" = "PG31",
                                                                     "PG032 Tratamento de Água e Captação Alternativa" = "PG32",
                                                                     "PG033 Educação Ambiental" = "PG33",
                                                                     "PG034 Preparação para Emergência Ambiental" = "PG34",
                                                                     "PG037 Gestão de Riscos Ambientais" = "PG37",
                                                                     "PG038 Monitoramento da Bacia do Rio Doce" = "PG38",
                                                                     "PG039 Unidades de Conservação" = "PG39",
                                                                     "PG040 CAR e PRAs" = "PG40",
                                                                     "PG042 Ressarcimento de Gastos Públicos Extraordinários" = "PG42"))) %>% 
     mutate(AssuntoTema = str_replace_all(AssuntoTema, " - ", " | ")) %>% 
     filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
     group_by(AssuntoTema) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     arrange(desc(Percentual))



# Demandas Individuais
# 1. Total de demandas individuais existentes (nov/15 até 31/03)
cenario %>% 
     filter(!is.na(StatusDemanda)) %>% 
     summarise(Total = n())


# 2. Status de todas as demandas individuais (nov/15 até 31/03)
cenario %>% 
     filter(!is.na(StatusDemanda)) %>% 
     group_by(StatusDemanda) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# 3. Quantidade de demandas individuais por assunto (PG) e status (nov/15 até 31/03)
cenario %>% 
     filter(!is.na(StatusDemanda)) %>% 
     group_by(StatusDemanda, ManifestacaoAssunto) %>% 
     summarise(Total = n()) %>% 
     # mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     pivot_wider(names_from = StatusDemanda, values_from = c(Total))







knitr::kable(cenario %>% 
                  mutate(MesAtual = ifelse(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31"), 1, 0)) %>% 
                  summarise(Total = n(),
                            MesAtual = sum(MesAtual)), caption = "Total de manifestações registradas e manifestações do último mês")