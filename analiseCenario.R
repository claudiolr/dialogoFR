library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
library(knitr)



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

cenario <- filtro327[c(2, 3, 6, 21, 22, 70, 88, 1, 58, 59, 9, 44, 45, 46, 49, 82, 37, 72, 68)]


### Manifestações
## 1.Total de manifestações registradas de nov/15 até 31/03
# Geral
M1Geral <- cenario %>% 
     mutate(MesAtual = ifelse(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31"), 1, 0)) %>% 
     summarise(Total = n(),
               MesAtual = sum(MesAtual))


# Território
M1Territorio <- cenario %>% 
     group_by(territorio) %>% 
     summarise(Total = n())



## 2. Total de manifestações registradas nos três últimos meses
#Geral
M2Geral <- cenario %>% 
     filter(datareg >= ymd("2020-01-01") & datareg <= ymd("2020-03-31")) %>% 
     mutate(datareg = format(datareg, "%b-%Y")) %>% 
     group_by(datareg) %>% 
     summarise(Total = n())


# Território (apenas o último mês)
M2Territorio <- cenario %>% 
     filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
     mutate(datareg = format(datareg, "%b-%Y")) %>% 
     group_by(datareg, territorio) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = datareg, values_from = Total)



## 3. Status de todas as manifestações (nov/15 até 31/03) – status simplificado (respondidas x não respondidas)
# Geral
M3Geral <- cenario %>% 
     mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     group_by(statusManifestacao) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# Território
M3Territorio <- cenario %>% 
     mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Respondida", "Não respondida")) %>% 
     group_by(territorio, statusManifestacao, ManifestacaoAssunto) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     pivot_wider(names_from = statusManifestacao, values_from = c(Total, Percentual))



## 4. Quantidade e % de manifestações por assunto (PG) no último mês (março/20)
# Geral
M4Geral <- cenario %>% 
     mutate(Assunto = str_replace_all(ManifestacaoAssunto, c("PG001 Levantamento e Cadastro" = "PG01",
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
     filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
     group_by(Assunto) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     arrange(desc(Percentual))



## 5. Quantidade e % de manifestações por assunto/tema (PG + detalhamento) no último mês (março/20)
# Geral
M5Geral <- cenario %>% 
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
     filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
     group_by(AssuntoTema) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     arrange(desc(Percentual))


# Território
M5Territorio <- cenario %>% 
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
     filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
     group_by(AssuntoTema, territorio) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     arrange(desc(Percentual))




### Demandas Individuais
## 1. Total de demandas individuais existentes (nov/15 até 31/03)
# Geral
D1Geral <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     summarise(Total = n())


# Território
D1Territorio <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     group_by(territorio) %>% 
     summarise(Total = n())



## 2. Status de todas as demandas individuais (nov/15 até 31/03)
# Geral
D2Geral <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     group_by(statusdemanda) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# Território
D2Territorio <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     group_by(statusdemanda, territorio) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = territorio, values_from = Total, values_fill = list(Total = 0))




## 3. Quantidade de demandas individuais por assunto (PG) e status (nov/15 até 31/03)
# Geral
D3Geral <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     group_by(statusdemanda, ManifestacaoAssunto) %>% 
     summarise(Total = n()) %>% 
     # mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
     pivot_wider(names_from = statusdemanda, values_from = c(Total), values_fill = list(Total = 0))


# Território
D3Territorio <- cenario %>% 
     filter(!is.na(statusdemanda)) %>% 
     group_by(territorio, statusdemanda, ManifestacaoAssunto) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = statusdemanda, values_from = c(Total), values_fill = list(Total = 0))





setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/AnaliseCenario/Dados")

write.xlsx(list(M1Geral = M1Geral,
                M1Territorio = M1Territorio,
                M2Geral = M2Geral,
                M2Territorio = M2Territorio,
                M3Geral = M3Geral,
                M3Territorio = M3Territorio,
                M4Geral = M4Geral,
                M5Geral = M5Geral,
                M5Territorio = M5Territorio,
                D1Geral = D1Geral,
                D1Territorio = D1Territorio,
                D2Geral = D2Geral,
                D2Territorio = D2Territorio,
                D3Geral = D3Geral,
                D3Territorio = D3Territorio), "AnaliseCenario.xlsx")

