library(tidyverse)
library(openxlsx)
library(lubridate)
library(knitr)

setwd("~/GitHub/dialogoFR")
# source("filtro327.R", encoding = "UTF-8")

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


cenario <- filtro327[c(2, 3, 6, 21, 22, 70, 87, 1, 58, 59, 9, 44, 45, 46, 49, 82, 37, 72, 68, 80)]
cenario <- cenario %>% separate(ManifestacaoAssunto, sep = " ", into = "Assunto")


geraDados <- function(conjuntos){
        conjuntos <- list(Tema1 = c("PG001"),
                          Tema2 = c("PG002", "PG021"),
                          Tema3 = c("PG017", "PG025", "PG026", "PG027", "PG040"),
                          Tema4 = c("PG016"),
                          Tema5 = c("PG015", "PG018", "PG019", "PG020", "PG042"),
                          Tema6 = c(paste0("PG0",c(23:24, 28:31, 33:41))),
                          Tema7 = c("PG032"),
                          Tema8 = c("PG008", "PG010"),
                          Tema9 = c("PG005", "PG014"),
                          Tema10 = c("PG003", "PG004"),
                          Tema11 = c("PG013"))
        
        for(i in conjuntos){
                PG = i
                MesAtual <- seq.Date(from = ymd("2020-03-01"), to = ymd("2020-03-31"), by = "day")
                Semestre <- seq.Date(from = ymd("2019-10-01"), to = ymd("2020-03-31"), by = "day")
                
                
                tabela1 <- cenario %>% 
                        mutate(MesAtual = ifelse(datareg %in% MesAtual, 1, 0),
                               statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                                     "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                                     "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
                        filter(Assunto %in% PG) %>% 
                        group_by(statusManifestacao) %>% 
                        summarise(N = n()) %>% 
                        mutate(Percentual = round(N/sum(N), digits = 3)*100)
                
                
                tabela2 <- cenario %>% 
                        mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                          "T02 Alto Rio Doce" = "T02. ARD",
                                                                          "T03 Calha do Rio Doce" = "T03. CRD",
                                                                          "T04 Médio Rio Doce" = "T04. MRD",
                                                                          "T05 Baixo Rio Doce" = "T05. BRD",
                                                                          "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
                        filter(Assunto %in% PG,
                               territorio != "Não informado",
                               datareg %in% MesAtual) %>% 
                        group_by(territorio) %>% 
                        summarise(N = n()) %>% 
                        mutate(Percentual = round(N/sum(N), digits = 3)*100)
                
                
                tabela3 <- cenario %>% 
                        filter(datareg %in% Semestre,
                               Assunto %in% PG) %>% 
                        mutate(datareg = format(datareg, "%Y-%m")) %>% 
                        group_by(datareg) %>% 
                        summarise(N = n())
                
                
                tabela4 <- cenario %>% 
                        filter(datareg %in% MesAtual,
                               Assunto %in% PG) %>% 
                        group_by(ManifestacaoTema) %>% 
                        summarise(N = n()) %>% 
                        mutate(Percentual = round(N/sum(N), digits = 3)*100) %>% 
                        arrange(desc(Percentual))
                
                
                tabela5 <- cenario %>%
                        mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                          "T02 Alto Rio Doce" = "T02. ARD",
                                                                          "T03 Calha do Rio Doce" = "T03. CRD",
                                                                          "T04 Médio Rio Doce" = "T04. MRD",
                                                                          "T05 Baixo Rio Doce" = "T05. BRD",
                                                                          "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
                        filter(Assunto %in% PG,
                               territorio != "Não informado") %>% 
                        group_by(territorio) %>% 
                        summarise(N = n()) %>% 
                        mutate(Percentual = round(N/sum(N), digits = 3)*100)
                
                
                tabela6 <- cenario %>% 
                        mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                                     "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                                     "Respondida no ato"), "Respondida", "Não Respondida")) %>% 
                        filter(Assunto %in% PG) %>% 
                        group_by(ManifestacaoTema, statusManifestacao) %>% 
                        summarise(Total = n()) %>%
                        arrange(desc(Total)) %>%
                        pivot_wider(names_from = statusManifestacao, values_from = Total, values_fill = list(Total = 0))
                
                
                tabela7 <- cenario %>% 
                        mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                                     "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                           "Finalizada",
                                                           ifelse(statusManifestacao == "Respondida no ato", "Finalizada no ato", "Não finalizada"))) %>% 
                        filter(Assunto %in% PG) %>% 
                        group_by(statusManifestacao) %>% 
                        summarise(Total = n()) %>% 
                        mutate(Percentual = round(prop.table(Total), digits = 3)*100)
                
                
                tabela8 <- cenario %>% 
                        filter(Assunto %in% PG,
                               requerAcaoFutura == 1) %>% 
                        mutate(statusdemanda = ifelse(statusdemanda == "A iniciar", "Não respondida", "Respondida")) %>% 
                        group_by(statusdemanda) %>% 
                        summarise(Total = n()) %>% 
                        mutate(Percentual = round(prop.table(Total), digits = 3)*100)
                
                
                tabela9 <- cenario %>% 
                        mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                          "T02 Alto Rio Doce" = "T02. ARD",
                                                                          "T03 Calha do Rio Doce" = "T03. CRD",
                                                                          "T04 Médio Rio Doce" = "T04. MRD",
                                                                          "T05 Baixo Rio Doce" = "T05. BRD",
                                                                          "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
                        filter(Assunto %in% PG,
                               requerAcaoFutura == 1,
                               territorio != "Não informado") %>% 
                        group_by(territorio) %>% 
                        summarise(Total = n()) %>% 
                        mutate(Percentual = round(prop.table(Total), digits = 3)*100)
                
                
                tabela10 <- cenario %>% 
                        mutate(statusdemanda = str_replace_all(statusdemanda, c("Em andamento para retorno intermediário" = "Em andamento",
                                                                                "Em andamento para retorno final" = "Em andamento"))) %>% 
                        filter(Assunto %in% PG,
                               requerAcaoFutura == 1) %>% 
                        group_by(statusdemanda) %>% 
                        summarise(Total = n()) %>% 
                        mutate(Percentual = round(prop.table(Total), digits = 3)*100)
                
                
                tabela11 <- cenario %>% 
                        filter(Assunto %in% PG,
                               requerAcaoFutura == 1) %>% 
                        mutate(statusdemanda = ifelse(statusdemanda == "A iniciar", "Não respondida", "Respondida")) %>% 
                        group_by(ManifestacaoTema, statusdemanda) %>% 
                        summarise(Total = n()) %>% 
                        arrange(desc(Total)) %>% 
                        pivot_wider(names_from = statusdemanda, values_from = Total, values_fill = list(Total = 0))
                
                
                setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/AnaliseCenario/Dados")
                
                write.xlsx(list(infos = c("Programas analisados",PG),
                                "Manif(geral)" = tabela1,
                                "Manif(mês)_territ" = tabela2,
                                "Manif(semestre)" = tabela3,
                                "Manif(tema)" = tabela4,
                                "Manif(geral)_territ" = tabela5,
                                "Manif_statusSubtema" = tabela6,
                                "Manif_statusGeral" = tabela7,
                                "DemandasIndiv(geral)" = tabela8,
                                "DemandasTerrit" = tabela9,
                                "DemandasStatus" = tabela10,
                                "AssuntosStatus" = tabela11),
                           paste0("AnaliseCenario_",PG[1],".xlsx"),
                           colWidths = c(NA, "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto"))
        }
}

geraDados(conjuntos)


