library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)



# Escolha das colunas a partir do filtro utilizado ------------------------

########## Filtro129.R
# [1] "dataregistro"                         [2] "idManifestacao"                    [3] "protocolo"                        
# [4] "statusPostoAtendimento"               [5] "idStatusManifestacao"              [6] "statusManifestacao"
# [7] "datainsercao"                         [8] "Unidade"                           [9] "FormaRecebimento"
# [10] "recebimentoManifestacao_codPessoa"   [11] "operadorinseriu"                  [12] "nomeoperadorinseriu"
# [13] "localidadedemandadesc"               [14] "manifestante_codPessoa"           [15] "manifestante"
# [16] "cpf_manifestante"                    [17] "CategoriaAcidenteInformada"       [18] "constamodatingidos"
# [19] "uf"                                  [20] "municipio"                        [21] "comunidade"
# [22] "localidademanifestante"              [23] "localTrabalho"                    [24] "areatrabalho"                     
# [25] "cargo"                               [26] "CATEGORIA"                        [27] "telefones"                        
# [28] "email"                               [29] "prazo"                            [30] "idManifestacaoNatureza"           
# [31] "manifestacaoNatureza"                [32] "idManifestacaoTema"               [33] "idManifestacaoAssunto"            
# [34] "ManifestacaoAssunto"                 [35] "ManifestacaoTema"                 [36] "manifestacaoAssunto"              
# [37] "manifestacaoCriticidade"             [38] "abrangencia"                      [39] "idAcao"                           
# [40] "analistaResponsavel_codPessoa"       [41] "analistaResponsavel"              [42] "resumo"                           
# [43] "endereco"                            [44] "formapreferidaretorno"            [45] "respostajuridico"                 
# [46] "DATAHORA_EXP"                        [47] "requeracaofutura"                 [48] "StatusDemanda"                    
# [49] "ultimoEncaminhamento"                [50] "ultimoEncaData"                   [51] "ultimoEncaDE"                     
# [52] "ultimoEncaPARA"                      [53] "manifestacaoSubtema"              [54] "DataRegistroAnoMes"               
# [55] "ManifestacaoFinalizada"              [56] "municipioRecod"                   

cadastro <- filtro129[c(2, 6, 55, 1, 54, 8, 9, 34, 35, 53, 14, 13, 19, 20, 56)]


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
# [40] "ManifestacaoAssunto"                 [41] "ManifestacaoTema"                      [42] "manifestacaoAssunto"              
# [43] "idManifestacaoSubtema"               [44] "manifestacaoSubtema"                   [45] "manifestacaoCriticidade"
# [46] "abrangencia"                         [47] "idAcao"                                [48] "analistaResponsavel_codPessoa"
# [49] "analistaResponsavel"                 [50] "resumo"                                [51] "endereco"
# [52] "dataLimiteConclusao"                 [53] "dataconclusao"                         [54] "ndias"
# [55] "responsavelconclusao"                [56] "Unidade_conclusao"                     [57] "operadoralteracaofinal"
# [58] "nomeoperadoralteracaofinal"          [59] "resumoconclusao"                       [60] "avaliacaoTratamento"
# [61] "avaliacaoTratamentoMotivo"           [62] "localidadedemanda"                     [63] "localidadedemandadesc"
# [64] "localidadedemanda_UF"                [65] "territorio"                            [66] "MODULO_ATINGIDO"
# [67] "municipio_origem"                    [68] "comunidade_origem"                     [69] "formapreferidaretorno"
# [70] "respostajuridico"                    [71] "DATAHORA_EXP"                          [72] "latitude"
# [73] "longitude"                           [74] "numero_contato_manifestante"           [75] "requerAcaoFutura"
# [76] "responsavelPelaDemanda"              [77] "StatusDemanda"                         [78] "ultimoEncaminhamento"
# [79] "ultimoEncaData"                      [80] "ultimoEncaDE"                          [81] "ultimoEncaPARA"
# [82] "ManifestacaoFinalizada"              [83] "municipioRecod"

cadastro <- filtro327[c(2, 6, 82, 1, 14, 9, 40, 41, 44, 15, 63, 21, 22, 83)]


### Filtra por: solicitação de cadastro e data
cadastro <- cadastro %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                                  datareg >= ymd("2018-02-01") & 
                                  datareg <= ymd("2020-03-31") & 
                                  ManifestacaoTema == "Solicitação de cadastro")


### Gráficos

# 

cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = datareg, fill = ManifestacaoFinalizada)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = 1,
               color = "black", size = 4) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 3200, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())



# 
cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = datareg)) +
     geom_bar(stat = "count", position = 'dodge', na.rm = TRUE, fill = "#666564") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1.0),
               vjust = -0.2,
               color = "black", size = 6) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 3200, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ ManifestacaoFinalizada, nrow = 5, ncol = 1)



# 
cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = datareg, fill = statusManifestacao)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = 1,
               color = "white", size = 6) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 3200, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())


#
cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = datareg, fill = statusManifestacao)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 3200, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ ManifestacaoFinalizada, nrow = 5, ncol = 1)



# 
cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = fct_infreq(municipioRecod))) +
     geom_bar(stat = "count", position = "dodge", fill = "#2C3E50") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1),
               vjust = -0.5,
               size = 3.5) +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 6000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5, hjust = 1),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())


# 
cadastro %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     ggplot(aes(x = fct_infreq(municipioRecod), fill = statusManifestacao)) +
     geom_bar(position = "stack") +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 5000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.text = element_text(size = 10),
           legend.position = "bottom", panel.background = element_blank())


# 
cadastro %>% 
     ggplot(aes(x = fct_infreq(municipioRecod))) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = -0.2,
               color = "black", size = 5) +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 5000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ ManifestacaoFinalizada, nrow = 5, ncol = 1)



# 
filtro327 %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                          datareg >= ymd("2018-01-03") & 
                          datareg <= ymd("2020-02-29") & 
                          ManifestacaoTema == "Solicitação de cadastro") %>% 
     group_by(municipioRecod) %>% 
     summarise(Total = n()) %>% 
     mutate(destaque = ifelse(Total > 1000, TRUE, FALSE)) %>% 
     ggplot(aes(x = fct_reorder(municipioRecod, Total), y = Total)) +
     geom_bar(aes(fill = destaque), stat = "identity") +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") +
     scale_fill_manual(values = c('darkgreen', 'darkred')) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           text = element_text(color = '#444444'),
           axis.text.y = element_text(hjust = 1),
           legend.position = 'none',
           axis.title = element_text(face = 'bold'),
           axis.title.y = element_text(angle = 0, vjust = .5)) +
     coord_flip()


# 
filtro327 %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                          datareg >= ymd("2018-01-03") & 
                          datareg <= ymd("2020-02-29") & 
                          ManifestacaoTema == "Solicitação de cadastro") %>% 
     group_by(dataregAnoMes) %>% 
     summarise(Total = n()) %>% 
     mutate(destaque = ifelse(Total > 1000, TRUE, FALSE)) %>% 
     ggplot(aes(x = dataregAnoMes, y = Total)) +
     geom_bar(aes(fill = destaque), stat = "identity") +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") +
     scale_fill_manual(values = c('darkgreen', 'darkred')) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           text = element_text(color = '#444444'),
           axis.text.y = element_text(hjust = 1),
           legend.position = 'none',
           axis.title = element_text(face = 'bold'),
           axis.title.y = element_text(angle = 0, vjust = .5))


### Tabelas a serem salvas em Excel
# 
tabela1 <- cadastro %>% 
     group_by(municipioRecod, datareg) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = municipioRecod, values_from = Total)
tabela1[is.na(tabela1)] <- 0


#
tabela2 <- cadastro %>% 
     group_by(municipioRecod, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela2[is.na(tabela2)] <- 0


#
tabela3 <- cadastro %>% 
     group_by(municipioRecod, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela3[is.na(tabela3)] <- 0


#
tabela4 <- cadastro %>% 
     group_by(municipioRecod, StatusManifestacao) %>% 
     summarise(Total = n())
tabela4[is.na(tabela4)] <- 0


#
write.xlsx(list(tabela1,
                tabela2,
                tabela3,
                tabela4), "SolicitacaoCadastroFiltro129_Desde20180103.xlsx", asTable = TRUE)
rm(cadastro, tabela1, tabela2, tabela3, tabela4)