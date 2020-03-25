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


### Filtra por: solicitação de cadastro e data
cadastro <- cadastro %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                                  dataregistro >= ymd("2018-01-03") & 
                                  dataregistro <= ymd("2020-02-29") & 
                                  ManifestacaoTema == "Solicitação de cadastro")


### Gráficos

# 
png("SolicitacoesTotalMes.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = DataRegistroAnoMes, fill = ManifestacaoFinalizada)) +
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
dev.off()


# 
ggplot(cadastro, aes(x = DataRegistroAnoMes)) +
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
ggplot(cadastro, aes(x = DataRegistroAnoMes, fill = StatusManifestacao)) +
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
ggplot(cadastro, aes(x = DataRegistroAnoMes, fill = StatusManifestacao)) +
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
ggplot(cadastro, aes(x = fct_infreq(municipioRecod))) +
     geom_bar(stat = "count", position = "dodge", fill = "#2C3E50") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1),
               vjust = -0.5,
               size = 3.5) +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 6000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())


# 
ggplot(cadastro, aes(x = fct_infreq(municipioRecod), fill = StatusManifestacao)) +
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
ggplot(cadastro, aes(x = fct_infreq(municipioRecod))) +
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
filtro129 %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                          dataregistro >= ymd("2018-01-03") & 
                          dataregistro <= ymd("2020-02-29") & 
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
filtro129 %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                          dataregistro >= ymd("2018-01-03") & 
                          dataregistro <= ymd("2020-02-29") & 
                          ManifestacaoTema == "Solicitação de cadastro") %>% 
     group_by(DataRegistroAnoMes) %>% 
     summarise(Total = n()) %>% 
     mutate(destaque = ifelse(Total > 1000, TRUE, FALSE)) %>% 
     ggplot(aes(x = DataRegistroAnoMes, y = Total)) +
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
     group_by(municipioRecod, DataRegistroAnoMes) %>% 
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