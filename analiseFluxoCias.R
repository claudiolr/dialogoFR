library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
library(janitor)


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

cias <- filtro129[c(2, 6, 55, 1, 54, 8, 9, 34, 35, 53, 14, 13, 19, 20, 56)]



# Análise de manifestações por CIAs----------------------------------------

unidades <- cias %>% 
     filter(Unidade %in% c("CIA -  Aimorés",
                           "CIA -  Baixo Guandu",
                           "CIA -  Belo Oriente",
                           "CIA - Aracruz",
                           "CIA - Barra Longa",
                           "CIA - Colatina",
                           "CIA - Governador Valadares",
                           "CIA - Linhares",
                           "CIA - Mariana",
                           "CIA - Naque",
                           "CIA - Periquito",
                           "CIA - Povoação",
                           "CIA - Regência",
                           "CIA - Resplendor",
                           "CIA - Rio Doce e Sta Cruz do Escalvado",
                           "CIA - São Mateus",
                           "CIA - Tumiritinga") & dataregistro >= ymd("2018-01-03")) %>%
     group_by(municipioRecod) %>%
     count(Unidade) %>% 
     mutate(n = round(prop.table(n), digits = 3)*100) %>% 
     pivot_wider(names_from = Unidade, values_from = n, values_fill = list(n = 0))


cias %>% 
     filter(Unidade %in% c("CIA -  Aimorés",
                           "CIA -  Baixo Guandu",
                           "CIA -  Belo Oriente",
                           "CIA - Aracruz",
                           "CIA - Barra Longa",
                           "CIA - Colatina",
                           "CIA - Governador Valadares",
                           "CIA - Linhares",
                           "CIA - Mariana",
                           "CIA - Naque",
                           "CIA - Periquito",
                           "CIA - Povoação",
                           "CIA - Regência",
                           "CIA - Resplendor",
                           "CIA - Rio Doce e Sta Cruz do Escalvado",
                           "CIA - São Mateus",
                           "CIA - Tumiritinga") & dataregistro >= ymd("2018-01-03")) %>%
     group_by(municipioRecod) %>%
     count(Unidade) %>% 
     mutate(n = round(prop.table(n), digits = 3)*100) %>% 
     ggplot(aes(x = Unidade, y = municipioRecod, fill = n)) +
     geom_tile() +
     labs(title = "Municípios de origem das manifestações por CIA onde foram registradas",
          subtitle = "(Distribuição das manifestações por CIA)",
          x = "Unidade", y = "Município de origem", fill = "%") +
     guides(fill = guide_legend(reverse = TRUE)) +
     scale_fill_gradient2() +
     theme(plot.title = element_text(size = 18, hjust = 0.5),
           plot.subtitle = element_text(size = 14, hjust = 0.5),
           axis.text.x = element_text(angle = 90, hjust = 0.9, vjust = 0.4),
           panel.background = element_blank())



cias %>% 
     filter(Unidade %in% c("CIA -  Aimorés",
                           "CIA -  Baixo Guandu",
                           "CIA -  Belo Oriente",
                           "CIA - Aracruz",
                           "CIA - Barra Longa",
                           "CIA - Colatina",
                           "CIA - Governador Valadares",
                           "CIA - Linhares",
                           "CIA - Mariana",
                           "CIA - Naque",
                           "CIA - Periquito",
                           "CIA - Povoação",
                           "CIA - Regência",
                           "CIA - Resplendor",
                           "CIA - Rio Doce e Sta Cruz do Escalvado",
                           "CIA - São Mateus",
                           "CIA - Tumiritinga") & dataregistro >= ymd("2018-01-03")) %>%
     group_by(Unidade) %>%
     summarise(Total = n()) %>%
     ggplot(aes(x = Unidade, y = Total, fill = -Total, label = Total)) +
     geom_col(position = "dodge") +
     geom_text(position = position_dodge(width = 0.6),
               vjust = 1.5,
               color = "white",
               fontface = "bold") +
     labs(title = "Manifestações por CIA", subtitle = "Total de manifestações desde 03/01/2018") +
     theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))



# Análise de manifestações por tipo (solicitação de cadastro) -------------

solicitacoesCadastro <- cias %>% 
     filter(dataregistro >= ymd("2018-01-03") &
                 ManifestacaoTema == "Solicitação de cadastro") %>% 
     group_by(municipioRecod) %>%
     filter(!duplicated(manifestante_codPessoa)) %>% 
     count(municipioRecod) #%>% 
mutate(n = round(prop.table(n), digits = 3)*100)





