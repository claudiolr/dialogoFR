library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Escolha das colunas a partir do filtro utilizado ------------------------

########## Filtro1332.R
# [1] "idManifestacao"                  [2] "protocolo"                    [3] "manifestante_codPessoa"
# [4] "manifestante"                    [5] "SEXO"                         [6] "idFamilia"
# [7] "statusPostoAtendimento"          [8] "statusManifestacao"           [9] "dataregistro"
# [10] "datareg"                        [11] "dataLimiteConclusao"         [12] "ndias"
# [13] "ManifestacaoAssunto"            [14] "ManifestacaoTema"            [15] "localidadedemandadesc"
# [16] "idterritorio"                   [17] "territorio"                  [18] "comunidade_origem"
# [19] "localidadedemandadescuf"        [20] "analistaResponsavel"         [21] "manifestacaoCriticidade"
# [22] "C1manifreldiffin"               [23] "C2manifrelprosaude"          [24] "C3manifsolaliment"
# [25] "C4manifrelriscalg"              [26] "C5manifsolappisc"            [27] "C8manifreltensuic"
# [28] "C6manifrelgqtpoeira"            [29] "C7manifamFecmasocila"         [30] "manifestacaoOrigem"
# [31] "resumo"                         [32] "dataConclusao"               [33] "resumoconclusao"
# [34] "requerAcaoFutura"               [35] "manifestacaoSubtema"         [36] "responsavelPelaDemanda"
# [37] "StatusDemanda"                  [38] "UltimoEncaminhamento"        [39] "ultimoEncaData"
# [40] "pultimoEncaDE"                  [41] "ultimoEncaPARA"              [42] "DATAHORA_EXP"
# [43] "DataRegistroAnoMes"             [44] "DataConclusaoAnoMes"         [45] "ManifestacaoFinalizada"

manifestacoes <- filtro1332[c(1, 2, 8, 45, 9, 43, 32, 44, 12, 30, 13, 14, 35, 15, 17)]


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

manifestacoes <- filtro327[c(2, 3, 6, 21, 22, 83, 1, 54, 55, 9, 40, 41, 42, 45, 68, 64)]
manifestacoes <- filtro327[c(2, 3, 6, 21, 22, 82, 1, 53, 54, 9, 40, 41, 44, 67, 65)]



### Por mês/ano do registro
# Cria resumo com média de dias, máximo de dias e total de manifestações por mês/ano do registro da manifestação
manifestacoes %>%
     filter(statusManifestacao != "Cancelada") %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     group_by(datareg, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Total = n()) %>% 
     ggplot() +
     geom_col(aes(x = datareg, y = Total, fill = ManifestacaoFinalizada)) +
     labs(title = "Manifestações registradas por mês", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 30000, 2500)) +
     scale_fill_manual(name = "Status", values = c("#3d800e", "#b81a2c"), labels = c("Finalizada", "Não finalizada")) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           legend.position = "bottom",
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


### Por mês/ano da finalização
manifestacoes %>%
     mutate(dataconclusao = format(dataconclusao, "%Y-%m")) %>% 
     filter(ManifestacaoFinalizada == "Finalizada") %>% 
     filter(!is.na(dataconclusao)) %>% 
     group_by(dataconclusao, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias, na.rm = TRUE), digits = 1),
               Total = n()) %>% 
     ggplot() +
     geom_col(aes(x = dataconclusao, y = Total, fill = Média)) +
     labs(title = "Manifestações concluídas por mês,\n por média de tempo de resolução", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 50000, 2500)) +
     scale_fill_gradient(name = "Média de tempo (dias)", low = "#036121", high = "#c71221") +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           legend.position = "bottom",
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


### Por mês/ano do registro e tipo
# manifestacoes %>%
#      mutate(datareg = format(datareg, "%Y-%m")) %>% 
#      filter(ManifestacaoFinalizada == "Não finalizada" &
#                  statusManifestacao != "Cancelada") %>% 
#      group_by(datareg, ManifestacaoFinalizada, ManifestacaoAssunto) %>% 
#      summarise(Média = round(mean(ndias), digits = 1),
#                Total = n()) %>% 
#      mutate(ManifestacaoAssuntoRecod = ifelse(ManifestacaoAssunto )) %>% 
#      ggplot() +
#      geom_col(aes(x = DataRegistroAnoMes, y = Total, fill = ManifestacaoAssunto)) +
#      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#            legend.position = "bottom",
#            axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
#            axis.ticks.y = element_line(colour = "gray", size = .5),
#            panel.grid.major.y = element_line(colour = "gray", size = .5),
#            panel.background = element_blank())



### Assunto
# Cria resumo com média de dias, máximo de dias e total de manifestações por assunto
manifestacoes %>% 
     group_by(ManifestacaoAssunto, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias, na.rm = TRUE), digits = 1),
               Máximo = max(ndias),
               Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Média, Máximo, Total))


### Por Tema
# Cria resumo com média de dias, máximo de dias e total de manifestações por tema
manifestacoes %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Máximo = max(ndias),
               Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Média, Máximo, Total))


### Por Canais
# Cria resumo com média de dias, máximo de dias e total de manifestações por canais
manifestacoes %>% 
     group_by(manifestacaoOrigem, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Máximo = max(ndias),
               Total = n()) %>% 
     pivot_wider(names_from = c(ManifestacaoFinalizada), values_from = c(Média, Máximo, Total))



### Cálculo de Tipos de manifestações
# Cria resumo com média de dias, máximo de dias e total de manifestações por status da manifestação (finalizada e não finalizada)
manifestacoes %>% 
     mutate(StatusManifestacaoRecod = ifelse(statusManifestacao %in%
                                                  c("Aguarda Conclusão do Atendimento",
                                                    "Aguardando conclusão",
                                                    "Cancelada",
                                                    "Em tratamento",
                                                    "Em tratamento para Canais de Relacionamento",
                                                    "Em tratamento para elaboração de resposta escrita",
                                                    "Em tratamento para resposta final",
                                                    "Em tratamento para resposta intermediária"), "Não finalizada",
                                             ifelse(statusManifestacao %in% c("Respondida",
                                                           "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                             "Finalizada", "Finalizada no ato"))) %>% 
     group_by(StatusManifestacaoRecod) %>% 
     summarise(Quantidade = n(),
               Média = round(mean(ndias), digits = 1),
               Mínimo = min(ndias))



### Por território e origem da demanda
# Por território
manifestacoes %>% 
     mutate(manifestacaoOrigemRecod = ifelse(manifestacaoOrigem %in% c("0800",
                                                                       "Contato Ativo 0800/Fale Conosco"), "0800",
                                             ifelse(manifestacaoOrigem %in% c("Contato Ativo CIAs",
                                                                              "Postos de Atendimento CIAs"), "CIAs",
                                                    ifelse(manifestacaoOrigem %in% c("Fale Conosco",
                                                                                     "Fale Conosco - Portal"), "Portal do usuário",
                                                           ifelse(manifestacaoOrigem %in% c("Abordagem presencial",
                                                                                            "Mobilização",
                                                                                            "Reunião de Diálogo",
                                                                                            "Eventos"), "Diálogo", "Outros"))))) %>% 
     group_by(territorio, ManifestacaoFinalizada, manifestacaoOrigemRecod) %>% 
     summarise(Total = n())


     
# Por origem da demanda
manifestacoes %>%
     mutate(FormaRecebimentoRecod = ifelse(FormaRecebimento %in% c("0800",
                                                                       "Contato Ativo 0800/Fale Conosco"), "0800",
                                             ifelse(FormaRecebimento %in% c("Contato Ativo CIAs",
                                                                              "Postos de Atendimento CIAs"), "CIAs",
                                                    ifelse(FormaRecebimento %in% c("Fale Conosco",
                                                                                     "Fale Conosco - Portal"), "Portal do usuário",
                                                           ifelse(FormaRecebimento %in% c("Abordagem presencial",
                                                                                            "Mobilização",
                                                                                            "Reunião de Diálogo",
                                                                                            "Eventos"), "Diálogo", "Outros"))))) %>% 
     group_by(territorio, FormaRecebimentoRecod, ManifestacaoFinalizada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Total)



setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202002")
write.xlsx(list(Assunto = Assunto,
                Tema = Tema,
                Origem = Origem,
                MesAnoRegistro = MesAnoRegistro,
                ManifestacoesStatus = ManifestacoesStatus,
                ManifestacoesStatusGrupo = ManifestacoesStatusGrupo,
                Territorio = territorio,
                TerritorioOrigem = territorioOrigem
),
"RptMensal.xlsx", which = c("Assunto",
                            "Tema",
                            "Canais",
                            "MesAno",
                            "ManifestacoesStatus",
                            "ManifestacoesStatusGrupo"
),
headerStyle = createStyle(halign = "center", textDecoration = "bold"),
firstCol = TRUE, firstRow = TRUE, colWidths = "auto", withFilter = TRUE)

rm(Assunto,
   Tema,
   Canais,
   MesAnoRegistro,
   ManifestacoesStatus,
   ManifestacoesStatusGrupo,
   territorio, 
   territorioOrigem)



# Gráficos ----------------------------------------------------------------

manifestacoes %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     group_by(datareg, ManifestacaoFinalizada) %>% 
     ggplot() +
     geom_col(aes(x = datareg, y = mean(ndias), fill = ManifestacaoFinalizada)) +
     labs(title = "Dias decorridos desde \n o registro da manifestação",
          x = "Mês/ano", y = "Dias") +
     scale_y_continuous(breaks = seq(0, 100, 10)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


manifestacoes %>%
     mutate(dataconclusao = format(dataconclusao, "%Y-%m")) %>% 
     filter(statusManifestacao != "Cancelada" & !is.na(statusManifestacao)) %>% 
     filter(!is.na(dataconclusao)) %>% 
     ggplot() +
     geom_col(aes(x = dataconclusao, y = mean(ndias), fill = ManifestacaoFinalizada)) +
     labs(title = "Dias decorridos desde \n o registro da manifestação",
          x = "Mês/ano", y = "Dias") +
     scale_y_continuous(breaks = seq(0, 600000, 50000)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


manifestacoes %>%
     mutate(dataconclusao = format(dataconclusao, "%Y-%m")) %>% 
     ggplot(aes(x = dataconclusao, y = ManifestacaoFinalizada)) +
     geom_col() +
     labs(title = "Manifestações finalizadas, por mês", x = "", y = "") + 
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


manifestacoes %>% 
     ggplot(aes(x = territorio, y = Soma, fill = ManifestacaoOrigemRecod)) +
     geom_bar(stat = "identity",
              position = "fill") + 
     theme_minimal() +
     scale_fill_brewer(palette = "Set2") +
     labs(title = "Manifestações regitradas por território", x = "", y = "") + 
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 45, size = 8, vjust = 1, hjust = 1, face = "bold"),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5)) +
     geom_text(aes(label = Soma), position = position_stack (vjust = 0.5), size = 3)


# Por territórios específicos ---------------------------------------------

# 1. Mariana: MRN
# 2. Alto Rio Doce: ARD
# 3. Calha do Rio Doce: CRD
# 4. Médio Rio Doce: MRD
# 5. Baixo Rio Doce: BRD
# 6. Foz do Rio Doce: FRD


### Cabeçalho do relatório
TotalGeral <- manifestacoes %>%
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(municipio_origem, ManifestacaoFinalizada) %>%
     summarise(Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Total) %>% 
     mutate(Total = sum(Finalizada, `Não finalizada`)) %>% 
     mutate(`Taxa de Finalização` = (Finalizada/Total)*100)


TotalMesAtual <- manifestacoes %>%
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     filter(datareg == "2020-02" | datareg == "2020-03") %>% 
     group_by(municipio_origem, datareg) %>%
     summarise(DemandasMes = n()) %>% 
     pivot_wider(names_from = datareg, values_from = DemandasMes)

Geral <- merge(TotalMesAtual, TotalGeral, by = "localidadedemandadesc")
rm(TotalMesAtual, TotalGeral)


### Cálculo de Tipos de manifestações (pizza)
ManifestacoesStatus <- manifestacoes %>% 
     mutate(StatusManifestacaoRecod = ifelse(statusManifestacao %in%
                                                  c("Aguarda Conclusão do Atendimento",
                                                    "Aguardando conclusão",
                                                    "Cancelada",
                                                    "Em tratamento",
                                                    "Em tratamento para Canais de Relacionamento",
                                                    "Em tratamento para elaboração de resposta escrita",
                                                    "Em tratamento para resposta final",
                                                    "Em tratamento para resposta intermediária"), "Não finalizada",
                                             ifelse(statusManifestacao
                                                    %in% c("Respondida",
                                                           "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                    "Finalizada", "Finalizada no ato"))) %>%
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(StatusManifestacaoRecod) %>% 
     summarise(Total = n())


### Página 1 (segunda parte)
Mensal <- manifestacoes %>%
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     group_by(datareg, municipio_origem) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = municipio_origem, values_from = c(Total)) %>% 
     mutate(Total = sum(`Barra Longa`, `Rio Doce`, `Santa Cruz do Escalvado`, na.rm = TRUE))


TempoResolucao <- manifestacoes %>%
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     filter(ManifestacaoFinalizada == "Finalizada") %>% 
     group_by(datareg) %>% 
     summarise(Média = mean(ndias))



### Página 2
AssuntoTema <- manifestacoes %>%
     filter(municipio_origem %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Total = n(),
               Média = round(mean(ndias), digits = 1),
               Máximo = max(ndias)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Total, Média, Máximo)) %>% 
     mutate("Total de manifestações" = sum(Total_Finalizada, `Total_Não finalizada`, na.rm = TRUE)) %>% 
     filter(`Total de manifestações` > 30) %>%
     arrange(ManifestacaoAssunto, desc(`Total de manifestações`))



### Salva os cálculos
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202003")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202003")
write.xlsx(list(Cabeçalho = Geral,
                ManifestacoesStatus = ManifestacoesStatus,
                MensalResolucao = Mensal,
                TempoMedioResolucao = TempoResolucao,
                AssuntoTema = AssuntoTema),
           "RptMensal_ARD.xlsx",
           headerStyle = createStyle(halign = "center", textDecoration = "bold"),
           firstCol = TRUE, firstRow = TRUE, colWidths = "auto", withFilter = TRUE)

rm(AssuntoTema,
   Geral,
   ManifestacoesStatus,
   Mensal,
   TempoResolucao)