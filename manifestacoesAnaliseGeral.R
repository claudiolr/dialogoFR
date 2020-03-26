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
# [28] "C6manifrelgqtpoeira"            [29] "C7manifamecmasocila"         [30] "manifestacaoOrigem"
# [31] "resumo"                         [32] "dataConclusao"               [33] "resumoconclusao"
# [34] "requerAcaoFutura"               [35] "manifestacaoSubtema"         [36] "responsavelPelaDemanda"
# [37] "StatusDemanda"                  [38] "UltimoEncaminhamento"        [39] "ultimoEncaData"
# [40] "pultimoEncaDE"                  [41] "ultimoEncaPARA"              [42] "DATAHORA_EXP"
# [43] "DataRegistroAnoMes"             [44] "DataConclusaoAnoMes"         [45] "ManifestacaoFinalizada"

manifestacoes <- filtro1332[c(1, 2, 8, 45, 9, 43, 32, 44, 12, 30, 13, 14, 35, 15, 17)]




### Por mês/ano do registro
# Cria resumo com média de dias, máximo de dias e total de manifestações por mês/ano do registro da manifestação
manifestacoes %>%
     filter(statusManifestacao != "Cancelada") %>% 
     group_by(DataRegistroAnoMes, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Total = n()) %>% 
     ggplot() +
     geom_col(aes(x = DataRegistroAnoMes, y = Total, fill = ManifestacaoFinalizada)) +
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
     filter(ManifestacaoFinalizada == "Finalizada") %>% 
     filter(!is.na(DataConclusaoAnoMes)) %>% 
     group_by(DataConclusaoAnoMes, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(ndias, na.rm = TRUE), digits = 1),
               Total = n()) %>% 
     ggplot() +
     geom_col(aes(x = DataConclusaoAnoMes, y = Total, fill = Média)) +
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
manifestacoes %>%
     filter(ManifestacaoFinalizada == "Não finalizada" &
                 statusManifestacao != "Cancelada") %>% 
     group_by(DataRegistroAnoMes, ManifestacaoFinalizada, ManifestacaoAssunto) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Total = n()) %>% 
     mutate(ManifestacaoAssuntoRecod = ifelse(ManifestacaoAssunto )) %>% 
     ggplot() +
     geom_col(aes(x = DataRegistroAnoMes, y = Total, fill = ManifestacaoAssunto)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           legend.position = "bottom",
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())



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
     select(StatusManifestacaoRecod, localidadedemandadesc, ndias) %>% 
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
     group_by(territorio, manifestacaoOrigemRecod, ManifestacaoFinalizada) %>% 
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

ggplot(manifestacoes) +
     geom_col(aes(x = DataRegistroAnoMes, y = mean(ndias), fill = ManifestacaoFinalizada)) +
     labs(title = "Dias decorridos desde \n o registro da manifestação",
          x = "Mês/ano", y = "Dias") +
     scale_y_continuous(breaks = seq(0, 100, 10)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


manifestacoes %>% filter(StatusManifestacao != "Cancelada") %>% 
     ggplot() +
     geom_col(aes(x = DataConclusaoAnoMes, y = mean(ndias), fill = ManifestacaoFinalizada)) +
     labs(title = "Dias decorridos desde \n o registro da manifestação",
          x = "Mês/ano", y = "Dias") +
     scale_y_continuous(breaks = seq(0, 1500, 50)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


ggplot(manifestacoes, aes(x = DataConclusaoAnoMes, y = ManifestacaoFinalizada)) +
     geom_col() +
     labs(title = "Manifestações finalizadas, por mês", x = "", y = "") + 
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())



ggplot(territorioOrigem, aes(x = territorio, y = Soma, fill = ManifestacaoOrigemRecod)) +
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
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(localidadedemandadesc, ManifestacaoFinalizada) %>%
     summarise(Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Total) %>% 
     mutate(Total = sum(Finalizada, `Não finalizada`)) %>% 
     mutate(`Taxa de Finalização` = (Finalizada/Total)*100)


TotalMesAtual <- manifestacoes %>%
     select(localidadedemandadesc, DataRegistroAnoMes, ndias) %>% 
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     filter(DataRegistroAnoMes == "2020/02" | DataRegistroAnoMes == "2020/03") %>% 
     group_by(localidadedemandadesc, DataRegistroAnoMes) %>%
     summarise(DemandasMes = n()) %>% 
     pivot_wider(names_from = DataRegistroAnoMes, values_from = DemandasMes)

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
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(StatusManifestacaoRecod) %>% 
     summarise(Total = n())


### Página 1 (segunda parte)
Mensal <- manifestacoes %>%
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     group_by(DataRegistroAnoMes, localidadedemandadesc) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = localidadedemandadesc, values_from = c(Total)) %>% 
     mutate(Total = sum(`Barra Longa`, `Rio Doce`, `Santa Cruz do Escalvado`, na.rm = TRUE))


TempoResolucao <- manifestacoes %>%
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     filter(ManifestacaoFinalizada == "Finalizada") %>% 
     group_by(DataRegistroAnoMes) %>% 
     summarise(Média = mean(ndias))



### Página 2
AssuntoTema <- manifestacoes %>%
     filter(localidadedemandadesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Total = n(),
               Média = round(mean(ndias), digits = 1),
               Máximo = max(ndias)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Total, Média, Máximo)) %>% 
     mutate("Total de manifestações" = sum(Total_Finalizada, `Total_Não finalizada`, na.rm = TRUE)) %>% 
     filter(`Total de manifestações` > 30) %>%
     arrange(ManifestacaoAssunto, desc(`Total de manifestações`))



### Salva os cálculos
setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202003")
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




# Demanda 16/03 -----------------------------------------------------------

cadastro <- manifestacoes %>% filter(ManifestacaoAssunto == "PG001 Levantamento e Cadastro" &
                                          DataRegistro >= ymd("2018-01-03") & 
                                          ManifestacaoTema == "Solicitação de cadastro")

cadastro$ManifestacaoTemaRecod <- ifelse(cadastro$ManifestacaoTema %in% c("Solicitação de atualização de informações do formulário entregue",
                                                                          "Solicitação de correção de informações do formulário entregue",
                                                                          "Informação sobre devolutivas",
                                                                          "Solicitação de cópia do formulário",
                                                                          "Solicitação de contato para não localizados",
                                                                          "Solicitação de resposta formal",
                                                                          "Solicitação de cancelamento do cadastro",
                                                                          "Impossibilidade de estar na cidade da propriedade impactada",
                                                                          "Atualização de informações pessoais",
                                                                          "Informações sobre o programa"), "Outros",
                                         ifelse(cadastro$ManifestacaoTema == "Situação cadastral", "Situação cadastral",
                                                ifelse(cadastro$ManifestacaoTema == "Solicitação de cadastro", "Solicitação de cadastro",
                                                       ifelse(cadastro$ManifestacaoTema == "Informação sobre o processo", "Informação sobre o processo", NA))))




setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/FundRenova/DialogoSocial/AnaliseManifestacoes")


png("SolicitacoesTotalMes.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = DataRegistroAnoMes)) +
     geom_bar(stat = "count", position = 'dodge', na.rm = TRUE, fill = "black") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1.0),
               vjust = 1,
               color = "white", size = 4) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 2800, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())
dev.off()

png("SolicitacoesTotalMes_wrap.png", width = 1600, height = 1000)
ggplot(cadastro, aes(x = DataRegistroAnoMes)) +
     geom_bar(stat = "count", position = 'dodge', na.rm = TRUE, fill = "black") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1.0),
               vjust = -0.5,
               color = "black", size = 4) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 2800, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ StatusManifestacao, nrow = 5, ncol = 1)
dev.off()


png("SolicitacoesTotalMesStatus.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = DataRegistroAnoMes, fill = StatusManifestacao)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = 1,
               color = "white", size = 4) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 2800, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())
dev.off()


png("SolicitacoesTotalMesStatus_wrap.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = DataRegistroAnoMes, fill = StatusManifestacao)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = -0.5,
               color = "black", size = 4) +
     labs(title = "Solicitações de cadastro por mês (desde 01/03/2018)", x = "", y = "") + 
     scale_y_continuous(breaks = seq(0, 2800, 200)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ StatusManifestacao, nrow = 5, ncol = 1)
dev.off()


png("SolicitacoesMunicipio.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = fct_infreq(localidadedemandadesc))) +
     geom_bar(stat = "count", position = "dodge", fill = "#2C3E50") +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_dodge(width = 1),
               vjust = -0.5,
               size = 4) +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 5000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(size = 12, vjust = 1),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     coord_flip()
dev.off()


png("SolicitacoesMunicipioStatus.png", width = 1600, height = 800)
ggplot(cadastro, aes(x = fct_infreq(localidadedemandadesc), fill = StatusManifestacao)) +
     geom_bar(position = "stack") +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 5000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank())
dev.off()

pdf("SolicitacoesMunicipioStatus_wrap.pdf", width = 1600, height = 800)
ggplot(cadastro, aes(x = fct_infreq(localidadedemandadesc), fill = StatusManifestacao)) +
     geom_bar(stat = "count", position = 'stack', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count",
               position = position_stack(),
               vjust = -0.5,
               color = "black", size = 4) +
     labs(title = "Solicitações de cadastro por município (desde 01/03/2018)", x = "", y = "") +
     scale_y_continuous(breaks = seq(0, 5000, 1000)) +
     theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 12, vjust = 0.5),
           axis.text.y = element_text(size = 12),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           legend.position = "bottom", panel.background = element_blank()) +
     facet_wrap( ~ StatusManifestacao, nrow = 5, ncol = 1)
dev.off()



tabela1 <- cadastro %>% 
     group_by(localidadedemandadesc, DataRegistroAnoMes) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = localidadedemandadesc, values_from = Total)
tabela1[is.na(tabela1)] <- 0


tabela2 <- cadastro %>% 
     group_by(DataRegistroAnoMes, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela2[is.na(tabela2)] <- 0


tabela3 <- cadastro %>% 
     group_by(localidadedemandadesc, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela3[is.na(tabela3)] <- 0


tabela4 <- cadastro %>% 
     group_by(localidadedemandadesc, StatusManifestacao) %>% 
     summarise(Total = n())
tabela4[is.na(tabela4)] <- 0


write.xlsx(list(tabela1,
                tabela2,
                tabela3,
                tabela4), "SolicitacaoCadastro_Desde20180103.xlsx")
rm(cadastro, tabela1, tabela2, tabela3, tabela4)
