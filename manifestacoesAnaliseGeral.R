library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Escolha das colunas a partir do filtro utilizado ------------------------

########## Filtro129.R
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

manifestacoes <- filtro1332[c(1, 2, 8, 45, 9, 43, 32, 44, 12, 13, 14, 35, 15)]




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
     filter(ManifestacaoFinalizada == "Não finalizada") %>% 
     filter(statusManifestacao != "Cancelada") %>% 
     group_by(DataRegistroAnoMes, ManifestacaoFinalizada, ManifestacaoAssunto) %>% 
     summarise(Média = round(mean(ndias), digits = 1),
               Total = n()) %>% 
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
Assunto <- manifestacoes %>% 
     select(ManifestacaoAssunto, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1),
               Máximo = max(Ndias),
               Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Média, Máximo, Total))


### Por Tema
# Cria resumo com média de dias, máximo de dias e total de manifestações por tema
Tema <- manifestacoes %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1),
               Máximo = max(Ndias),
               Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = c(Média, Máximo, Total))


### Por Canais
# Cria resumo com média de dias, máximo de dias e total de manifestações por canais
Origem <- manifestacoes %>% 
     group_by(ManifestacaoOrigem, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1),
               Máximo = max(Ndias),
               Total = n()) %>% 
     pivot_wider(names_from = c(ManifestacaoFinalizada), values_from = c(Média, Máximo, Total))




### Cálculo de Tipos de manifestações
# Cria resumo com média de dias, máximo de dias e total de manifestações por status da manifestação (finalizada e não finalizada)
ManifestacoesStatus <- manifestacoes %>% 
     select(StatusManifestacao, LocalidadeDemandaDesc, Ndias) %>% 
     group_by(StatusManifestacao) %>% 
     summarise(Quantidade = n(),
               Média = round(mean(Ndias), digits = 1))

ManifestacoesStatus$StatusManifestacaoRecod <- ifelse(ManifestacoesStatus$StatusManifestacao %in%
                                                           c("Aguarda Conclusão do Atendimento",
                                                             "Aguardando conclusão",
                                                             "Cancelada",
                                                             "Em tratamento",
                                                             "Em tratamento para Canais de Relacionamento",
                                                             "Em tratamento para elaboração de resposta escrita",
                                                             "Em tratamento para resposta final",
                                                             "Em tratamento para resposta intermediária"), "Não finalizada",
                                                      ifelse(ManifestacoesStatus$StatusManifestacao 
                                                             %in% c("Respondida",
                                                                    "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                             "Finalizada", "Finalizada no ato"))


### Manifestações por categoria recodificada em três
ManifestacoesStatusGrupo <- ManifestacoesStatus %>%
     select(Quantidade, StatusManifestacaoRecod) %>% 
     group_by(StatusManifestacaoRecod) %>% 
     summarise(Total = sum(Quantidade))


### Por território e origem da demanda
# Por território
territorio <- manifestacoes %>% 
     select(territorio, ManifestacaoOrigem, ManifestacaoFinalizada) %>% 
     group_by(territorio, ManifestacaoFinalizada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Total)

territorio$territorio[is.na(territorio$territorio)] <- "Geral"
territorio$territorio <- as.factor(territorio$territorio)



# Por origem da demanda
territorioOrigem <- manifestacoes %>% 
     group_by(territorio, ManifestacaoOrigem, ManifestacaoFinalizada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Total)


territorioOrigem$territorio <- factor(territorioOrigem$territorio, levels = c("Território de Mariana",
                                                                              "Território Alto Rio Doce",
                                                                              "Território Calha do Rio Doce",
                                                                              "Território Médio Rio Doce",
                                                                              "Território Baixo Rio Doce",
                                                                              "Território Foz Rio Doce",
                                                                              "Sem informação"))


territorioOrigem$ManifestacaoOrigem <- as.factor(territorioOrigem$ManifestacaoOrigem)

territorioOrigem$ManifestacaoOrigemRecod <- ifelse(territorioOrigem$ManifestacaoOrigem %in% c("0800",
                                                                                              "Contato Ativo 0800/Fale Conosco"), "0800",
                                                   ifelse(territorioOrigem$ManifestacaoOrigem %in% c("Contato Ativo CIAs",
                                                                                                     "Postos de Atendimento CIAs"), "CIAs",
                                                          ifelse(territorioOrigem$ManifestacaoOrigem %in% c("Fale Conosco",
                                                                                                            "Fale Conosco - Portal"), "Portal do usuário",
                                                                 ifelse(territorioOrigem$ManifestacaoOrigem %in% c("Abordagem presencial",
                                                                                                                   "Mobilização",
                                                                                                                   "Reunião de Diálogo",
                                                                                                                   "Eventos"), "Diálogo", "Outros"))))


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



###########################################################################
# Gráficos ----------------------------------------------------------------
###########################################################################

ggplot(manifestacoes) +
     geom_col(aes(x = DataRegistroAnoMes, y = mean(Ndias), fill = ManifestacaoFinalizada)) +
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
     geom_col(aes(x = DataConclusaoAnoMes, y = mean(Ndias), fill = ManifestacaoFinalizada)) +
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


###########################################################################
# Por territórios específicos ---------------------------------------------
###########################################################################

# 1. Mariana: MRN
# 2. Alto Rio Doce: ARD
# 3. Calha do Rio Doce: CRD
# 4. Médio Rio Doce: MRD
# 5. Baixo Rio Doce: BRD
# 6. Foz do Rio Doce: FRD


### Por Assunto
AssuntoMedia_ARD <- manifestacoes %>%
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema) %>% 
     summarise(Média = round(mean(Ndias), digits = 1))
colnames(AssuntoMedia_ARD) = c("ManifestacaoAssunto",
                               "ManifestacaoTema",
                               "MediaDiasResolucao")


AssuntoMediaFin_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Média)
colnames(AssuntoMediaFin_ARD) = c("ManifestacaoAssunto",
                                  "ManifestacaoTema",
                                  "MediaDiasResolucaoFinalizadas",
                                  "MediaDiasResolucaoNaoFinalizadas")


AssuntoMaximo_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Máximo = max(Ndias)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Máximo)
colnames(AssuntoMaximo_ARD) = c("ManifestacaoAssunto",
                                "ManifestacaoTema",
                                "TempoMaximoFinalizadas",
                                "TempoMaximoNaoFinalizadas")


AssuntoTotal_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema) %>% 
     count(ManifestacaoTema, name = "Manifestacoes")
colnames(AssuntoTotal_ARD) = c("ManifestacaoAssunto",
                               "ManifestacaoTema",
                               "TotalManifestacoes")


AssuntoFin_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     count(ManifestacaoTema, name = "Manifestacoes") %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Manifestacoes)
colnames(AssuntoFin_ARD) = c("ManifestacaoAssunto",
                             "ManifestacaoTema",
                             "TotalManifestacoesFinalizadas",
                             "TotalManifestacoesNaoFinalizadas")


Assunto_ARD <- cbind(AssuntoTotal_ARD, AssuntoFin_ARD, AssuntoMedia_ARD, AssuntoMediaFin_ARD, AssuntoMaximo_ARD)
Assunto_ARD <- Assunto_ARD[c("ManifestacaoAssunto",
                             "ManifestacaoTema",
                             "TotalManifestacoes",
                             "TotalManifestacoesFinalizadas",
                             "TotalManifestacoesNaoFinalizadas",
                             "MediaDiasResolucao",
                             "MediaDiasResolucaoFinalizadas",
                             "MediaDiasResolucaoNaoFinalizadas",
                             "TempoMaximoFinalizadas",
                             "TempoMaximoNaoFinalizadas")]
rm(AssuntoFin_ARD, AssuntoMaximo_ARD, AssuntoMedia_ARD, AssuntoMediaFin_ARD, AssuntoTotal_ARD)

Assunto_ARD_30 <- Assunto_ARD[which(Assunto_ARD$TotalManifestacoes > 30),]

Assunto_ARD_30$ManifestacaoAssunto <- Assunto_ARD_30$ManifestacaoAssunto %>%
     str_replace_all(c("PG001 Levantamento e Cadastro" = "PG01",
                       "PG002 Ressarcimento e Indenização" = "Pg02",
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
                       "PG025 Recuperação da Área Ambiental 1" = "PG25",
                       "PG026 Recuperação de APPs" = "PG26",
                       "PG027 Recuperação de Nascentes" = "PG27",
                       "PG030 Fauna e Flora Terrestre Ameaçadas de Extinção" = "PG30",
                       "PG031 Coleta e Tratamento de Esgoto e Destinação de Resíduos Sólidos" = "PG31",
                       "PG032 Tratamento de Água e Captação Alternativa" = "PG32",
                       "PG034 Preparação para Emergência Ambiental" = "PG34",
                       "PG037 Gestão de Riscos Ambientais" = "PG37",
                       "PG038 Monitoramento da Bacia do Rio Doce" = "PG38",
                       "PG040 CAR e PRAs" = "PG40"))

Assunto_ARD_30 <- arrange(Assunto_ARD_30, ManifestacaoAssunto, desc(TotalManifestacoes))



### Por Tema
TemaMedia_ARD <- manifestacoes %>%
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema) %>% 
     summarise(Média = round(mean(Ndias), digits = 1))
colnames(TemaMedia_ARD) = c("ManifestacaoAssunto",
                            "ManifestacaoTema",
                            "MediaDiasResolucao")


TemaMediaFin_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Média)
colnames(TemaMediaFin_ARD) = c("ManifestacaoAssunto",
                               "ManifestacaoTema",
                               "MediaDiasResolucaoFinalizadas",
                               "MediaDiasResolucaoNaoFinalizadas")


TemaMaximo_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, Ndias, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     summarise(Máximo = max(Ndias)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Máximo)
colnames(TemaMaximo_ARD) = c("ManifestacaoAssunto",
                             "ManifestacaoTema",
                             "TempoMaximoFinalizadas",
                             "TempoMaximoNaoFinalizadas")


TemaTotal_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema) %>% 
     count(ManifestacaoTema, name = "Manifestacoes")
colnames(TemaTotal_ARD) = c("ManifestacaoAssunto",
                            "ManifestacaoTema",
                            "TotalManifestacoes")


TemaFin_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, ManifestacaoFinalizada) %>% 
     count(ManifestacaoTema, name = "Manifestacoes") %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Manifestacoes)
colnames(TemaFin_ARD) = c("ManifestacaoAssunto",
                          "ManifestacaoTema",
                          "TotalManifestacoesFinalizadas",
                          "TotalManifestacoesNaoFinalizadas")


Tema_ARD <- cbind(TemaTotal_ARD, TemaFin_ARD, TemaMedia_ARD, TemaMediaFin_ARD, TemaMaximo_ARD)
Tema_ARD <- Tema_ARD[c("ManifestacaoAssunto",
                       "ManifestacaoTema",
                       "TotalManifestacoes",
                       "TotalManifestacoesFinalizadas",
                       "TotalManifestacoesNaoFinalizadas",
                       "MediaDiasResolucao",
                       "MediaDiasResolucaoFinalizadas",
                       "MediaDiasResolucaoNaoFinalizadas",
                       "TempoMaximoFinalizadas",
                       "TempoMaximoNaoFinalizadas")]
rm(TemaFin_ARD, TemaMaximo_ARD, TemaMedia_ARD, TemaMediaFin_ARD, TemaTotal_ARD)

Tema_ARD_30 <- Tema_ARD[which(Tema_ARD$TotalManifestacoes > 30),]

Tema_ARD_30$ManifestacaoAssunto <- Tema_ARD_30$ManifestacaoAssunto %>%
     str_replace_all(c("PG001 Levantamento e Cadastro" = "PG01",
                       "PG002 Ressarcimento e Indenização" = "Pg02",
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
                       "PG025 Recuperação da Área Ambiental 1" = "PG25",
                       "PG026 Recuperação de APPs" = "PG26",
                       "PG027 Recuperação de Nascentes" = "PG27",
                       "PG030 Fauna e Flora Terrestre Ameaçadas de Extinção" = "PG30",
                       "PG031 Coleta e Tratamento de Esgoto e Destinação de Resíduos Sólidos" = "PG31",
                       "PG032 Tratamento de Água e Captação Alternativa" = "PG32",
                       "PG034 Preparação para Emergência Ambiental" = "PG34",
                       "PG037 Gestão de Riscos Ambientais" = "PG37",
                       "PG038 Monitoramento da Bacia do Rio Doce" = "PG38",
                       "PG040 CAR e PRAs" = "PG40"))

Tema_ARD_30 <- arrange(Tema_ARD_30, ManifestacaoAssunto, desc(TotalManifestacoes))


### Por mês/ano

TotalGeral_ARD <-  manifestacoes %>%
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(LocalidadeDemandaDesc, DataRegistroAnoMes, Ndias) %>% 
     count(LocalidadeDemandaDesc, name = "DemandasTotal")

TotalMesAtual_ARD <- manifestacoes %>%
     select(LocalidadeDemandaDesc, DataRegistroAnoMes, Ndias) %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     filter(DataRegistroAnoMes == "2020/01") %>% 
     group_by(LocalidadeDemandaDesc, DataRegistroAnoMes) %>% 
     count(LocalidadeDemandaDesc, name = "DemandasMes")

TotalFinalizadasMesAtual_ARD <- manifestacoes %>%
     select(LocalidadeDemandaDesc, DataRegistroAnoMes, ManifestacaoFinalizada) %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     group_by(LocalidadeDemandaDesc, ManifestacaoFinalizada) %>% 
     count(ManifestacaoFinalizada, name = "Finalizadas") %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Finalizadas)
TotalFinalizadasMesAtual_ARD$TaxaFinalizacao = ((TotalFinalizadasMesAtual_ARD$Finalizada) / (TotalFinalizadasMesAtual_ARD$Finalizada+(TotalFinalizadasMesAtual_ARD$`Não finalizada`)))

Geral_ARD <- merge(TotalMesAtual_ARD, TotalGeral_ARD, by = "LocalidadeDemandaDesc")
Geral_ARD <- merge(Geral_ARD, TotalFinalizadasMesAtual_ARD, by = "LocalidadeDemandaDesc")
rm(TotalMesAtual_ARD, TotalGeral_ARD, TotalFinalizadasMesAtual_ARD)


MensalFinalizadas_ARD <- manifestacoes %>%
     select(LocalidadeDemandaDesc, DataRegistroAnoMes, ManifestacaoFinalizada) %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     group_by(DataRegistroAnoMes, ManifestacaoFinalizada) %>% 
     count(ManifestacaoFinalizada, name = "Finalizadas") %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Finalizadas)


MediaTempoFinalizadas_ARD <- manifestacoes %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     select(DataRegistroAnoMes, Ndias, ManifestacaoFinalizada) %>% 
     group_by(DataRegistroAnoMes, ManifestacaoFinalizada) %>% 
     summarise(Média = round(mean(Ndias), digits = 1)) %>% 
     pivot_wider(names_from = ManifestacaoFinalizada, values_from = Média)
colnames(MediaTempoFinalizadas_ARD) <- c("DataRegistroAnoMes","MediaFinalizadas","MediaNaoFinalizadas")


MensalAcumulado_ARD <- manifestacoes %>%
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     select(LocalidadeDemandaDesc, DataRegistroAnoMes, Ndias) %>% 
     group_by(DataRegistroAnoMes) %>% 
     count(LocalidadeDemandaDesc, name = "DemandasTotal") %>% 
     pivot_wider(names_from = LocalidadeDemandaDesc, values_from = DemandasTotal)
MensalAcumulado_ARD$`Barra Longa`[is.na(MensalAcumulado_ARD$`Barra Longa`)] <- 0
MensalAcumulado_ARD$`Rio Doce`[is.na(MensalAcumulado_ARD$`Rio Doce`)] <- 0
MensalAcumulado_ARD$`Santa Cruz do Escalvado`[is.na(MensalAcumulado_ARD$`Santa Cruz do Escalvado`)] <- 0
MensalAcumulado_ARD$`Barra Longa Acumulado` <- cumsum(MensalAcumulado_ARD$`Barra Longa`)
MensalAcumulado_ARD$`Rio Doce Acumulado` <- cumsum(MensalAcumulado_ARD$`Rio Doce`)
MensalAcumulado_ARD$`Santa Cruz do Escalvado Acumulado` <- cumsum(MensalAcumulado_ARD$`Santa Cruz do Escalvado`)
MensalAcumulado_ARD$`Rio Doce/ Santa Cruz do Escalavado Acumulado` <- cumsum(MensalAcumulado_ARD$`Rio Doce`) + cumsum(MensalAcumulado_ARD$`Santa Cruz do Escalvado`) 
MensalAcumulado_ARD$`Total Acumulado` <- cumsum(MensalAcumulado_ARD$`Barra Longa`) + 
     cumsum(MensalAcumulado_ARD$`Rio Doce`) +
     cumsum(MensalAcumulado_ARD$`Santa Cruz do Escalvado`)



### Cálculo de Tipos de manifestações
ManifestacoesStatus_ARD <- manifestacoes %>% 
     select(StatusManifestacao, LocalidadeDemandaDesc) %>% 
     filter(LocalidadeDemandaDesc %in% c("Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     count(StatusManifestacao, name = "Quantidade")

ManifestacoesStatus_ARD$StatusManifestacaoRecod <- ifelse(ManifestacoesStatus_ARD$StatusManifestacao %in%
                                                               c("Aguarda Conclusão do Atendimento",
                                                                 "Aguardando conclusão",
                                                                 "Cancelada",
                                                                 "Em tratamento",
                                                                 "Em tratamento para Canais de Relacionamento",
                                                                 "Em tratamento para elaboração de resposta escrita",
                                                                 "Em tratamento para resposta final",
                                                                 "Em tratamento para resposta intermediária"), "Não finalizada",
                                                          ifelse(ManifestacoesStatus_ARD$StatusManifestacao 
                                                                 %in% c("Respondida",
                                                                        "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                                 "Finalizada", "Finalizada no ato"))

ManifestacoesStatusGrupo_ARD <- ManifestacoesStatus_ARD %>%
     select(Quantidade, StatusManifestacaoRecod) %>% 
     group_by(StatusManifestacaoRecod) %>% 
     summarise(Total = sum(Quantidade))


setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202002")
write.xlsx(list(Geral = Geral_ARD,
                Assunto = Assunto_ARD,
                Assunto_30 = Assunto_ARD_30,
                Tema = Tema_ARD,
                Tema_30 = Tema_ARD_30,
                ManifestacoesStatus = ManifestacoesStatus_ARD,
                ManifestacoesStatusGrupo = ManifestacoesStatusGrupo_ARD,
                MediaTempoFinalizadas = MediaTempoFinalizadas_ARD,
                MensalAcumulado = MensalAcumulado_ARD,
                MensalFinalizadas = MensalFinalizadas_ARD),
           "RptMensal_ARD.xlsx", which = c("Geral",
                                           "Assunto",
                                           "Assunto_30",
                                           "Tema",
                                           "Tema_30",
                                           "ManifestacoesStatus",
                                           "ManifestacoesStatusGrupo",
                                           "MediaTempoFinalizadas",
                                           "MensalAcumulado",
                                           "MensalFinalizadas"),
           headerStyle = createStyle(halign = "center", textDecoration = "bold"),
           firstCol = TRUE, firstRow = TRUE, colWidths = "auto", withFilter = TRUE)

rm(Geral_ARD,
   Assunto_ARD,
   Assunto_ARD_30,
   Tema_ARD,
   Tema_ARD_30,
   ManifestacoesStatus_ARD,
   ManifestacoesStatusGrupo_ARD,
   MediaTempoFinalizadas_ARD,
   MensalAcumulado_ARD,
   MensalFinalizadas_ARD)




###########################################################################
# Demanda 16/03 -----------------------------------------------------------
###########################################################################

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
ggplot(cadastro, aes(x = fct_infreq(LocalidadeDemandaDesc))) +
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
ggplot(cadastro, aes(x = fct_infreq(LocalidadeDemandaDesc), fill = StatusManifestacao)) +
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
ggplot(cadastro, aes(x = fct_infreq(LocalidadeDemandaDesc), fill = StatusManifestacao)) +
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
     group_by(LocalidadeDemandaDesc, DataRegistroAnoMes) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = LocalidadeDemandaDesc, values_from = Total)
tabela1[is.na(tabela1)] <- 0


tabela2 <- cadastro %>% 
     group_by(DataRegistroAnoMes, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela2[is.na(tabela2)] <- 0


tabela3 <- cadastro %>% 
     group_by(LocalidadeDemandaDesc, StatusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = StatusManifestacao, values_from = Total)
tabela3[is.na(tabela3)] <- 0


tabela4 <- cadastro %>% 
     group_by(LocalidadeDemandaDesc, StatusManifestacao) %>% 
     summarise(Total = n())
tabela4[is.na(tabela4)] <- 0


write.xlsx(list(tabela1,
                tabela2,
                tabela3,
                tabela4), "SolicitacaoCadastro_Desde20180103.xlsx")
rm(cadastro, tabela1, tabela2, tabela3, tabela4)
