library(tidyverse)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
require(plotly, quietly = TRUE)



setwd("~/GitHub/dialogoFR")
source("filtro327.R", FALSE, encoding = "UTF-8")

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

manifestacoes <- filtro327[c(2, 3, 6, 21, 22, 70, 87, 1, 58, 59, 9, 44, 45, 46, 49, 82, 37, 72, 68)]


setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/RelatTerritorios")

# Canais de origem e território -------------------------------------------

### Origem das manifestações (canais)
png("ManifestacoesOrigemCanais.png", width = 1200, height = 600)
manifestacoes %>% 
   filter(territorio != "Não informado",
          datareg >= ymd("2015-11-05")) %>% 
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          FormaRecebimento = ifelse(FormaRecebimento %in% c("0800",
                                                            "Contato Ativo 0800/Fale Conosco"), "0800",
                                    ifelse(FormaRecebimento %in% c("Contato Ativo CIAs",
                                                                   "Postos de Atendimento CIAs"), "CIAs",
                                           ifelse(FormaRecebimento %in% c("Fale Conosco",
                                                                          "Fale Conosco - Portal"), "Portal do usuário",
                                                  ifelse(FormaRecebimento %in% c("Abordagem presencial",
                                                                                 "Mobilização",
                                                                                 "Reunião de Diálogo",
                                                                                 "Eventos"), "Diálogo", "Outros"))))) %>% 
   group_by(FormaRecebimento) %>% 
   summarise(Total = n()) %>% 
   ggplot(aes(x = FormaRecebimento, y = Total, fill = FormaRecebimento)) +
   geom_col() +
   labs(title = "Canais de origem das manifestações",
        subtitle = "Manifestações registradas desde 05/11/2015",
        x = "", y = "") +
   geom_text(aes(label = Total), position = "stack", vjust = -0.3, size = 3) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 12, vjust = 0.3, hjust = 1),
         axis.text.y = element_blank(),
         legend.position = "bottom",
         panel.grid.major.y = element_line(colour = "gray", size = .5))
dev.off()


png("ManifestacoesOrigemCanaisTerritorio.png", width = 1200, height = 600)
manifestacoes %>% 
   filter(territorio != "Não informado",
          datareg >= ymd("2015-11-05")) %>% 
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          FormaRecebimento = ifelse(FormaRecebimento %in% c("0800",
                                                            "Contato Ativo 0800/Fale Conosco"), "0800",
                                    ifelse(FormaRecebimento %in% c("Contato Ativo CIAs",
                                                                   "Postos de Atendimento CIAs"), "CIAs",
                                           ifelse(FormaRecebimento %in% c("Fale Conosco",
                                                                          "Fale Conosco - Portal"), "Portal do usuário",
                                                  ifelse(FormaRecebimento %in% c("Abordagem presencial",
                                                                                 "Mobilização",
                                                                                 "Reunião de Diálogo",
                                                                                 "Eventos"), "Diálogo", "Outros"))))) %>% 
   group_by(FormaRecebimento, territorio) %>% 
   summarise(Total = n()) %>% 
   ggplot(aes(x = FormaRecebimento, y = Total, fill = FormaRecebimento)) +
   geom_col() +
   labs(title = "Canais de origem das manifestações",
        subtitle = "Manifestações registradas desde 05/11/2015",
        x = "", y = "", fill = "") +
   geom_text(aes(label = Total), position = "stack", vjust = -0.3, size = 3) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 12, vjust = 0.3, hjust = 1),
         axis.text.y = element_blank(),
         legend.position = "bottom",
         panel.grid.major.y = element_line(colour = "gray", size = .5)) +
   facet_wrap( ~ territorio, nrow = 1, ncol = 6)
dev.off()


# Assuntos agrupados, status e território ---------------------------------

## Totais por Assunto agrupado
png("ManifestacoesAssuntoStatus.png", width = 1200, height = 600)
manifestacoes %>%
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          ManifestacaoAssunto = ifelse(ManifestacaoAssunto == "PG001 Levantamento e Cadastro", "PG01",
                                       ifelse(ManifestacaoAssunto == "PG002 Ressarcimento e Indenização", "PG02",
                                              ifelse(ManifestacaoAssunto == "Fundação Renova", "Fundação Renova",
                                                     ifelse(ManifestacaoAssunto == "PG021 Auxílio Financeiro Emergencial", "PG21", "Outros"))))) %>% 
   group_by(ManifestacaoAssunto, statusManifestacao) %>% 
   summarise(Total = n()) %>%
   ggplot(aes(x = ManifestacaoAssunto, y = Total, fill = statusManifestacao)) +
   geom_col() +
   labs(title = "Principais assuntos das manifestações, por status",
        x = "", y = "", fill = "") +
   geom_text(aes(label = Total), position = "stack", vjust = -0.4, size = 4) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 9, vjust = 0.5, hjust = 1),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         panel.background = element_blank())
dev.off()


png("ManifestacoesAssuntoStatusTerritorio.png", width = 1200, height = 600)
manifestacoes %>%
   filter(territorio != "Não informado") %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          ManifestacaoAssunto = ifelse(ManifestacaoAssunto == "PG001 Levantamento e Cadastro", "PG01",
                                       ifelse(ManifestacaoAssunto == "PG002 Ressarcimento e Indenização", "PG02",
                                              ifelse(ManifestacaoAssunto == "Fundação Renova", "Fundação Renova",
                                                     ifelse(ManifestacaoAssunto == "PG021 Auxílio Financeiro Emergencial", "PG21", "Outros"))))) %>% 
   group_by(ManifestacaoAssunto, statusManifestacao, territorio) %>% 
   summarise(Total = n()) %>%
   ggplot(aes(x = ManifestacaoAssunto, y = Total, fill = statusManifestacao)) +
   geom_col() +
   labs(title = "Principais assuntos das manifestações, por status",
        x = "", y = "", fill = "", fill = "") +
   # geom_text(aes(label = Total), position = "stack", vjust = -0.4, size = 4) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 9, vjust = 0.5, hjust = 1),
         axis.text.y = element_blank()) +
   facet_wrap( ~ territorio, nrow = 1, ncol = 7)
dev.off()




# Canais de origem, mês e território ------------------------------------

# Formas de recebimento da manifestação por mês (%)
png("ManifestacoesFormaRecebimentoMesPercentual.png", width = 1200, height = 600)
manifestacoes %>% 
   filter(territorio != "Não informado",
          datareg >= ymd("2015-11-05")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          FormaRecebimento = ifelse(FormaRecebimento %in% c("0800",
                                                            "Contato Ativo 0800/Fale Conosco"), "0800",
                                    ifelse(FormaRecebimento %in% c("Contato Ativo CIAs",
                                                                   "Postos de Atendimento CIAs"), "CIAs",
                                           ifelse(FormaRecebimento %in% c("Fale Conosco",
                                                                          "Fale Conosco - Portal"), "Portal do usuário",
                                                  ifelse(FormaRecebimento %in% c("Abordagem presencial",
                                                                                 "Mobilização",
                                                                                 "Reunião de Diálogo",
                                                                                 "Eventos"), "Diálogo", "Outros"))))) %>% 
   group_by(datareg, FormaRecebimento) %>% 
   summarise(Total = n()) %>% 
   mutate(Percentual = (Total/sum(Total)*100)) %>% 
   ggplot(aes(x = datareg, y = Percentual, fill = FormaRecebimento)) + 
   geom_col() +
   labs(title = "Formas de recebimento das manifestações, por mês",
        subtitle = "Percentual por canais de diálogo",
        x = "", y = "Percentual") +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())
dev.off()   


png("ManifestacoesFormaRecebimentoMesTotal.png", width = 1200, height = 600)
manifestacoes %>% 
   filter(territorio != "Não informado",
          datareg >= ymd("2015-11-05")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          FormaRecebimento = ifelse(FormaRecebimento %in% c("0800",
                                                            "Contato Ativo 0800/Fale Conosco"), "0800",
                                    ifelse(FormaRecebimento %in% c("Contato Ativo CIAs",
                                                                   "Postos de Atendimento CIAs"), "CIAs",
                                           ifelse(FormaRecebimento %in% c("Fale Conosco",
                                                                          "Fale Conosco - Portal"), "Portal do usuário",
                                                  ifelse(FormaRecebimento %in% c("Abordagem presencial",
                                                                                 "Mobilização",
                                                                                 "Reunião de Diálogo",
                                                                                 "Eventos"), "Diálogo", "Outros"))))) %>% 
   group_by(datareg, FormaRecebimento) %>% 
   summarise(Total = n()) %>% 
   mutate(Percentual = (Total/sum(Total)*100)) %>% 
   ggplot(aes(x = datareg, y = Total, fill = FormaRecebimento)) + 
   geom_col() +
   labs(title = "Formas de recebimento das manifestações, por mês",
        subtitle = "Total por canais de diálogo",
        x = "", y = "Total") +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())
dev.off()   



# Registradas por mês, status e território --------------------------------------
png("ManifestacoesMesStatus_DataRegistro.png", width = 1200, height = 600)
manifestacoes %>%
   filter(datareg >= ymd("2015-11-03"),
          territorio != "Não informado") %>% 
   mutate(datareg = format(datareg, "%Y/%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao != "Cancelada") %>% 
   group_by(datareg, statusManifestacao) %>% 
   summarise(Média = round(mean(ndias), digits = 1),
             Total = n()) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total, fill = statusManifestacao)) +
   labs(title = "Manifestações registradas por mês", subtitle = "Finalizadas e não finalizadas",
        x = "", y = "", fill = "Status da manifestação") + 
   scale_y_continuous(breaks = seq(0, 30000, 5000)) +
   # scale_x_date(date_labels = "%m/%d", date_breaks = "2 weeks") +
   scale_fill_manual(name = "Status", values = c("#3d800e", "#b81a2c"), labels = c("Finalizada", "Não finalizada")) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         legend.position = "bottom",
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())
dev.off()


png("ManifestacoesMesStatus_DataRegistroTerritorio.png", width = 1200, height = 1800)
manifestacoes %>% 
   filter(datareg >= ymd("2015-11-03"),
          territorio != "Não informado") %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   group_by(datareg, statusManifestacao, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = datareg, y = Total, fill = statusManifestacao)) +
   geom_col() +
   labs(title = "Manifestações registradas, por mês e status",
        x = "", y = "", fill = "Status da manifestação") +
   scale_y_continuous(breaks = seq(0, 30000, 6000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank(),
         legend.position = "bottom")+
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)
dev.off()



# Concluídas por mês, status e território ---------------------------------
png("ManifestacoesConcluidas_MesConclusao.png", width = 1200, height = 600)
manifestacoes %>% 
   filter(dataconclusao >= ymd("2015-11-03") & dataconclusao <= Sys.Date() & !is.na(dataconclusao),
          territorio != "Não informado") %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = dataconclusao, y = Total)) +
   geom_col() +
   labs(title = "Manifestações concluídas, por mês de conclusão",
        x = "", y = "") +
   scale_y_continuous(breaks = seq(0, 30000, 6000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank(),
         legend.position = "bottom")
dev.off()


png("ManifestacoesConcluidas_MesConclusaoTerritorio.png", width = 1200, height = 1800)
manifestacoes %>% 
   filter(dataconclusao >= ymd("2015-11-03") &
             dataconclusao <= Sys.Date() &
             !is.na(dataconclusao),
          territorio != "Não informado") %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = dataconclusao, y = Total)) +
   geom_col() +
   labs(title = "Manifestações concluídas, por mês de conclusão",
        x = "", y = "") +
   scale_y_continuous(breaks = seq(0, 30000, 6000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank(),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)
dev.off()





## Não finalizadas, por mês de registro e território
png("ManifestacoesNaoFinalizadas_MesRegistro.png", width = 1200, height = 600)
manifestacoes %>%
   filter(datareg >= ymd("2015-11-05")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Não finalizada") %>% 
   group_by(datareg, ManifestacaoAssunto, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total)) +
   labs(title = "Manifestações não finalizadas, por mês de registro",
        x = "", y = "") +
   # scale_y_continuous(breaks = seq(0, 60000, 5000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())
dev.off()



png("ManifestacoesNaoFinalizadas_MesRegistroTerritorio.png", width = 1200, height = 1800)
manifestacoes %>%
   filter(datareg >= ymd("2015-11-05")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Não finalizada") %>% 
   group_by(datareg, ManifestacaoAssunto, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total)) +
   labs(title = "Manifestações não finalizadas, por mês de registro",
        x = "", y = "") +
   # scale_y_continuous(breaks = seq(0, 60000, 5000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank()) +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)
dev.off()



### Cálculo de Tipos de manifestações
# Cria resumo com média de dias, máximo de dias e total de manifestações por status da manifestação (finalizada e não finalizada)
manifestacoes %>% 
   mutate(StatusManifestacao = ifelse(statusManifestacao %in%
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
   group_by(StatusManifestacao) %>% 
   summarise(Quantidade = n(),
             Média = round(mean(ndias), digits = 1),
             Mínimo = min(ndias))



# Tempo médio de finalização, mês de finalização --------------------------
png("ManifestacoesFinalizadasTempoMedio.png", width = 1200, height = 600)
manifestacoes %>%
   filter(!is.na(dataconclusao) & dataconclusao > ymd("2015-11-05") & dataconclusao < ymd("2020-12-31"),
          territorio !=  "Não informado") %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>%
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   mutate(Tempo = as.factor(ifelse(Média <  15, 1,
                                   ifelse(Média >= 15 & Média < 27, 2,
                                          ifelse(Média >= 27 & Média < 77, 3, 4))))) %>% 
   ggplot(aes(x = dataconclusao, y = Total, fill = Tempo)) +
   geom_col() +
   labs(title = "Número e tempo médio de finalização de manifestações,\npor mês de finalização", x = "", y = "") + 
   scale_fill_manual(labels = c("até 15 dias", "15 a 27 dias", "27 a 77 dias", "Mais de 77 dias"),
                     values = c('darkgreen', 'darkblue', 'darkorange', 'darkred')) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         legend.position = "bottom", panel.background = element_blank())
dev.off()


png("ManifestacoesFinalizadasTempoMedioTerritorio.png", width = 1200, height = 1800)
manifestacoes %>%
   filter(!is.na(dataconclusao) & dataconclusao > ymd("2015-11-05") & dataconclusao < ymd("2020-12-31"),
          territorio !=  "Não informado") %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>%
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   mutate(Tempo = as.factor(ifelse(Média <  15, 1,
                                   ifelse(Média >= 15 & Média < 27, 2,
                                          ifelse(Média >= 27 & Média < 77, 3, 4))))) %>% 
   ggplot(aes(x = dataconclusao, y = Total, fill = Tempo)) +
   geom_col() +
   labs(title = "Número e tempo médio de finalização de manifestações,\npor mês de finalização", x = "", y = "") + 
   scale_fill_manual(labels = c("até 15 dias", "15 a 27 dias", "27 a 77 dias", "Mais de 77 dias"),
                     values = c('darkgreen', 'darkblue', 'darkorange', 'darkred')) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         legend.position = "bottom", panel.background = element_blank()) +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)
dev.off()



# Tempo médio de finalização, por grupos e mês de registro ----------------
png("ManifestacoesFinalizadasTempoMedio_MesRegistro.png", width = 1200, height = 600)
manifestacoes %>%
   filter(datareg > ymd("2015-11-05") & datareg < ymd("2020-12-31")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>%
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(datareg, statusManifestacao) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   mutate(Tempo = as.factor(ifelse(Média < 15, 1,
                                   ifelse(Média >= 15 & Média < 27, 2,
                                          ifelse(Média >= 27 & Média < 77, 3, 4))))) %>% 
   ggplot(aes(x = datareg, y = Média, fill = Tempo)) +
   geom_col() +
   labs(title = "Número e tempo médio de finalização de manifestações,\npor mês de registro", x = "", y = "") + 
   scale_fill_manual(labels = c("até 15 dias", "15 a 27 dias", "27 a 77 dias", "Mais de 77 dias"),
                     values = c('darkgreen', 'darkblue', 'darkorange', 'darkred')) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         legend.position = "bottom", panel.background = element_blank())
dev.off()


# Tempo médio de finalização, por grupos e mês de conclusão ----------------
png("ManifestacoesFinalizadasTempoMedio_MesFinalizacao.png", width = 1200, height = 600)
manifestacoes %>%
   filter(dataconclusao > ymd("2015-11-05") & dataconclusao < ymd("2020-12-31"),
          !is.na(dataconclusao)) %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>%
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao, statusManifestacao) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   mutate(Tempo = as.factor(ifelse(Média < 15, 1,
                                   ifelse(Média >= 15 & Média < 27, 2,
                                          ifelse(Média >= 27 & Média < 77, 3, 4))))) %>% 
   ggplot(aes(x = dataconclusao, y = Média, fill = Tempo)) +
   geom_col() +
   labs(title = "Número e tempo médio de finalização de manifestações,\npor mês de finalização", x = "", y = "") + 
   scale_fill_manual(labels = c("até 15 dias", "15 a 27 dias", "27 a 77 dias", "Mais de 77 dias"),
                     values = c('darkgreen', 'darkblue', 'darkorange', 'darkred')) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         legend.position = "bottom", panel.background = element_blank())
dev.off()


# Demandas por status -----------------------------------------------------
png("DemandasStatus.png", width = 1200, height = 600)
manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(statusdemanda) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot() +
   geom_col(aes(x = statusdemanda, y = Total, fill = statusdemanda)) +
   labs(title = "Demandas por status",
        x = "", y = "", fill = "Status da demanda") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_blank(),
         legend.position = "bottom", panel.background = element_blank())
   
dev.off()


png("DemandasStatusTerritorio.png", width = 1200, height = 600)
manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda),
          territorio != "Não informado") %>% 
   group_by(statusdemanda, territorio) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot() +
   geom_col(aes(x = statusdemanda, y = Total, fill = statusdemanda)) +
   labs(title = "Demandas por status",
        x = "", y = "", fill = "Status da demanda") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_blank(),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 1, ncol = 7)
dev.off()




# Demandas por status, mês ------------------------------------------------
png("DemandasStatusMes.png", width = 1200, height = 600)
manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda),
          !is.na(ultimoencaData)) %>% 
   group_by(statusdemanda, ultimoencaData) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot() +
   geom_col(aes(x = ultimoencaData, y = Total, fill = statusdemanda)) +
   labs(title = "Demandas por status",
        subtitle = "Data do último encaminhamento",
        x = "", y = "", fill = "") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         legend.position = "bottom")
dev.off()


png("DemandasStatusMesTerritorio.png", width = 1200, height = 1800)
manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda),
          !is.na(ultimoencaData),
          territorio != "Não informado") %>% 
   group_by(statusdemanda, ultimoencaData, territorio) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot() +
   geom_col(aes(x = ultimoencaData, y = Total, fill = statusdemanda)) +
   labs(title = "Demandas por status",
        subtitle = "Data do último encaminhamento",
        x = "", y = "", fill = "") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)
dev.off()


# Análise de cenário ------------------------------------------------------

### Manifestações
## 1.Total de manifestações registradas de nov/15 até 31/03
manifestacoes %>% 
   mutate(MesAtual = ifelse(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31"), 1, 0)) %>% 
   group_by(territorio) %>% 
   summarise(Total = n(),
             MesAtual = sum(MesAtual))


manifestacoes %>% 
   group_by(territorio) %>% 
   summarise(Total = n())



## 2. Total de manifestações registradas nos três últimos meses
#Geral
manifestacoes %>% 
   filter(datareg >= ymd("2019-10-01") & datareg <= ymd("2020-03-31")) %>% 
   mutate(datareg = format(datareg, "%Y-%m")) %>% 
   group_by(datareg) %>% 
   summarise(Total = n())


# Território (apenas o último mês)
manifestacoes %>% 
   filter(datareg >= ymd("2020-03-01") & datareg <= ymd("2020-03-31")) %>% 
   mutate(datareg = format(datareg, "%b-%Y")) %>% 
   group_by(datareg, territorio) %>% 
   summarise(Total = n()) %>% 
   pivot_wider(names_from = datareg, values_from = Total)



## 3. Status de todas as manifestações (nov/15 até 31/03) – status simplificado (respondidas x não respondidas)
# Geral
manifestacoes %>% 
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   group_by(statusManifestacao) %>% 
   summarise(Total = n()) %>% 
   mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# Território
manifestacoes %>% 
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Respondida", "Não respondida")) %>% 
   group_by(territorio, statusManifestacao, ManifestacaoAssunto) %>% 
   summarise(Total = n()) %>% 
   mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
   pivot_wider(names_from = statusManifestacao, values_from = c(Total, Percentual))



## 4. Quantidade e % de manifestações por assunto (PG) no último mês (março/20)
# Geral
manifestacoes %>% mutate(Assunto = str_replace_all(ManifestacaoAssunto, c("PG001 Levantamento e Cadastro" = "PG01",
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
manifestacoes %>% 
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
manifestacoes %>% 
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
manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   summarise(Total = n())


# Território
manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(territorio) %>% 
   summarise(Total = n())



## 2. Status de todas as demandas individuais (nov/15 até 31/03)
# Geral
manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(statusdemanda) %>% 
   summarise(Total = n()) %>% 
   mutate(Percentual = round(prop.table(Total), digits = 3)*100)


# Território
D2Territorio <- manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(statusdemanda, territorio) %>% 
   summarise(Total = n()) %>% 
   pivot_wider(names_from = territorio, values_from = Total, values_fill = list(Total = 0))




## 3. Quantidade de demandas individuais por assunto (PG) e status (nov/15 até 31/03)
# Geral
D3Geral <- manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(statusdemanda, ManifestacaoAssunto) %>% 
   summarise(Total = n()) %>% 
   # mutate(Percentual = round(prop.table(Total), digits = 3)*100) %>% 
   pivot_wider(names_from = statusdemanda, values_from = c(Total), values_fill = list(Total = 0))


# Território
D3Territorio <- manifestacoes %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(territorio, statusdemanda, ManifestacaoAssunto) %>% 
   summarise(Total = n()) %>% 
   pivot_wider(names_from = statusdemanda, values_from = c(Total), values_fill = list(Total = 0))

