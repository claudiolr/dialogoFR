library(tidyverse)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
require(plotly, quietly = TRUE)


options(scipen = 999) # desativa a notação científica

# com o filtro327 carregado e tratado, escolha as colunas listadas abaixo

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

dlg_form((list = c(seq(1, length(filtro327)))), title = "Fill the form")

# Canais de origem e território -------------------------------------------

### Origem das manifestações (canais)

manifestacoes %>% 
   filter(datareg >= ymd("2015-11-05")) %>% 
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
   geom_text(aes(label = Total), position = "stack", vjust = -0.4, size = 3) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         axis.text.x = element_text(angle = 0, size = 12, vjust = 0.5, hjust = 0.5),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),
         legend.position = "none")



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
   ggplot(aes(x = FormaRecebimento, fill = FormaRecebimento)) +
   geom_bar() +
   labs(title = "Canais de origem das manifestações",
        subtitle = "Manifestações registradas desde 05/11/2015",
        x = "", y = "", fill = "") +
   geom_text(stat = "count", aes(label = ..count..), position = "stack", vjust = -0.3, size = 3) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         axis.text.x = element_blank(), axis.text.y = element_blank(),
         axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),
         legend.position = "bottom", strip.background = element_rect(fill = "darkgray")) +
   facet_wrap( ~ territorio, nrow = 1, ncol = 6)



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
   ggplot(aes(x = territorio, fill = FormaRecebimento)) +
   geom_bar(position = "fill") +
   labs(title = "Canais de origem das manifestações",
        subtitle = "Manifestações registradas desde 05/11/2015",
        x = "", y = "", fill = "") +
   geom_text(stat = "count", aes(label = ..count..), position =  "fill", size = 3) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 10, hjust = 0.5),
         axis.text.x = element_blank(), axis.text.y = element_blank(),
         axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),
         legend.position = "bottom", strip.background = element_rect(fill = "darkgray")) +
facet_wrap( ~ territorio, nrow = 1, ncol = 6)



# Assuntos agrupados, status e território ---------------------------------

## Totais por Assunto agrupado N
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
   ggplot(aes(x = ManifestacaoAssunto, fill = statusManifestacao)) +
   geom_bar(position = "stack") +
   labs(title = "Principais assuntos das manifestações, por status",
        x = "", y = "", fill = "") +
   geom_text(stat = "count", aes(label = ..count..), position = "stack", vjust = -0.4, size = 4) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 0, size = 9, vjust = 0.5, hjust = 0.5),
         axis.text.y = element_blank(), axis.ticks = element_blank())



## Totais por Assunto agrupado %
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
   mutate(Prop = round(Total/sum(Total), digits = 3)*100) %>%
   ggplot(aes(x = ManifestacaoAssunto, y = Prop, fill = statusManifestacao, label = Prop)) +
   geom_col() +
   geom_label(aes(y = Prop), colour = "white", fontface = "bold", position = position_stack(vjust = 0.5)) + 
   labs(title = "Principais assuntos das manifestações, por status",
        x = "", y = "", fill = "") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5, hjust = 0.5, face = "bold"),
         axis.text.y = element_blank(), axis.ticks = element_blank())



## Totais por Assunto agrupado por território
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
         axis.text.y = element_blank(),
         axis.ticks = element_blank()) +
   facet_wrap( ~ territorio, nrow = 1, ncol = 7)




# Canais de origem, mês e território ------------------------------------

# Formas de recebimento da manifestação por mês (%)

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
        x = "", y = "Percentual", fill = "Forma de recebimento da manifestação") +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5),
         axis.ticks = element_blank(), panel.background = element_blank())
   




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
        x = "", y = "") +
   scale_y_continuous(breaks = seq(0, 30000, 2500)) +
   theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_blank(),
         panel.grid.major.y = element_line(colour = "lightgray", size = .3, linetype = 3),
         panel.background = element_blank())




# Registradas por mês, status e território --------------------------------------

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
   scale_y_continuous(breaks = seq(0, 30000, 2500)) +
   # scale_x_date(date_labels = "%m/%d", date_breaks = "2 weeks") +
   scale_fill_manual(name = "Status", values = c("#3d800e", "#b81a2c"), labels = c("Finalizada", "Não finalizada")) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_blank(),
         legend.position = "bottom",
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank())



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
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank(),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)



# Concluídas por mês, status e território ---------------------------------

manifestacoes %>% 
   filter(dataconclusao >= ymd("2015-11-03") & dataconclusao <= Sys.Date() & !is.na(dataconclusao),
          territorio != "Não informado") %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Finalizada") %>% 
   group_by(dataconclusao) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = dataconclusao, y = Total)) +
   geom_col() +
   labs(title = "Manifestações concluídas, por mês de conclusão",
        x = "", y = "") +
   scale_y_continuous(breaks = seq(0, 33000, 3000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_blank(),
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank(),
         legend.position = "bottom")



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
         axis.ticks = element_blank(),
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank(),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)




## Não finalizadas, por mês de registro e território

manifestacoes %>%
   filter(datareg >= ymd("2015-11-05")) %>% 
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Não finalizada",
          is.na(dataconclusao)) %>% 
   group_by(datareg) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total, fill = Média)) +
   labs(title = "Manifestações não finalizadas, por mês de registro",
        x = "", y = "") +
   scale_fill_gradient(low = "darkgreen", high = "darkred", na.value = NA) +
   scale_y_continuous(name = "Tempo de resolução (em dias)", breaks = seq(0, 5000, 500)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_blank(),
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank())




manifestacoes %>%
   filter(datareg >= ymd("2015-11-05")) %>% 
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Não finalizada") %>% 
   group_by(datareg, territorio) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total, fill = Média)) +
   labs(title = "Manifestações não finalizadas, por mês de registro e território",
        x = "", y = "") +
   scale_fill_gradient(low = "darkgreen", high = "darkred", na.value = NA) +
   # scale_y_continuous(breaks = seq(0, 60000, 5000)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_blank(),
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         panel.background = element_blank(),
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 7, ncol = 1)




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
             Mínimo = min(ndias)) %>% 
   plotly::plot_ly(labels = ~StatusManifestacao, values = ~Quantidade, type = "pie")  %>% 
   add_pie(hole = 0.3)


   


# Tempo médio de finalização, mês de finalização --------------------------

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
   ggplot(aes(x = dataconclusao, y = Total, fill = Média)) +
   geom_col() +
   labs(title = "Número e tempo médio de finalização de manifestações,\npor mês de finalização", x = "", y = "") + 
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "lightgray", size = .1, linetype = 3),
         legend.position = "bottom", panel.background = element_blank())




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




# Tempo médio de finalização, por grupos e mês de registro ----------------

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



# Tempo médio de finalização, por grupos e mês de conclusão ----------------

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



# Demandas por status -----------------------------------------------------

manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda)) %>% 
   group_by(statusdemanda) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot(aes(x = statusdemanda, y = Total, fill = statusdemanda, label = Total)) +
   geom_col(position = position_stack()) +
   geom_label(aes(y = Total), colour = "white", fontface = "bold", position = position_stack(vjust = 0.5)) + 
   labs(title = "Demandas por status",
        x = "", y = "", fill = "Status da demanda") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_blank(), axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         # legend.box = element_rect()
         legend.position = "bottom", panel.background = element_blank())
   


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
         axis.ticks = element_blank(),
         
         legend.position = "bottom") +
   facet_wrap( ~ territorio, nrow = 1, ncol = 7)





# Demandas por status, mês ------------------------------------------------
manifestacoes %>% 
   mutate(ultimoencaData = format(ultimoencaData, "%Y-%m")) %>% 
   filter(!is.na(statusdemanda),
          !is.na(ultimoencaData)) %>% 
   group_by(statusdemanda, ultimoencaData) %>% 
   summarise(Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ggplot() +
   geom_col(aes(x = ultimoencaData, y = Total, fill = statusdemanda)) +
   scale_y_continuous(breaks = seq(0, 3000, 500)) +
   labs(title = "Demandas por status",
        subtitle = "Data do último encaminhamento",
        x = "", y = "", fill = "") +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks = element_blank(),
         legend.box.margin = margin(),
         legend.position = "bottom")



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




manifestacoes %>%
   filter(dataconclusao > ymd("2015-11-05") & !is.na(dataconclusao) & dataconclusao <= Sys.Date(),
          datareg > ymd("2015-11-05") & datareg <= Sys.Date()) %>% 
   group_by(datareg, dataconclusao, ManifestacaoTema) %>%
   summarise(N = n()) %>% 
   # summarise(Total = n(),
   #           Média = mean(ndias)) %>% 
   ggplot(aes(x = datareg, y = dataconclusao)) +
   geom_point(aes(fill = ManifestacaoTema)) +
   theme(legend.position = "none")



manifestacoes %>%
   filter(dataconclusao > ymd("2015-11-05") & !is.na(dataconclusao) & dataconclusao <= Sys.Date(),
          datareg > ymd("2015-11-05") & datareg <= Sys.Date()) %>% 
   group_by(datareg, dataconclusao, ManifestacaoTema) %>%
   summarise(N = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = Média)) +
   geom_histogram()
   
   
   
   
   
   
   
   