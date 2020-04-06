library(tidyverse)
library(openxlsx)
library(lubridate)
library(RColorBrewer)

source("filtro327.R", TRUE, encoding = "UTF-8")

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
# [28] "C6manifrelgqtpoeira"            [29] "C7manifamFecmasocila"        [30] "manifestacaoOrigem"
# [31] "resumo"                         [32] "dataConclusao"               [33] "resumoconclusao"
# [34] "requerAcaoFutura"               [35] "manifestacaoSubtema"         [36] "responsavelPelaDemanda"
# [37] "StatusDemanda"                  [38] "UltimoEncaminhamento"        [39] "ultimoEncaData"
# [40] "pultimoEncaDE"                  [41] "ultimoEncaPARA"              [42] "DATAHORA_EXP"
# [43] "DataRegistroAnoMes"             [44] "DataConclusaoAnoMes"         [45] "ManifestacaoFinalizada"

# manifestacoes <- filtro1332[c(1, 2, 8, 45, 9, 43, 32, 44, 12, 30, 13, 14, 35, 15, 17)]

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

manifestacoes <- filtro327[c(2, 3, 6, 1, 58, 59, 9, 45, 46, 49, 68, 22, 70)]


### Por mês/ano do registro
# Cria resumo com média de dias, máximo de dias e total de manifestações por mês/ano do registro da manifestação
manifestacoes %>%
   mutate(datareg = format(datareg, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao != "Cancelada") %>% 
   group_by(datareg, statusManifestacao) %>% 
   summarise(Média = round(mean(ndias), digits = 1),
             Total = n()) %>% 
   ggplot() +
   geom_col(aes(x = datareg, y = Total, fill = statusManifestacao)) +
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
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   filter(statusManifestacao == "Finalizada",
          !is.na(dataconclusao)) %>% 
   group_by(dataconclusao, statusManifestacao) %>% 
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


### Assunto
# Cria resumo com média de dias, máximo de dias e total de manifestações por assunto
manifestacoes %>%
   mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
          statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     group_by(ManifestacaoAssunto, statusManifestacao) %>% 
     summarise(Média = round(mean(ndias, na.rm = TRUE), digits = 1),
               Máximo = max(ndias),
               Total = n()) %>% 
   ggplot(aes(x = ManifestacaoAssunto, y = Total, fill = statusManifestacao)) +
             geom_col() +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         legend.position = "bottom",
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())


### Por Tema
# Cria resumo com média de dias, máximo de dias e total de manifestações por tema
manifestacoes %>%
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   group_by(ManifestacaoAssunto, ManifestacaoTema, statusManifestacao) %>% 
   summarise(Média = round(mean(ndias), digits = 1),
             Máximo = max(ndias),
             Total = n()) %>% 
   pivot_wider(names_from = statusManifestacao, values_from = c(Média, Máximo, Total), values_fill = list(Média = 0, Máximo = 0, Total = 0))


### Por Canais
# Cria resumo com média de dias, máximo de dias e total de manifestações por canais
manifestacoes %>%
   mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                "Respondida no ato"), "Finalizada", "Não finalizada"),
          datareg = format(datareg, "%Y-%m")) %>% 
   group_by(FormaRecebimento, datareg) %>% 
   summarise(Média = round(mean(ndias), digits = 1),
             Máximo = max(ndias),
             Total = n()) %>% 
   ggplot(aes(x = FormaRecebimento, y = Total, fill = Média)) +
   geom_col()



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



### Por território e origem da demanda
# Por território
manifestacoes %>% 
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
   group_by(territorio, FormaRecebimento) %>% 
   summarise(Total = n()) %>% 
   ggplot(aes(x = territorio, y = ..count..), fill = FormaRecebimento) +
   geom_bar()


     
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




# Gráficos ----------------------------------------------------------------

manifestacoes %>% 
     mutate(datareg = format(datareg, "%Y-%m"),
            statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                         "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                         "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
   summarise(Total = n(),
             Média = mean(ndias)) %>% 
   ggplot(aes(x = datareg, y = Total, fill = statusManifestacao)) +
   geom_col() +
   labs(title = "Dias decorridos desde \n o registro da manifestação",
        x = "Mês/ano", y = "Dias") +
   scale_y_continuous(breaks = seq(0, 100, 10)) +
   theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
         axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
         axis.ticks.y = element_line(colour = "gray", size = .5),
         panel.grid.major.y = element_line(colour = "gray", size = .5),
         panel.background = element_blank())


manifestacoes %>%
     mutate(dataconclusao = format(dataconclusao, "%Y-%m"),
            statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     filter(statusManifestacao != "Cancelada" & !is.na(statusManifestacao),
            !is.na(dataconclusao)) %>% 
     ggplot() +
     geom_bar(aes(x = dataconclusao, fill = statusManifestacao)) +
     labs(title = "Dias decorridos desde \n o registro da manifestação",
          x = "Mês/ano", y = "Dias") +
     scale_y_continuous(breaks = seq(0, 60000, 5000)) +
     theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5),
           axis.ticks.y = element_line(colour = "gray", size = .5),
           panel.grid.major.y = element_line(colour = "gray", size = .5),
           panel.background = element_blank())


# Número e tempo médio de finalização de manifestações, por mês de finalização
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


# Número e tempo médio de finalização de manifestações, por mês de finalização
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

