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


# Por territórios específicos ---------------------------------------------

# 1. Mariana: MRN
# 2. Alto Rio Doce: ARD
# 3. Calha do Rio Doce: CRD
# 4. Médio Rio Doce: MRD
# 5. Baixo Rio Doce: BRD
# 6. Foz do Rio Doce: FRD


### Cabeçalho do relatório
TotalGeral <- manifestacoes %>%
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     group_by(localidadedemandadesc, statusManifestacao) %>%
     summarise(Total = n()) %>% 
     pivot_wider(names_from = statusManifestacao, values_from = Total) %>% 
     mutate(Total = sum(Finalizada, `Não finalizada`)) %>% 
     mutate(`Taxa de Finalização` = (Finalizada/Total)*100)


TotalMesAtual <- manifestacoes %>%
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado"),
            datareg >= "2020-02-01" & datareg <= "2020-03-31") %>% 
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     group_by(localidadedemandadesc, datareg) %>%
     summarise(DemandasMes = n()) %>% 
     pivot_wider(names_from = datareg, values_from = DemandasMes, values_fill = list(DemandasMes = 0))


Geral <- merge(TotalMesAtual, TotalGeral, by = "localidadedemandadesc")
rm(TotalMesAtual, TotalGeral)


### Cálculo de Tipos de manifestações (pizza)
ManifestacoesStatus <- manifestacoes %>% 
     mutate(statusManifestacao = ifelse(statusManifestacao %in%
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
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(statusManifestacao) %>% 
     summarise(Total = n())


### Página 1 (segunda parte)
Mensal <- manifestacoes %>%
     mutate(datareg = format(datareg, "%Y-%m")) %>% 
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>%
     group_by(datareg, localidadedemandadesc) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = localidadedemandadesc, values_from = c(Total), values_fill = list(Total = 0)) %>% 
     mutate(Total = sum(`Barra Longa`, `Rio Doce`, `Santa Cruz do Escalvado`, na.rm = TRUE))


TempoResolucao <- manifestacoes %>%
     mutate(datareg = format(datareg, "%Y-%m"),
            statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado"),
            statusManifestacao == "Finalizada") %>% 
     group_by(datareg) %>% 
     summarise(Média = round(mean(ndias), digits = 1))


StatusMes <- manifestacoes %>% 
     mutate(datareg = format(datareg, "%Y-%m"),
            statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(datareg, statusManifestacao) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = statusManifestacao, values_from = Total, values_fill = list(Total = 0))



### Página 2
AssuntoTema <- manifestacoes %>%
     mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                  "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                  "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
     filter(localidadedemandadesc %in% c("Mariana", "Barra Longa", "Rio Doce", "Santa Cruz do Escalvado")) %>% 
     group_by(ManifestacaoAssunto, ManifestacaoTema, statusManifestacao) %>% 
     summarise(Total = n(),
               Média = round(mean(ndias), digits = 1),
               Máximo = max(ndias)) %>% 
     pivot_wider(names_from = statusManifestacao, values_from = c(Total, Média, Máximo)) %>% 
     mutate("Total de manifestações" = sum(Total_Finalizada, `Total_Não finalizada`, na.rm = TRUE)) %>% 
     filter(`Total de manifestações` > 30) %>%
     arrange(ManifestacaoAssunto, desc(`Total de manifestações`))



### Salva os cálculos
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202003")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes/Relatorios/ReportMensal/Relat202003")
write.xlsx(list(Cabeçalho = Geral,
                ManifestacoesStatus = ManifestacoesStatus,
                StatusMes = StatusMes,
                MensalRegistro = Mensal,
                TempoMedioResolucao = TempoResolucao,
                AssuntoTema = AssuntoTema),
           "RptMensal_ARD_V2.xlsx",
           headerStyle = createStyle(halign = "center", textDecoration = "bold"),
           firstCol = TRUE, firstRow = TRUE, colWidths = "auto", withFilter = TRUE)

rm(Geral,
   ManifestacoesStatus,
   StatusMes,
   Mensal,
   TempoResolucao,
   AssuntoTema)
