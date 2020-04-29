

relatorioMensal <- function() {
   
   if(!require(tidyverse)) {
      install.packages("tidyverse")
      library(tidyverse)
   }
   if(!require(openxlsx)) {
      install.packages("openxlsx")
      library(openxlsx)
   }
   if(!require(lubridate)) {
      install.packages("lubridate")
      library(lubridate)
   }
   if(!require(knitr)) {
      install.packages("knitr")
      library(knitr)    
   }
   if(!require(svDialogs)) {
      install.packages("svDialogs")
      library(svDialogs)
   }
   
   options(scipen = 999) # desativa a notação científica
   
   
   # Escolha das colunas a partir do filtro utilizado ------------------------
   
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
   
   manifestacoes <- filtro327[c(2, 3, 6, 1, 58, 59, 9, 45, 46, 49, 68, 22, 70)]
   
   
   # Por territórios específicos ---------------------------------------------
   
   # 1. Mariana: MRN
   # 2. Alto Rio Doce: ARD
   # 3. Calha do Rio Doce: CRD
   # 4. Médio Rio Doce: MRD
   # 5. Baixo Rio Doce: BRD
   # 6. Foz do Rio Doce: FRD
   
   dlgMessage("A seguir, defina o periodo que sera considerado para as analises recentes")
   periodoAnalise <- seq.Date(from = dmy(dlgInput("Defina a data de INICIO no formato dd-mm-aaaa (sem aspas)")$res),
                              to = dmy(dlgInput("Defina a data de FIM no formato dd-mm-aaaa (sem aspas)")$res), by = "day")
   
   municipios <- list("6" = "Acaiaca", "38" = "Aimorés", "31" = "Alpercata", "44" = "Aracruz",
                      "39" = "Baixo Guandu", "3" = "Barra Longa", "28" = "Belo Oriente",
                      "18" = "Bom Jesus do Galho", "24" = "Bugre", "19" = "Caratinga",
                      "40" = "Colatina", "48" = "Conceição da Barra", "35" = "Conselheiro Pena",
                      "16" = "Córrego Novo", "11" = "Dionísio", "27" = "Fernandes Tourinho",
                      "47" = "Fundão", "33" = "Galileia", "32" = "Governador Valadares",
                      "25" = "Iapu", "23" = "Ipaba", "21" = "Ipatinga", "37" = "Itueta",
                      "43" = "Linhares", "1" = "Mariana", "41" = "Marilândia", "13" = "Marliéria",
                      "29" = "Naque", "2" = "Ouro Preto", "52" = "Outros ES", "51" = "Outros MG",
                      "42" = "Pancas", "30" = "Periquito", "17" = "Pingo D’Água", "7" = "Ponte Nova",
                      "15" = "Raul Soares", "36" = "Resplendor", "9" = "Rio Casca", "4" = "Rio Doce",
                      "5" = "Santa Cruz do Escalvado", "22" = "Santana do Paraíso", "10" = "São Domingos do Prata",
                      "12" = "São José do Goiabal", "45" = "São Mateus", "14" = "São Pedro dos Ferros",
                      "8" = "Sem-Peixe", "46" = "Serra", "26" = "Sobrália", "50" = "Sooretama",
                      "20" = "Timóteo", "53" = "Todos os municípios", "34" = "Tumiritinga", "49" = "Vitória")
   
   
   municipiosEscolhidos <- dlgList(municipios, "Escolha os municipios", multiple = TRUE)$res
   
   
   ### Cabeçalho do relatório
   TotalGeral <- manifestacoes %>%
      filter(localidadedemandadesc %in% municipiosEscolhidos) %>% 
      mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
      group_by(localidadedemandadesc, statusManifestacao) %>%
      summarise(Total = n()) %>% 
      pivot_wider(names_from = statusManifestacao, values_from = Total) %>% 
      mutate(Total = sum(Finalizada, `Não finalizada`)) %>% 
      mutate(`Taxa de Finalização` = (Finalizada/Total)*100)
   
   
   TotalMesAtual <- manifestacoes %>%
      filter(localidadedemandadesc %in% municipiosEscolhidos,
             datareg %in% periodoAnalise) %>% 
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
      filter(localidadedemandadesc %in% municipiosEscolhidos) %>% 
      group_by(statusManifestacao) %>% 
      summarise(Total = n())
   
   
   ### Página 1 (segunda parte)
   Mensal <- manifestacoes %>%
      mutate(datareg = format(datareg, "%Y-%m")) %>% 
      filter(localidadedemandadesc %in% municipiosEscolhidos) %>%
      group_by(datareg, localidadedemandadesc) %>% 
      summarise(Total = n()) %>% 
      pivot_wider(names_from = localidadedemandadesc, values_from = c(Total), values_fill = list(Total = 0)) %>% 
      mutate(Total = sum(`Barra Longa`, `Rio Doce`, `Santa Cruz do Escalvado`, na.rm = TRUE))
   
   
   TempoResolucao <- manifestacoes %>%
      mutate(datareg = format(datareg, "%Y-%m"),
             statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
      filter(localidadedemandadesc %in% municipiosEscolhidos,
             statusManifestacao == "Finalizada") %>% 
      group_by(datareg) %>% 
      summarise(Média = round(mean(ndias), digits = 1))
   
   
   StatusMes <- manifestacoes %>% 
      mutate(datareg = format(datareg, "%Y-%m"),
             statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
      filter(localidadedemandadesc %in% municipiosEscolhidos) %>% 
      group_by(datareg, statusManifestacao) %>% 
      summarise(Total = n()) %>% 
      pivot_wider(names_from = statusManifestacao, values_from = Total, values_fill = list(Total = 0))
   
   
   
   ### Página 2
   AssuntoTema <- manifestacoes %>%
      mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
      filter(localidadedemandadesc %in% municipiosEscolhidos) %>% 
      group_by(ManifestacaoAssunto, ManifestacaoTema, statusManifestacao) %>% 
      summarise(Total = n(),
                Média = round(mean(ndias), digits = 1),
                Máximo = max(ndias)) %>% 
      pivot_wider(names_from = statusManifestacao, values_from = c(Total, Média, Máximo)) %>% 
      mutate("Total de manifestações" = sum(Total_Finalizada, `Total_Não finalizada`, na.rm = TRUE)) %>% 
      filter(`Total de manifestações` > 30) %>%
      arrange(ManifestacaoAssunto, desc(`Total de manifestações`))
   
   
   
   ### Salva os cálculos
   
   
   setwd(gsub("FundaÃ§Ã£o Renova DiÃ¡logo - ExecuÃ§Ã£o",
              dlgDir(default = getwd(), title = 'Defina a pasta de salvamento')$res,
              replacement = "Fundação Renova Diálogo - Execução"))
   write.xlsx(list(Cabeçalho = Geral,
                   ManifestacoesStatus = ManifestacoesStatus,
                   StatusMes = StatusMes,
                   MensalRegistro = Mensal,
                   TempoMedioResolucao = TempoResolucao,
                   AssuntoTema = AssuntoTema),
              paste0("RptMensal",dlgInput("Complemente o nome do arquivo: RptMensal___")$res,
                                        "_",format(Sys.Date(), "%Y%m"),".xlsx"),
              headerStyle = createStyle(halign = "center", textDecoration = "bold"),
              firstCol = TRUE, firstRow = TRUE, colWidths = "auto", withFilter = TRUE)
   
   rm(Geral,
      ManifestacoesStatus,
      StatusMes,
      Mensal,
      TempoResolucao,
      AssuntoTema)
   
}

relatorioMensal()

rm(relatorioMensal)

   