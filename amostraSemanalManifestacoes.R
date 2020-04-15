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


monitoramento <- filtro327[c(1, 2, 3, 4, 6, 7, 9, 12, 13, 14, 15, 16, 22, 23, 24, 29,
                             34, 35, 37, 38, 39, 41, 44, 49, 50, 55, 56, 58, 60, 61, 64, 65, 66, 68, 70, 80, 81, 82, 83, 84, 85, 86)]


atendentes <- data.frame("id" = c("490608","251870","124561","136308","422350","50357","369974","169453","557128",
                                  "136334","467772","355184","101667","502506","37520","466911","168789","490607",
                                  "52026","515890","516014","290922","557836","421681","303420","136394","40572",
                                  "60158","38714","518807","136402","487445","571532","283447","487444","140441",
                                  "318392","84263","564200","499772"),
                         "Nome" = c("ALDICÉIA COSTA DA SILVA","ALINE CANDEIA SOARES","Aline Constancio da Silva",
                                    "Amanda Cristina Rodrigues Moreira","AMANDA DA SILVA MARTINS","Ana Carolina Roldão",
                                    "Ana Laura da Silva","BIANCA APARECIDA FONSECA","Carlos Henrique Fioret Cruz",
                                    "DIEGO DA COSTA CORTELETTI","Giselle Serrano Stein Mendonça","IZABELA FERREIRA LIMA",
                                    "JOÃO VITOR BATISTA NICCHIO","José Pereira da Cunha","Joyce Moraes Pereira",
                                    "Karina de Souza Fernandes","LARISSA DOS SANTOS PEREIRA","LEANDRO VICENTE DA CUNHA",
                                    "Lígia Batista dos Santos","Marcela Lacerda de Araujo","Marcela Soares Marques",
                                    "Mateus Guilherme de Carvalho","Naiane De Souza Santos","Paulo Alexandre Coelho",
                                    "RAFAEL FERREIRA DE OLIVEIRA","Raquel Fernandes Santiago Barbosa","Rejane Miranda Silva",
                                    "Rosangela Alves Pereira","ROSYANE MARTINS DA SILVA","Sara Neves Silva",
                                    "Sarez Domingos Netto","Sidelio de Oliveira","Sinval Juliao Amaral Gomes",
                                    "Tairon Junior Martins","Thaís Santos Mattos","TIAGO ISAC ALVES DA","VINICIUS SOUSA MOREIRA",
                                    "YURI TSURUTA PLACIDO","Stéphany Phaipher Borges","João Marcelo de Almeida"))


cias <- c("CIA -  Aimorés",
          "CIA -  Baixo Guandu",
          "CIA -  Belo Oriente",
          "CIA - Aracruz",
          "CIA - Baguari",
          "CIA - Barra Longa",
          "CIA - Colatina",
          "CIA - Governador Valadares",
          "CIA - Linhares",
          "CIA - Maria Ortiz",
          "CIA - Mariana",
          "CIA - Mauá",
          "CIA - Naque",
          "CIA - Periquito",
          "CIA - Povoação",
          "CIA - Regência",
          "CIA - Resplendor",
          "CIA - Rio Doce e Sta Cruz do Escalvado",
          "CIA - São Mateus",
          "CIA - Tumiritinga")

periodo <- seq.Date(ymd("2020-03-09"), ymd("2020-04-10"), by = "day")

# amostra <- 
# monitoramento %>% 
#      mutate(SemanaAtual = isoweek(Sys.Date()),
#             SemanaRegistro = isoweek(datareg),
#             AnoRegistro = isoyear(datareg)) %>% 
#      filter((SemanaRegistro == SemanaAtual -1 |
#                   SemanaRegistro == SemanaAtual -2) &
#                  AnoRegistro == isoyear(Sys.Date()),
#                  operadorinseriu %in% atendentes$id,
#             unidadeinseriu %in% cias)


NovosRegistros <- 
     monitoramento %>% 
     filter(operadorinseriu %in% atendentes$id,
            unidadeinseriu %in% cias,
            datareg %in% periodo) %>% 
     group_by(operadorinseriu) %>% 
     sample_n(size = 2, replace = FALSE)


Conclusoes <- 
     monitoramento %>% 
     filter(responsavelconclusao %in% atendentes$Nome,
            dataconclusao %in% periodo) %>% 
     group_by(responsavelconclusao) %>% 
     sample_n(size = 2, replace = TRUE)


Encaminhamentos <- 
        monitoramento %>% 
        filter(ultimoEncaDE %in% atendentes$Nome,
               ultimoencaData %in% periodo) %>%
        group_by(ultimoEncaDE) %>% 
        sample_n(size = 2, replace = TRUE)


Demandas <- 
     monitoramento %>%
     separate(ultimoEncaDEEnc, into = c("ultimoEncaDEEncId", "ultimoEncaDEEncNome"), sep = " - ") %>% 
     mutate(ultimoEncaDEEncId = str_squish(ultimoEncaDEEncId)) %>% 
     filter(ultimoEncaDEEncId %in% atendentes$id,
            ymd(ultimoEncaDataEnc) %in% periodo) %>% 
     group_by(ultimoEncaDEEncId) %>% 
     sample_n(size = 2, replace = TRUE)



setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Manifestacoes")
write.xlsx(list(Infos = c(paste0("Dados referentes ao período de ",
                                 format(as.Date(min(periodo)),
                                        "%d/%m/%Y")," a ",format(as.Date(max(periodo)), "%d/%m/%Y")),
                          paste0("Arquivo gerado em ",format(Sys.time(), "%d/%m/%Y %H:%M"))),
                NovosRegistros = NovosRegistros,
                Encaminhamentos = Encaminhamentos,
                Conclusoes = Conclusoes,
                Demandas = Demandas), paste0("MonitoramentoPreenchimento",format(Sys.Date(), "%Y%m%d"),".xlsx"),
           firstRow = c(FALSE, TRUE, TRUE, TRUE, TRUE),
           firstCol = c(FALSE, TRUE, TRUE, TRUE, TRUE),
           colWidths = c(NA, "auto", "auto", "auto", "auto"))

rm(NovosRegistros, Conclusoes, Encaminhamentos, Demandas, atendentes, cias, periodo, monitoramento)

