# Verifica se os pacotes estão instalados, e caso não estejam, instala e carrega os necessários

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

# Carregamento dos filtros ------------------------------------------------
filtro327 <- read.xlsx(dlg_open(title = "Selecione o arquivo do filtro")$res) #carrega o filtro para o objeto 'filtro327'


# Tratamento das variáveis ------------------------------------------------
### Variáveis de data (o Excel considera a data de origem como "01/01/1900", mas no R é preciso determinar a data de origem com "30-12-1899")
filtro327$datareg <- as.Date(filtro327$datareg, origin = "1899-12-30")
filtro327$datainsercao <- as.Date(filtro327$datainsercao, origin = "1899-12-30")
filtro327$prazo <- as.Date(filtro327$prazo, origin = "1899-12-30")
filtro327$prazoajust <- as.Date(filtro327$prazoajust, origin = "1899-12-30")
filtro327$dataLimiteConclusao <- as.Date(filtro327$dataLimiteConclusao, origin = "1899-12-30")
filtro327$dataconclusao <- as.Date(filtro327$dataconclusao, origin = "1899-12-30")
filtro327$ultimoEncaDataEnc <- as.Date(filtro327$ultimoEncaDataEnc, origin = "1899-12-30")


# essa variável vem como texto (string) utilizando a barra (/) como separador
# algo que o R não reconhece. Para transformá-la em data é necessário usar a função as.Date e o argumento tryFormats
filtro327$ultimoencaData <- as.Date(filtro327$ultimoencaData, tryFormats = c("%d/%m/%Y"))



### Quebra variável de assunto/tema em duas: Assunto e Tema
# (o comando abaixo mantém a variável original 'manifestacaoAssuntoTema'.
# Para removê-la após a seperação de valores, alterar para 'remove = TRUE')
# Lembre-se que removê-la alterará os números de cada coluna na base final (ver final do código)
filtro327 <- separate(filtro327, manifestacaoAssuntoTema,
                      into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = " - ",
                      extra = "drop", fill = "right",
                      remove = FALSE)


### Cria 'municipiosRecod' a partir dos municípios do escopo e alguns outros importantes
filtro327$municipioRecod <- ifelse(filtro327$municipio %in% c("Acaiaca",
                                                              "Aimorés",
                                                              "Alpercata",
                                                              "Aracruz",
                                                              "Baixo Guandu",
                                                              "Barra Longa",
                                                              "Belo Oriente",
                                                              "Bom Jesus do Galho",
                                                              "Bugre",
                                                              "Caratinga",
                                                              "Colatina",
                                                              "Conceição da Barra",
                                                              "Conselheiro Pena",
                                                              "Córrego Novo",
                                                              "Dionísio",
                                                              "Fernandes Tourinho",
                                                              "Fundão",
                                                              "Galileia",
                                                              "Governador Valadares",
                                                              "Iapu",
                                                              "Ipaba",
                                                              "Ipatinga",
                                                              "Itueta",
                                                              "Linhares",
                                                              "Mariana",
                                                              "Marilândia",
                                                              "Marliéria",
                                                              "Naque",
                                                              "Ouro Preto",
                                                              "Pancas",
                                                              "Periquito",
                                                              "Pingo D’Água",
                                                              "Ponte Nova",
                                                              "Raul Soares",
                                                              "Resplendor",
                                                              "Rio Casca",
                                                              "Rio Doce",
                                                              "Santa Cruz do Escalvado",
                                                              "Santana do Paraíso",
                                                              "São Domingos do Prata",
                                                              "São José do Goiabal",
                                                              "São Mateus",
                                                              "São Pedro dos Ferros",
                                                              "Sem-Peixe",
                                                              "Serra",
                                                              "Sobrália",
                                                              "Sooretama",
                                                              "Timóteo",
                                                              "Tumiritinga",
                                                              "Vitória"), filtro327$municipio, "Demais municípios")


# Lista final de colunas da base
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