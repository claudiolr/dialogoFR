library(svDialogs)
setwd(dlgDir(default = getwd(), title = "Defina o local onde estao os codigos R" )$res)


# Carrega e trata o filtro 327 (o carregamento pode demorar cerca de 10 minutos)
source("filtro327.R", local = TRUE, verbose = TRUE, encoding = "utf-8")


# Executa o código para geração dos dados do relatório CIF
source("reunioesCIF.R", local = TRUE, verbose = TRUE, encoding = "utf-8")


# Executa o código para geração de amostras de registros para monitoramento pela equipe NICQ
source("amostraSemanalManifestacoes.R", local = TRUE, verbose = TRUE, encoding = "utf-8")


# Executa o código para geração das tabelas da análise de cenário (geral)
source("analiseCenario.R", local = TRUE, verbose = TRUE, encoding = "utf-8")


# Executa o código para geração das tabelas da análise de cenário (geral)
source("analiseCenarioPGs.R", local = TRUE, verbose = TRUE, encoding = "utf-8")


# Executa o código para geração dos dados para relatório mensal dos territórios
source("relatorioMensal.R", local = TRUE, verbose = TRUE, encoding = "utf-8")
