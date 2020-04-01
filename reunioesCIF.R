library(reshape2)
library(tidyverse)
library(openxlsx)
library(lubridate)



# Carregamento e tratamento das bases -------------------------------------

# Filtro 20 - Tratamento --------------------------------------------------
# Caminho do Filtro 20

setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/OrganizacaoInformacao/FiltrosSGS/20")
filtro <- read.xlsx("filtro_20_20200327144916.xlsx")
filtro <- filtro %>% select("idAcao",
                            "titulo",
                            "tipoAcao",
                            "abrangencias",
                            "municipio",
                            "UF",
                            "statusAcao",
                            "TotaldeParticipantes",
                            "realdatahorainicio",
                            "CategoriaContrato")


# Tratamento de variáveis -------------------------------------------------
filtro$CategoriaContrato <- as.factor(filtro$CategoriaContrato)       # transforma a variável em em categórica (factor)
filtro$tipoAcao <- as.factor(filtro$tipoAcao)                         # transforma a variável em categórica (factor)
filtro$statusAcao <- as.factor(filtro$statusAcao)                     # transforma a variável em categórica (factor)


# Variáveis de data ###
filtro$realdatahorainicio <- as.Date(filtro$realdatahorainicio, origin = "1899-12-30", tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))


# Filtros por valores
filtro <- filtro %>% filter(CategoriaContrato == "FUND. RENOVA - REPARAÇÃO") # filtra pela categoria de contrato "FUND. RENOVA - REPARAÇÃO"
filtro$CategoriaContrato <- NULL
filtro <- filtro %>% filter(!is.na(realdatahorainicio))                     # apaga linhas em que a data está em branco

# Seleciona tipos de ação por data (antes e depois de 11/02/2019)
filtro <- filtro %>% filter((tipoAcao == "Reunião Temática ou com Grupos Específicos" |
                                  tipoAcao == "Reunião com Comunidade" |
                                  tipoAcao == "Reunião com Poder Público" |
                                  tipoAcao == "Reunião com Lideranças" |
                                  tipoAcao == "Assembleia") & realdatahorainicio < ymd("2019-02-11") |
                                 tipoAcao == "Agenda de Diálogo" & realdatahorainicio >= ymd("2019-02-11"))

filtro <- filtro %>% filter(statusAcao == "Realizada") # filtra apenas as ações realizadas


# Quebra por estado
filtro$abrangenciasUF <- ifelse(grepl("MG", filtro$abrangencias), "Minas Gerais",
                                ifelse(grepl("ES", filtro$abrangencias), "Espírito Santo",
                                       ifelse(is.na(filtro$abrangencias), NA, "Outras Localidades")))


# Alterações manuais de abrangência ---------------------------------------
filtro$municipio[filtro$idAcao %in% c("20835","20873")] <- "Ipatinga"
filtro$municipio[filtro$idAcao %in% c("17741","19126","19128","20844")] <- "Linhares"
filtro$municipio[filtro$idAcao %in% c("19167")] <- "Mariana"
filtro$municipio[filtro$idAcao %in% c("19366")] <- "Outros ES"
filtro$municipio[filtro$idAcao %in% c("19127","21745")] <- "Aracruz"



# Municípios --------------------------------------------------------------
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")

municipios <- read.xlsx("Municipios.xlsx")


# Backup ações ------------------------------------------------------------
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")

bkpAcoes <- read.xlsx("BkpAcoes_10fev2019.xlsx", cols = c(1, 3))



# Matizes -----------------------------------------------------------------
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")

matizes <- read.xlsx("AcoesMatizes.xlsx")

matizes$realdatahorainicio <- as.Date(matizes$realdatahorainicio, origin = "1899-12-30")
matizes <- matizes[c("idAcao",
                     "titulo",
                     "municipio",
                     "codMunicipio",
                     "UF",
                     "territorio",
                     "codIBGE",
                     "statusAcao",
                     "TotaldeParticipantes",
                     "realdatahorainicio",
                     "fonte")]


# Consolidado Agenda ------------------------------------------------------
# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/DemEstAnalisesEspecificas/Delib216/ConsolAgendas")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/DemEstAnalisesEspecificas/Delib216/ConsolAgendas")

consolidado <- read.xlsx("PlanConsolidAgenda_202002V1.xlsm", startRow = 5,
                         sheet = "Registro de Reuniões", detectDates = TRUE,
                         cols = c(2:3,5:6,12:13, 15)) # Carrega o arquivo selecionando as colunas defininas no código

colnames(consolidado) <- c("titulo",
                           "realdatahorainicio",
                           "local",
                           "municipio",
                           "statusAcao",
                           "controleAta",
                           "TotaldeParticipantes")

consolidado$TotaldeParticipantes <- as.numeric(consolidado$TotaldeParticipantes)

consolidado$municipio <- substr(consolidado$municipio, start = 1, nchar(consolidado$municipio) - 5)
consolidado$municipio[consolidado$municipio=="Sta. Cruz do Escalvado"] <- "Santa Cruz do Escalvado"
consolidado <- consolidado %>% filter(statusAcao %in% c("Realizada", "Planejada"))
consolidado$idAcao <- as.numeric("")
consolidado <- add_column(consolidado, "fonte"="Plan")
consolidado$realdatahorainicio <- as.Date(consolidado$realdatahorainicio, origin = "1899-12-30")




# Montagem da base final --------------------------------------------------

# Merge de 'filtro' e 'bkpAcoes' ------------------------------------------
reunioes <- merge(filtro, bkpAcoes, by=c("idAcao"), all.x = TRUE)
reunioes$municipio.x <- ifelse(reunioes$realdatahorainicio < ymd("2019-02-11"), reunioes$municipio.y, reunioes$municipio.x)

reunioes[c("municipio.y", "UF")] <- NULL
reunioes <- rename(reunioes, municipio = municipio.x)



# Merge de 'reunioes' e 'municipios' (para pegar dados completos de território)
reunioes <- merge(reunioes, municipios, by = c("municipio"), all.x = TRUE)

reunioes <- reunioes[c("idAcao",
                       "titulo",
                       "municipio",
                       "codMunicipio",
                       "UF",
                       "territorio",
                       "codIBGE",
                       "statusAcao",
                       "TotaldeParticipantes",
                       "realdatahorainicio")]

reunioes$fonte <- "Ações"


# Merge de 'reunioes' e 'consolidado'
consolidado <- merge(consolidado, municipios, by = "municipio", all.x = TRUE)
consolidado <- consolidado[c("idAcao",
                             "titulo",
                             "municipio",
                             "codMunicipio",
                             "UF",
                             "territorio",
                             "codIBGE",
                             "statusAcao",
                             "realdatahorainicio",
                             "TotaldeParticipantes",
                             "fonte")]



# Empilhamento das bases (PlanPrincipal)
reunioes <- rbind(reunioes, matizes)
reunioes <- rbind(reunioes, consolidado)

reunioes$totalParticipantes[is.na(reunioes$TotaldeParticipantes)] <- 0



# Montagem da "planilha do João"
plan1 <- reunioes %>%
     select("realdatahorainicio",
            "municipio",
            "TotaldeParticipantes",
            "territorio",
            "codIBGE") %>% 
     mutate(realdatahorainicio = format(realdatahorainicio, "%b-%Y")) %>% 
     group_by(realdatahorainicio, municipio) %>% 
     summarise(Participantes = sum(TotaldeParticipantes, na.rm = TRUE),
               Reuniões = n()) %>% 
     pivot_longer(cols = 3:4, names_to = "Tipo", values_to = "Contagem")  # 'pivotagem' da planilha do "João"

plan1 <- merge(plan1, municipios, by = "municipio", all.x = TRUE)
plan1[c("codMunicipio")] <- NULL


# Montagem dos cálculos gerais
geral <- reunioes %>% 
     select("realdatahorainicio",
            "municipio",
            "UF",
            "TotaldeParticipantes",
            "territorio",
            "codIBGE") %>% 
     group_by(UF) %>% 
     summarise(Participantes = sum(TotaldeParticipantes, na.rm = TRUE),
               Reuniões = n()) %>% 
     pivot_longer(cols = 2:3, names_to = "Tipo", values_to = "Contagem") %>% 
     pivot_wider(names_from = UF, values_from = Contagem)



geralMesesAnteriores <- reunioes %>% 
     select("realdatahorainicio",
            "municipio",
            "UF",
            "TotaldeParticipantes",
            "territorio",
            "codIBGE") %>% 
     mutate(realdatahorainicio = format(realdatahorainicio, "%b-%Y")) %>% 
     filter(realdatahorainicio == "fev-2020" | realdatahorainicio == "jan-2020") %>%
     group_by(UF, realdatahorainicio) %>% 
     summarise(Participantes = sum(TotaldeParticipantes, na.rm = TRUE),
               Reuniões = n()) %>% 
     pivot_longer(cols = 3:4, names_to = "Tipo", values_to = "Contagem") %>% 
     pivot_wider(names_from = c(realdatahorainicio, UF), values_from = Contagem)


geral <- merge(geral, geralMesesAnteriores, by = "Tipo")


rm(bkpAcoes, consolidado, filtro, geralMesesAnteriores, matizes, municipios)



setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/RptMensal/202003")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/RptMensal/202003")
write.xlsx(list(PlanPrincipal = reunioes,
                Plan = plan1,
                Geral = geral),
           "BDDadCIF202003.xlsx",
           headerStyle = createStyle(halign = "center", textDecoration = "bold"),
           firstCol = TRUE, firstRow = TRUE, withFilter = TRUE)

rm(geral, plan1, reunioes)


