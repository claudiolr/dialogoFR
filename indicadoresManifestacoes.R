library(tidyverse)
library(stringr)
library(openxlsx)
library(lubridate)
library(svDialogs)


# Calcula indicadores de manifestações a partir do filtro327

periodoFim <- as.Date(dmy(dlgInput("Informe o ultimo dia do periodo de referencia no formato dd-mm-aaaa")$res))



# Indicador de respostas qualificadas -------------------------------------

# Geral (todos os anos)
filtro327 %>%
     filter(datareg <= ymd(periodoFim)) %>%  # Filtra as repostas até o último dia do período de referência
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(ultimoencaTipo %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>% 
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Total) %>% 
     mutate(RespostaQualificada = ifelse(is.na(Qualificada), "Não", "Sim")) %>% 
     group_by(RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total)*100, digits = 1))

knitr::kable(
round(prop.table(table(filtro327$ultimoencaTipo))*100, digits = 1))


# 2020
filtro327 %>%
     filter(datareg <= ymd(periodoFim) & datareg >= ymd("2020-01-01")) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(ultimoencaTipo %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Total) %>% 
     mutate(RespostaQualificada = ifelse(is.na(Qualificada), "Não", "Sim")) %>% 
     group_by(RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total)*100, digits = 1))



###########################################################################
# Indicador de tempo de resposta ------------------------------------------

# Geral
filtro327 %>%
     filter(datareg <= ymd(periodoFim)) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(ultimoencaTipo %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Mínimo = min(ndias)) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Mínimo) %>% 
     mutate(Indicador = ifelse(is.na(Qualificada), "Não",
                               ifelse(Qualificada > 20, "Não", "Sim"))) %>% 
     group_by(Indicador) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total)*100, digits = 1))


# 2020
filtro327 %>%
     filter(datareg <= ymd(periodoFim) & datareg >= ymd("2020-01-01")) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(ultimoencaTipo %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Mínimo = min(ndias)) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Mínimo) %>% 
     mutate(Indicador = ifelse(is.na(Qualificada), "Não",
                               ifelse(Qualificada > 20, "Não", "Sim"))) %>% 
     group_by(Indicador) %>% 
     summarise(Total = n()) %>% 
     mutate(Percentual = round(prop.table(Total)*100, digits = 1))

