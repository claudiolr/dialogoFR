library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


# Indicador de respostas qualificadas -------------------------------------

# Geral (todos os anos)
resposta %>%
     filter(datareg <= ymd("2020-02-29")) %>%  # Filtra as repostas até o último dia do período de referência
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(manifencaminhamentotipo_en %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>% 
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Total) %>% 
     mutate(RespostaQualificada = ifelse(is.na(Qualificada), "Não", "Sim")) %>% 
     group_by(RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     mutate(Proporção = round(prop.table(Total)*100, digits = 1))

round(prop.table(table(resposta$manifencaminhamentotipo_en))*100, digits = 1)

# 2020
resposta %>%
     filter(datareg <= ymd("2020-02-29") & datareg >= ymd("2020-01-01")) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(manifencaminhamentotipo_en %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Total) %>% 
     mutate(RespostaQualificada = ifelse(is.na(Qualificada), "Não", "Sim")) %>% 
     group_by(RespostaQualificada) %>% 
     summarise(Total = n()) %>% 
     mutate(Proporção = round(prop.table(Total)*100, digits = 1))



###########################################################################
# Indicador de tempo de resposta ------------------------------------------

# Geral
resposta %>%
     filter(datareg <= ymd("2020-02-29")) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(manifencaminhamentotipo_en %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Mínimo = min(diasEnc)) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Mínimo) %>% 
     mutate(Indicador = ifelse(is.na(Qualificada), "Não",
                               ifelse(Qualificada > 20, "Não", "Sim"))) %>% 
     group_by(Indicador) %>% 
     summarise(Total = n()) %>% 
     mutate(Proporção = round(prop.table(Total)*100, digits = 1))


# 2020
resposta %>%
     filter(datareg <= ymd("2020-02-29") & datareg >= ymd("2020-01-01")) %>% 
     mutate(RespostaQualificada = ifelse(statusManifestacao %in% c("Respondida",
                                                                   "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                   "Respondida no ato"), "Qualificada",
                                         ifelse(manifencaminhamentotipo_en %in% c("Dar retorno final",
                                                                                  "Dar retorno intermediário",
                                                                                  "Retorno final dado",
                                                                                  "Retorno intermediário dado"), "Qualificada", "Não qualificada"))) %>%
     group_by(idManifestacao, RespostaQualificada) %>% 
     summarise(Mínimo = min(diasEnc)) %>% 
     pivot_wider(names_from = RespostaQualificada, values_from = Mínimo) %>% 
     mutate(Indicador = ifelse(is.na(Qualificada), "Não",
                               ifelse(Qualificada > 20, "Não", "Sim"))) %>% 
     group_by(Indicador) %>% 
     summarise(Total = n()) %>% 
     mutate(Proporção = round(prop.table(Total)*100, digits = 1))

