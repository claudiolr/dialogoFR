

analiseCenarioPG <- function(){
     
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
     
     dlgMessage('Primeiro passo: certifique-se que o filtro327 esta carregado e tratado (codigo "filtro327.R)')
     

     # Cria coluna de Assunto apenas com o "PG"
     filtro327 <- filtro327 %>% separate(ManifestacaoAssunto, sep = " ", into = "Assunto")
     
     # 2. Define o período de análise no formato ymd(year-month-day)
     MesAtual <- seq.Date(from = dmy(dlgInput("Defina a data de inicio do MES de analise no formato dd-mm-aaaa (sem aspas)")$res),
                          to = dmy(dlgInput("Defina a data de fim do MES de analise no formato dd-mm-aaaa (sem aspas)")$res), by = "day")
     
     Semestre <- seq.Date(from = dmy(dlgInput("Defina a data de inicio do SEMESTRE no formato dd-mm-aaaa (sem aspas)")$res),
                          to = dmy(dlgInput("Defina a data de fim do SEMESTRE no formato dd-mm-aaaa (sem aspas)")$res), by = "day")
     
     # Cria lista de votores dos temas (conjuntos de PGs)
     conjuntos <- list(Tema1 = c("PG001"),
                       Tema2 = c("PG002", "PG021"),
                       Tema3 = c("PG017", "PG025", "PG026", "PG027", "PG040"),
                       Tema4 = c("PG016"),
                       Tema5 = c("PG015", "PG018", "PG019", "PG020", "PG042"),
                       Tema6 = c(paste0("PG0",c(23:24, 28:31, 33:41))),
                       Tema7 = c("PG032"),
                       Tema8 = c("PG008", "PG010"),
                       Tema9 = c("PG005", "PG014"),
                       Tema10 = c("PG003", "PG004"),
                       Tema11 = c("PG013"))
     
     for(i in conjuntos){
          PG = i
          
          tabela1 <- filtro327 %>% 
               mutate(MesAtual = ifelse(datareg %in% MesAtual, 1, 0),
                      statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                            "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                            "Respondida no ato"), "Finalizada", "Não finalizada")) %>% 
               filter(Assunto %in% PG) %>% 
               group_by(statusManifestacao) %>% 
               summarise(N = n()) %>% 
               mutate(Percentual = round(N/sum(N), digits = 3)*100)
          
          
          tabela2 <- filtro327 %>% 
               mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                 "T02 Alto Rio Doce" = "T02. ARD",
                                                                 "T03 Calha do Rio Doce" = "T03. CRD",
                                                                 "T04 Médio Rio Doce" = "T04. MRD",
                                                                 "T05 Baixo Rio Doce" = "T05. BRD",
                                                                 "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
               filter(Assunto %in% PG,
                      territorio != "Não informado",
                      datareg %in% MesAtual) %>% 
               group_by(territorio) %>% 
               summarise(N = n()) %>% 
               mutate(Percentual = round(N/sum(N), digits = 3)*100)
          
          
          tabela3 <- filtro327 %>% 
               filter(datareg %in% Semestre,
                      Assunto %in% PG) %>% 
               mutate(datareg = format(datareg, "%Y-%m")) %>% 
               group_by(datareg) %>% 
               summarise(N = n())
          
          
          tabela4 <- filtro327 %>% 
               filter(datareg %in% MesAtual,
                      Assunto %in% PG) %>% 
               group_by(ManifestacaoTema) %>% 
               summarise(N = n()) %>% 
               mutate(Percentual = round(N/sum(N), digits = 3)*100) %>% 
               arrange(desc(Percentual))
          
          
          tabela5 <- filtro327 %>%
               mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                 "T02 Alto Rio Doce" = "T02. ARD",
                                                                 "T03 Calha do Rio Doce" = "T03. CRD",
                                                                 "T04 Médio Rio Doce" = "T04. MRD",
                                                                 "T05 Baixo Rio Doce" = "T05. BRD",
                                                                 "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
               filter(Assunto %in% PG,
                      territorio != "Não informado") %>% 
               group_by(territorio) %>% 
               summarise(N = n()) %>% 
               mutate(Percentual = round(N/sum(N), digits = 3)*100)
          
          
          tabela6 <- filtro327 %>% 
               mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                            "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)",
                                                                            "Respondida no ato"), "Respondida", "Não Respondida")) %>% 
               filter(Assunto %in% PG) %>% 
               group_by(ManifestacaoTema, statusManifestacao) %>% 
               summarise(Total = n()) %>%
               arrange(desc(Total)) %>%
               pivot_wider(names_from = statusManifestacao, values_from = Total, values_fill = list(Total = 0))
          
          
          tabela7 <- filtro327 %>% 
               mutate(statusManifestacao = ifelse(statusManifestacao %in% c("Respondida",
                                                                            "Respondida (não se enquadra nas políticas de indenização e auxilio financeiro atuais)"),
                                                  "Finalizada",
                                                  ifelse(statusManifestacao == "Respondida no ato", "Finalizada no ato", "Não finalizada"))) %>% 
               filter(Assunto %in% PG) %>% 
               group_by(statusManifestacao) %>% 
               summarise(Total = n()) %>% 
               mutate(Percentual = round(prop.table(Total), digits = 3)*100)
          
          
          tabela8 <- filtro327 %>% 
               filter(Assunto %in% PG,
                      requerAcaoFutura == 1) %>% 
               mutate(statusdemanda = ifelse(statusdemanda == "A iniciar", "Não respondida", "Respondida")) %>% 
               group_by(statusdemanda) %>% 
               summarise(Total = n()) %>% 
               mutate(Percentual = round(prop.table(Total), digits = 3)*100)
          
          
          tabela9 <- filtro327 %>% 
               mutate(territorio = str_replace_all(territorio, c("T01 Mariana" = "T01. MRN",
                                                                 "T02 Alto Rio Doce" = "T02. ARD",
                                                                 "T03 Calha do Rio Doce" = "T03. CRD",
                                                                 "T04 Médio Rio Doce" = "T04. MRD",
                                                                 "T05 Baixo Rio Doce" = "T05. BRD",
                                                                 "T06 Foz do Rio Doce - Litoral ES" = "T06. FOZ"))) %>% 
               filter(Assunto %in% PG,
                      requerAcaoFutura == 1,
                      territorio != "Não informado") %>% 
               group_by(territorio) %>% 
               summarise(Total = n()) %>% 
               mutate(Percentual = round(prop.table(Total), digits = 3)*100)
          
          
          tabela10 <- filtro327 %>% 
               mutate(statusdemanda = str_replace_all(statusdemanda, c("Em andamento para retorno intermediário" = "Em andamento",
                                                                       "Em andamento para retorno final" = "Em andamento"))) %>% 
               filter(Assunto %in% PG,
                      requerAcaoFutura == 1) %>% 
               group_by(statusdemanda) %>% 
               summarise(Total = n()) %>% 
               mutate(Percentual = round(prop.table(Total), digits = 3)*100)
          
          
          tabela11 <- filtro327 %>% 
               filter(Assunto %in% PG,
                      requerAcaoFutura == 1) %>% 
               mutate(statusdemanda = ifelse(statusdemanda == "A iniciar", "Não respondida", "Respondida")) %>% 
               group_by(ManifestacaoTema, statusdemanda) %>% 
               summarise(Total = n()) %>% 
               arrange(desc(Total)) %>% 
               pivot_wider(names_from = statusdemanda, values_from = Total, values_fill = list(Total = 0))
          
          
          setwd(gsub("FundaÃ§Ã£o Renova DiÃ¡logo - ExecuÃ§Ã£o",
                     dlgDir(default = getwd(), title = 'Defina a pasta onde os arquivos serao salvos')$res,
                     replacement = "Fundação Renova Diálogo - Execução"))
          
          write.xlsx(list(infos = c("Programas analisados",PG),
                          "Manif(geral)" = tabela1,
                          "Manif(mês)_territ" = tabela2,
                          "Manif(semestre)" = tabela3,
                          "Manif(tema)" = tabela4,
                          "Manif(geral)_territ" = tabela5,
                          "Manif_statusSubtema" = tabela6,
                          "Manif_statusGeral" = tabela7,
                          "DemandasIndiv(geral)" = tabela8,
                          "DemandasTerrit" = tabela9,
                          "DemandasStatus" = tabela10,
                          "AssuntosStatus" = tabela11),
                     paste0("AnaliseCenario_",PG[1],".xlsx"),
                     colWidths = c(NA, "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto", "auto"))
          gc() # Liberar ram a cada loop
     }
}

analiseCenarioPG()

rm(analiseCenarioPG)

