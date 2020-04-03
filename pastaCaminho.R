caminho <- function(pasta){
     if(Sys.info()[7] == "Claudio")
          paste("C:/Users/",Sys.info()[7],"/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/", pasta, sep = "")
     else paste("C:/Users/",Sys.info()[7],"/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/", pasta, sep = "")
}