###########################################################################
# Carregamento dos filtros ------------------------------------------------
###########################################################################

setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/FundRenova/DialogoSocial/Filtros/129")
banco <- read.xlsx("filtro_129.xlsx", cols = c(1:3, 6, 8:9, 13:14, 19:20, 34, 52))


# cria backup da base única gerada para não ser necessário importar os arquivos novamente
# backup <- manifestacoes



# setwd("C:/Users/MAGNA TI/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")
setwd("C:/Users/Claudio/HERKENHOFF & PRATES/HERKENHOFF & PRATES/Fundação Renova Diálogo - Execução/Relatorios_ApresentacoesGerais/ArqAux")
municipios <- read.xlsx("Municipios.xlsx")

# gera base única de manifestações
banco <- merge(banco, municipios, by = "municipio", all = TRUE)
banco$municipioRecod <- ifelse(is.na(banco$codMunicipio), "Demais municípios", banco$municipio)
rm(municipios)



###########################################################################
# Tratamento das variáveis ------------------------------------------------
###########################################################################

### Transforma variáveis string em categóricas (as.factor)
banco$localidadedemandadesc <- as.factor(banco$localidadedemandadesc)
banco$statusManifestacao <- as.factor(banco$statusManifestacao)


### Transforma variáveis string em datas (origem 30/12/1899 se deve ao fato de a variável original conter horas)
banco$dataregistro <- as.Date(banco$dataregistro, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
banco$DataRegistroAnoMes <- format(banco$dataregistro, "%Y/%m")
banco$DataRegistroAnoMes <- as.factor(banco$DataRegistroAnoMes)


### Quebra variável de assunto/tema em duas: Assunto e Tema
banco <- separate(banco, manifestacaoAssuntoTema,
                  into = c("ManifestacaoAssunto", "ManifestacaoTema"), sep = "-",
                  extra = "drop", fill = "right",
                  remove = TRUE)


### Remove espaços extras das variáveis Assunto e Tema criadas
banco$ManifestacaoAssunto <- str_squish(str_trim(banco$ManifestacaoAssunto))
banco$ManifestacaoTema <- str_squish(str_trim(banco$ManifestacaoTema))


### Seleciona e reordena colunas

banco <- banco[c("idManifestacao",
                 "protocolo",
                 "statusManifestacao",
                 "dataregistro",
                 "DataRegistroAnoMes",
                 "Unidade",
                 "FormaRecebimento",
                 "ManifestacaoAssunto",
                 "ManifestacaoTema",
                 "manifestacaoSubtema",
                 "manifestante_codPessoa",
                 "localidadedemandadesc",
                 "uf",
                 "municipio",
                 "municipioRecod")]