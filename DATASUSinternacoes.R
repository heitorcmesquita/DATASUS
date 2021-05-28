#MICRODADOS
temp <- tempfile()
mes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
estado <- 'AL'
ano <- '19'

download.file(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/CIHA",estado, ano, "01", ".dbc"), temp, mode = "wb")
AL <- read.dbc::read.dbc(temp)
AL <- AL[0,]

for (i in 1:12){
download.file(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/CIHA",estado, ano, mes[i], ".dbc"), temp, mode = "wb")
ALmes <- read.dbc::read.dbc(temp)
AL <- rbind(AL, ALmes)
}

AL$FAIXA <- ""
AL$IDADE <- as.numeric(AL$IDADE)
AL$SEXO <- as.numeric(AL$SEXO)
AL$SEXO[which(AL$SEXO == 1)] <- "Masculino"
AL$SEXO[which(AL$SEXO == 2)] <- "Feminino"
AL$SEXO[which(AL$SEXO == 3)] <- "Não Informado"

for (i in 1:length(AL$ANO_CMPT)){
if(is.na(AL$IDADE[i])){AL$FAIXA[i] <- "Não informado"} else if(AL$IDADE[i] < 20){AL$FAIXA[i] <- "0 - 20 anos"} else if(AL$IDADE[i] >= 20 & AL$IDADE[i]< 40) {AL$FAIXA[i] <- "20 - 40 anos"} else if(AL$IDADE[i] >= 40 & AL$IDADE[i]< 60) {AL$FAIXA[i] <- "40 - 60 anos"} else if(AL$IDADE[i] >= 60 & AL$IDADE[i]< 80) {AL$FAIXA[i] <- "60 - 80 anos"}  else if(AL$IDADE[i] >= 80) {AL$FAIXA[i] <- "Mais de 80 anos"} else {AL$FAIXA[i] <- "a"}
}

write.csv(AL, "InternacoesAL.csv", row.names = FALSE)
td <- googledrive::drive_get("https://drive.google.com/drive/u/1/folders/1lGXh-Ci4p3M3uqZtqgYsP9wnzEzAuCLI")
2
googledrive::drive_put("InternacoesAL.csv", name="InternacoesAL", type="spreadsheet", path = googledrive::as_id(td))

#_________________________________________________________________
#SUMÁRIO
library(dplyr)
ano <-  c('11', '12', '13', '14', '15', '16', '17', '18', '19', '20')
mes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
estado <- "AL"
temp <- tempfile()
InternacoesAL <- data.frame(ANO_CMPT = numeric(), MES_CMPT = numeric(), MUNIC_RES = numeric(), FAIXA = character(), DIAG_PRINC = character())

for (j in 1:length(ano)){
for (i in 1:12){
  download.file(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/CIHA",estado, ano, "01", ".dbc"), temp, mode = "wb")
  Internacoes <- read.dbc::read.dbc(temp)
  Internacoes <- Internacoes[0,]
  
  download.file(paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/CIHA/201101_/Dados/CIHA",estado, ano[j], mes[i], ".dbc"), temp, mode = "wb")
  month <- read.dbc::read.dbc(temp)
  Internacoes <- rbind(Internacoes, month)
  Internacoes$FAIXA <- ""
  Internacoes$IDADE <- as.numeric(Internacoes$IDADE)
  Internacoes$SEXO <- as.numeric(Internacoes$SEXO)
  Internacoes$SEXO[which(Internacoes$SEXO == 1)] <- "Masculino"
  Internacoes$SEXO[which(Internacoes$SEXO == 2)] <- "Feminino"
  Internacoes$SEXO[which(Internacoes$SEXO == 3)] <- "Não Informado"
  
  for (k in 1:length(month$ANO_CMPT)){
    if(is.na(Internacoes$IDADE[k])){Internacoes$FAIXA[k] <- "Não informado"} else if(Internacoes$IDADE[k] < 20){Internacoes$FAIXA[k] <- "0 - 20 anos"} else if(Internacoes$IDADE[k] >= 20 & Internacoes$IDADE[k]< 40) {Internacoes$FAIXA[k] <- "20 - 40 anos"} else if(Internacoes$IDADE[k] >= 40 & Internacoes$IDADE[k]< 60) {Internacoes$FAIXA[k] <- "40 - 60 anos"} else if(Internacoes$IDADE[k] >= 60 & Internacoes$IDADE[k]< 80) {Internacoes$FAIXA[k] <- "60 - 80 anos"}  else if(Internacoes$IDADE[k] >= 80) {Internacoes$FAIXA[k] <- "Mais de 80 anos"} else {Internacoes$FAIXA[k] <- "a"}
  }
  
  resumo <- Internacoes %>%
    group_by(ANO_CMPT, MES_CMPT, MUNIC_RES, FAIXA, DIAG_PRINC) %>%
    summarise(n())
  
InternacoesAL <- rbind(InternacoesAL, resumo)
  
}
}

InternacoesAL$DIAG_PRINC <- as.character(InternacoesAL$DIAG_PRINC)


for(l in 1:length(InternacoesAL$ANO_CMPT)){
    if(is.na(nchar(InternacoesAL$DIAG_PRINC[l]) == 3)){
      InternacoesAL$DIAG_PRINC[l] <- InternacoesAL$DIAG_PRINC[l]}
  else if (nchar(InternacoesAL$DIAG_PRINC[l]) == 3){
      parte1 <- substring(InternacoesAL$DIAG_PRINC[l], 1, 1)
        parte2 <- substring(InternacoesAL$DIAG_PRINC[l], 2, 3)
        InternacoesAL$DIAG_PRINC[l] <- paste0(parte1, "0", parte2)
}
}


write.csv(InternacoesAL, "InternacoesAL.csv", row.names = FALSE)
td <- googledrive::drive_get("https://drive.google.com/drive/u/1/folders/1lGXh-Ci4p3M3uqZtqgYsP9wnzEzAuCLI")
2
googledrive::drive_put("InternacoesAL.csv", name="InternacoesAL", type="spreadsheet", path = googledrive::as_id(td))


