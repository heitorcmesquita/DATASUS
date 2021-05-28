library(tidyverse)
library(dplyr)
library(hash)

#materno

ano <- c('14', '15', '16', '17', '18','19')
uf <- c('RO', 'AC', 'AM','RR', 'PA','AP',
        'TO', 'MA', 'PI', 'CE', 'RN', 'PB',
        'PE','AL', 'SE','BA', 'MG','ES',
        'RJ','SP', 'PR','SC','RS','MT','GO','DF','MS')

mortalidadematerna <- function(ano, uf){
  h <- hash()
  h[['RO']] <- 11
  h[['AC']] <- 12
  h[['AM']] <- 13
  h[['RR']] <- 14
  h[['PA']] <- 15
  h[['AP']] <- 16
  h[['TO']] <- 17
  h[['MA']] <- 21
  h[['PI']] <- 22
  h[['CE']] <- 23
  h[['RN']] <- 24
  h[['PB']] <- 25
  h[['PE']] <- 26
  h[['AL']] <- 27
  h[['SE']] <- 28
  h[['BA']] <- 29
  h[['MG']] <- 31
  h[['ES']] <- 32
  h[['RJ']] <- 33
  h[['SP']] <- 35
  h[['PR']] <- 41
  h[['SC']] <- 42
  h[['RS']] <- 43
  h[['MT']] <- 51
  h[['GO']] <- 52
  h[['DF']] <- 53
  h[['MS']] <- 50
  
  if(ano!='20') {
    url <- paste0('ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/DOMAT', ano, '.dbc')
  } else {
    url <- paste0('ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/DOPMAT', ano, '.dbc')
  }

  
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  df <- read.dbc::read.dbc(temp)
 
  df$UF <- substr(df$CODMUNRES, 1, 2)
  df <- subset(df, UF == h[[uf]])
  df$IDADE_C<-substr(df$IDADE,2,3)
  df$IDADE_Fx <- cut(as.numeric(as.character(df$IDADE_C)), breaks = c(0, 15, 17, 20, 23, 28, 32, 40, 50, Inf), labels = c('0-14', '15-16', '17-19', '20-22', '23-27', '28-31', '32-39', '40-50', '50+'))
  df <- df %>% 
    group_by(RACACOR, CODMUNRES) %>%
    summarise(n())
    colnames(df) <- c('Raca', 'Mun','Obitos')
    df$ano<-ano
    
    return(df)

    
  if (ano!='20') {
    url <- paste0('ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES//DN', uf, '20', ano, '.dbc')
  } else {
    url<-paste0('ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES//DNP', uf, '2020.dbc')
  }
    temp <- tempfile()
    download.file(url, temp, mode = "wb")
    df2 <- read.dbc::read.dbc(temp)
    df2$IDADE_Fx <- cut(as.numeric(as.character(df2$IDADEMAE)), breaks = c(0, 15, 17, 20, 23, 28, 32, 40, 50, Inf), labels = c('0-14', '15-16', '17-19', '20-22', '23-27', '28-31', '32-39', '40-50', '50+'))
    
    df2 <- df2 %>% 
      group_by(RACACOR, CODMUNRES,IDADE_Fx,ESCMAE) %>%
      summarise(n())
      colnames(df2) <- c('Raca Viv', 'Mun Viv','Fx Etaria Viv','Escolaridade Viv','Nascidos Vivos')
      df2$ano <- ano

  
    df <- merge(df, df2, all.x = TRUE, all.y = TRUE, by.y = 'Mun Viv', by.x = 'Mun')

  return(df)
  }
  
mortmat <- data.frame()

for (j in uf){
  for (i in ano){
    df <- mortalidadematerna(i, j)
    df$UF <- j
    mortmat <- rbind(mortmat, df)
  }  
}


write.csv(mortmat, "mortalidadematerna.csv", row.names = FALSE)
td <- googledrive::drive_get("https://drive.google.com/drive/u/1/folders/1ub18RCGe763AksCx4DOOjd4TaXK6xokh")
2
googledrive::drive_put("mortalidadematerna.csv", path = googledrive::as_id(td))
