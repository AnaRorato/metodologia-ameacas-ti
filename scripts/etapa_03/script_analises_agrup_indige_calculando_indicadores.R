########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

### Combinando as diferentes Zonas de buffer (faixas) concêntricas no entorno dos agrupamentos indígenas no mesmo arquivo para calcular os indicadores

# 1. Inicialização dos pacotes requeridos. 
#Caso o pacote ainda não esteja instalado: 1° install.packages("pacote_desejado") e 2º library("pacote_desejado")

library(openxlsx)
library(ggpmisc)
library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
library(maptools)
library(spatstat)
library(rgeos)
library(ggmap)
library(sf)
library(ggspatial)
library(ggsci)
library(RColorBrewer)
library(maps)
library(grid)
library(cowplot)
library(MetBrewer)
library(raster)
library(tidyr)
library(dplyr)


#2. carrega o diretório de trabalho
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_zones_agrupamentos_indigenas")

#3.Carrega os shapefiles com os dados para as três faixas/buffers

bf_0_05<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_5/faixa_5_variaveis.shp")
bf_0_10<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/faixa_10_variaveis.shp")
bf_0_25<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/faixa_25_variaveis.shp")

#4. Verificar a estrutura do shapefile para entender como as colunas estão organizadas
colnames(bf_0_05) 
colnames(bf_0_10) 
colnames(bf_0_25) 

#5. Juntar os shapes das faixas/buffers 0_10 km e 0_5 km em um só para fazer as subtrações de área de desmatamento, degradação florestal, mineração e número de focos de calor

## Juntando os shapes com base no campo "ID"

library(dplyr)
library(sf)

# Faz o join (união) mantendo a geometria de bf_0_10 como referência

shp_joined_10_05 <- bf_0_10 %>%
  left_join(st_drop_geometry(bf_0_05), by = "ID")

colnames(shp_joined_10_05)

# 6. Salva o novo shapefile como:
st_write(shp_joined_10_05, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/shp_joined_faixa_10_and_05.shp", append=FALSE)



########################################## Calculando os indicadores da faixa de 5 - 10 km

###7. Calcular os indicadores da faixa 5_10 km com base na subtração do buffer 0_5 km do buffer 0_10 km:

# Carrega o shapefiles com os dados das faixas/buffers 0_10 e 0_5 unidos
bf10<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/shp_joined_faixa_10_and_05.shp")


### 8. calcular a área do BF 05-10  (para isso = area bf 0-10 - area bf 0-5)

bf10$ar5_10ha <- round(bf10$arbf10ha - bf10$arbf05ha, 1)


### Indicador de desmatamento

### 9. Calcular área de desmatamento acumulado no período de 2018-2023 no buffer 5-10 km. (Para isso: defor bf 0-10 - defor bf 0-5)

bf10$df_5_10h <- round(bf10$dfacha10 - bf10$dfacha05, 1)

  
### 10. Calcular % de desmatamento acumulado no periodo de 2018-2023 no buffer 5-10 km. (Para isso: area acumulada de defor / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf10$df_5_10p <- c(bf10$df_5_10h / bf10$ar5_10ha )


### Indicador de degradação florestal

### 9. Calcular área de degradação florestal acumulada no período de 2018-2023 no buffer 5-10 km. (Para isso: degrad bf 0-10 - degrad bf 0-5)

bf10$dg_5_10h <- round(bf10$dgacha10 - bf10$dgacha05, 1)

###10. Calcular % de degradação florestal acumulada no período de 2018-2023 no buffer 5-10 km (Para isso: area acumulada de degrad / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf10$dg_5_10p <- c(bf10$dg_5_10h / bf10$ar5_10ha )


### Indicador de mineração

### 11. Calcular área de mineração em 2023 no buffer 5-10 km. (Para isso: mine bf 0-10 - mine bf 0-5)

bf10$mi_5_10h <- round(bf10$mi23ha10 - bf10$mi23ha05, 1)


###12. Calcular % de mineração acumulada em 2023 no buffer 5-10 km (Para isso: area mine / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf10$mi_5_10p <- c(bf10$mi_5_10h / bf10$ar5_10ha )


### Indicador de fogo

### 13. Calcular o número de focos de calor no período de 2018-2023 no buffer 5-10 km. (Para isso: focos bf 0-10 - focos bf 0-5)

bf10$fg_5_10n <- c(bf10$fgacnu10 - bf10$fgacnu05)

###14. Calcular a densidade de focos no buffer 5-10 km (Para isso: número de focos / area do buffer). 

bf10$fg_5_10d <- c(bf10$fg_5_10n / bf10$ar5_10ha )


# 15. Salva como shapefile
st_write(bf10, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/shp_joined_faixa_10_and_05_indicadores.shp", append=FALSE)




########################################## Calculando faixa de 10 - 25 km

#16. Juntar os shapes das faixas/buffers 0-10 e 0-25 km em um só para fazer as subtrações de área e número de focos

## Juntando os shapes com base no campo "ID"

library(dplyr)
library(sf)

# Faz o join mantendo a geometria de bf_0_25
shp_joined_25_10 <- bf_0_25 %>%
  left_join(st_drop_geometry(bf_0_10), by = "ID")

colnames(shp_joined_25_10)

# Salva como shapefile
st_write(shp_joined_25_10, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_faixa_25_and_10.shp", append=FALSE)


###17. Calcula os indicadores da faixa 10-25 km

bf25<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_faixa_25_and_10.shp")

### 18. calcular a área do BF 10-25 km  (para isso = area bf 0-25 - area bf 0-10)
bf25$ar10_25h <- round(bf25$arbf25ha - bf25$arbf10ha, 1)


### Indicador de desmatamento

### 19. Calcular área de desmatamento acumulado no período de 2018-2023 no buffer 10-25 km. (Para isso: defor bf 0-25 - defor bf 0-10)

bf25$df10_25h <- round(bf25$dfacha25 - bf25$dfacha10, 1 )


### 20. Calcular % de desmatamento acumulado no período de 2018-2023 no buffer 10-25. (Para isso: area acumulada de defor / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf25$df10_25p <- c(bf25$df10_25h / bf25$ar10_25h )


### Indicador de degradação florestal

###21. Calcular área de degradação florestal acumulada no periodo de 2018-2023 no buffer 10-25 km. (Para isso: degrad bf 0-25 - degrad bf 0-10)

bf25$dg10_25h <- round(bf25$dgacha25 - bf25$dgacha10, 1)


###22. Calcular % de degradação florestal acumulada no periodo de 2018-2023 no buffer 10-25 (Para isso area acumulada de degrad / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf25$dg10_25p <- c(bf25$dg10_25h / bf25$ar10_25h )


### Indicador de mineração

### 23. Calcular área de mineração em 2023 no buffer 10-25 km. (Para isso: mine bf 0-25 - mine bf 0-10)

bf25$mi10_25h <- round(bf25$mi23ha25 - bf25$mi23ha10, 1)


###24. Calcular % de mineração acumulada em 2023 no buffer 10-25 km (Para isso: area mine / area do buffer). 
#Não multiplicar por 100 pq precisamos do indicador entre 0-1

bf25$mi10_25p <- c(bf25$mi10_25h / bf25$ar10_25h)


### Indicador de fogo

### 25. Calcular o número de focos de calor no período de 2018-2023 no buffer 10-25 km. (Para isso: focos bf 0-25 - focos bf 0-10)

bf25$fg10_25n <- c(bf25$fgacnu25 - bf25$fgacnu10)

###26. Calcular a densidade de focos no buffer 10-25 km. (Para isso: numero de focos / area do buffer). 

bf25$fg10_25d <- c(bf25$fg10_25n / bf25$ar10_25h )


# 27. Salva como shapefile
st_write(bf25, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_faixa_25_and_10_indicadores.shp", append=FALSE)


##### Calculando indicadores da faixa de 0 - 5 km


### 28. A área do BF 0 - 5 já estava calculada

bf_0_05$arbf05ha


### Indicador de desmatamento

### 29. Calcular a % de desmatamento acumulado no período de 2018-2023 no buffer 0-5 km. (Para isso: somar as % de area de todos os anos)

bf_0_05$df_0_5p <- c(bf_0_05$df18_05 + bf_0_05$df19_05 + bf_0_05$df20_05 + bf_0_05$df21_05 + bf_0_05$df22_05 + bf_0_05$df23_05)


### Indicador de degradação

### 30. Calcular a % de degradação florestal acumulada no período de 2018-2023 no buffer 0-5 km. (Para isso: somar as % de area de todos os anos)

bf_0_05$dg_0_5p <- c(bf_0_05$dg18_05 + bf_0_05$dg19_05 + bf_0_05$dg20_05 + bf_0_05$dg21_05 + bf_0_05$dg22_05 + bf_0_05$dg23_05)


### Indicador de mineração

### 31. Calcular % de mineracao em 2023 no buffer 0-5 km. Já estava calculado desde o preenchimento com o FillCell (Etapa 01), apenas renomear para alinhar com os outros indicadores. 

bf_0_05$mi_0_5p <- c(bf_0_05$map23_05)


### 32. Calcular a densidade de focos de calor acumulados no período de 2018-2023 no buffer 0-5 km. (Para isso: numero total de focos em todos os anos / area do buffer)

bf_0_05$fg_0_5d <- c(bf_0_05$fgacnu05 / bf_0_05$arbf05ha)


# 27. Salva como shapefile
st_write(bf_0_05, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_5/shp_faixa_0_5_indicadores.shp", append=FALSE)


##################################################################################################

###Juntar os arquivos para poder calcular juntos os indices ponderados pela distância dos buffers


## Juntando os shapes com base no campo "ID"

library(dplyr)
library(sf)

bf_05<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_5/shp_faixa_0_5_indicadores.shp")
bf_10<-st_read ("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/shp_joined_faixa_10_and_05_indicadores.shp")
bf_25<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_faixa_25_and_10_indicadores.shp")

class(bf_25$ID)
# Faz o join do shape bf_5 com o shape bf_25, mantendo a geometria de bf_25
shp_joined_25_05 <- bf_25 %>%
  left_join(st_drop_geometry(bf_05), by = "ID")

colnames(shp_joined_25_05)

# Faz o join do shape com a uniao feita acima (bf_5 com o shape bf_25) com o shape bf_10, mantendo a geometria de bf_25
shp_joined_25_10 <-shp_joined_25_05%>%
  left_join(st_drop_geometry(bf_10), by = "ID")

colnames(shp_joined_25_10)

### Seleciona apenas as colunas desejadas para as próximas análises:
## colunas referentes as informações dos agrupamentos indígenas ("SITUACAO_x.x", "AREA_KM2_x.x", "NM_UF_x.x",    "NM_MUN_x.x",   "NM_AGLOM_x.x", "CD_AGLOM", "ID")
## coluna referente ao número de pessoas nos agrupamentos indígenas ("v0001_x.x")
### Colunas referentes aos indicadores ambientais calculados para os diferentes buffers (demais colunas)

class(shp_joined_25_10$ID)

colunas_desejadas<-c("SITUACAO_x.x", "AREA_KM2_x.x", "NM_UF_x.x",    "NM_MUN_x.x",   "NM_AGLOM_x.x", "CD_AGLOM",    
                     "v0001_x.x" , "ID", 
                     "ar10_25h",     "df10_25h",     "df10_25p" , "dg10_25h",     "dg10_25p",     "mi10_25h",     "mi10_25p",     "fg10_25n",     "fg10_25d",
                     "df_0_5p",      "dg_0_5p",      "mi_0_5p",      "fg_0_5d",
                     "df_5_10h", "df_5_10p" ,    "dg_5_10h" ,    "dg_5_10p" ,   "mi_5_10h",     "mi_5_10p",     "fg_5_10n" ,    "fg_5_10d" ,    "geometry"   )

bfs <- shp_joined_25_10[, colunas_desejadas]
colnames(bfs)

# Salva as colunas desejadas como um novo shapefile, que será utilizado nas próximas análises
st_write(bfs, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_all_faixas_indicadores_finais.shp", append=FALSE)



########################################################################################################
### índices de ameaça ambiantal aos agrupamentos indígenas  

# Carrega o shapefile salvo logo acima
bf<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/shp_joined_all_faixas_indicadores_finais.shp")
colnames(bf)

### Renomear nomes de colunas que foram abreviados (opcional)

colnames(bf) <- c("SITUACAO", "AREA_KM2", "NM_UF",    "NM_MUN",   "NM_AGLOM", "CD_AGLOM",    
                  "v0001" , "ID",
                  "ar10_25h",     "df10_25h",     "df10_25p" , "dg10_25h",     "dg10_25p",     "mi10_25h",     "mi10_25p",     "fg10_25n",     "fg10_25d",
                  "df_0_5p",      "dg_0_5p",      "mi_0_5p",      "fg_0_5d",
                  "df_5_10h", "df_5_10p" ,    "dg_5_10h" ,    "dg_5_10p" ,   "mi_5_10h",     "mi_5_10p",     "fg_5_10n" ,    "fg_5_10d" ,    "geometry" )

colnames(bf)

#### Calculando indices

##Índices de Ameaças Ambientais Ponderados por Distância
# Distance-Weighted Environmental Threat Indices

#I_Defor = (%defor buffer1 x 0.5) + (%defor buffer2 x 0.3) + (%defor buffer3 x 0.2)

bf$i_defor <- c((bf$df_0_5p * 0.5) + (bf$df_5_10p * 0.3) + (bf$df10_25p * 0.2))

min(bf$i_defor)
max(bf$i_defor)

#I_Degrad = (%degrad buffer1 x 0.5) + (%degrad buffer2 x 0.3) + (%degrad buffer3 x 0.2)

bf$i_degrad <- c((bf$dg_0_5p * 0.5) + (bf$dg_5_10p * 0.3) + (bf$dg10_25p * 0.2))

min(bf$i_degrad)
max(bf$i_degrad)

#I_Mine = (%mine buffer1 x 0.5) + (%mine buffer2 x 0.3) + (%mine buffer3 x 0.2)

bf$i_mine <- c((bf$mi_0_5p * 0.5) + (bf$mi_5_10p * 0.3) + (bf$mi10_25p * 0.2))

min(bf$i_mine)
max(bf$i_mine)

#I_Fire = (fire density buffer1 x 0.5) + (fire density buffer2 x 0.3) + (fire density buffer3 x 0.2)

bf$i_fire <- c((bf$fg_0_5d * 0.5) + (bf$fg_5_10d * 0.3) + (bf$fg10_25d * 0.2))

min(bf$i_fire)
max(bf$i_fire)


### Normalização da variável de população e dos Índices de Ameaças Ambientais Ponderados por Distância
## Usando o método de normalização Min-Máx


#Pop_norm = (Pop - Pop_min) / (Pop_max - Pop_min)
bf$pop_norm<-c(((bf$v0001 - min(bf$v0001)) / (max(bf$v0001) - min(bf$v0001))))
max(bf$pop_norm)
min(bf$pop_norm)

## Defor
bf$i_def_n<-c(((bf$i_defor - min(bf$i_defor)) / (max(bf$i_defor) - min(bf$i_defor))))
max(bf$i_def_n)
min(bf$i_def_n)

## Degrad
bf$i_dg_n<-c(((bf$i_degrad - min(bf$i_degrad)) / (max(bf$i_degrad) - min(bf$i_degrad))))
max(bf$i_dg_n)
min(bf$i_dg_n)

## Mining
bf$i_mi_n<-c(((bf$i_mine - min(bf$i_mine)) / (max(bf$i_mine) - min(bf$i_mine))))
max(bf$i_mi_n)
min(bf$i_mi_n)

## Fire
bf$i_fg_n<-c(((bf$i_fire - min(bf$i_fire)) / (max(bf$i_fire) - min(bf$i_fire))))
max(bf$i_fg_n)
min(bf$i_fg_n)


# Calculando o Índice Composto de Ameaças Ambientais (CETI)
# Composite Environmental Threat Index (CETI)
# O CETI é calculado como a soma ponderada dos Índices de Ameaças Ambientais Ponderados por Distância normalizados
# Pesos adaptados de Rorato et al., 2022
#CETI = (IDefornorm X 0.282) + (IDegradnorm  X 0.261) + (IMiningnorm X 0.290) + (IFirenorm X 0.166)


bf$CETI <- c((bf$i_def_n * 0.282) + (bf$i_dg_n * 0.261) + (bf$i_mi_n * 0.290) + (bf$i_fg_n * 0.166))


# Salva o arquivo com os novos índices calculados como shapefile
st_write(bf, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/buffers_agrupamentos_indigenas_indices_finais_risco_socio_ambiental.shp", append=FALSE)


#### Salvar o arquivo com os novos índices calculados como uma tabela em excel 

# Remover a geometria (pois Excel não aceita colunas espaciais diretamente)
shp_df <- bf %>% st_drop_geometry()

#install.packages("writexl") 
library(writexl)  # para salvar em Excel

# Salvar em Excel
write_xlsx(shp_df, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/buffers_agrupamentos_indigenas_indices_finais_risco_socio_ambiental.xlsx")

############################################## FIM   ############################
