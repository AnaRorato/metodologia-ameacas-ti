########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

# Componente de Exposição
# Buffer zone (BF) de 10km no entorno das TIS 


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
library(tidyr)
library(dplyr)
library(sp)

#2. carrega o diretório de trabalho
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km")


#3.Carrega o shapefile com os dados.Esse shapefile já tem os dados das ameaças preenchidos para a TI e agora tem para o BT (limite da TI + BF de 10km)). 
#Os polígonos correspondem ao BT.

bt<-shapefile("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_preenchido.shp")


#4. Calcular área do BT
# Calcular a área do BT em m²

bt$areaBTm2 <- gArea(bt, byid = TRUE)

# Se quiser em hectares (1 ha = 10.000 m²)

bt$areaBTha <- c(as.numeric(bt$areaBTm2) / 10000)


#5. Verificar a estrutura do shapefile para entender como as colunas estão organizadas
names(bt@data)


#6.Juntar esse shapefile com outro shapefile com os nomes das TIS e outros campos em formato string corrigidos. 
#Alguns processamentos degradam esses campos

#Shape das TIS com os nomes corrigidos
tis<-shapefile("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/TIS/tis_amazonia_legal_poligonais/tis_amazonia_legal_poligonaisPolygon_nomes.shp")

#7. Calcular a área em metros quadrados para as TIS e criar nova coluna

# Certifique-se de que o shapefile está em uma projeção com unidades métricas (ex: UTM) para cálculo correto de área
# Verifique com:
#crs(tis)

# Calcular a área em m²

tis$areaTIm2 <- gArea(tis, byid = TRUE)

# Se quiser em hectares (1 ha = 10.000 m²)

tis$areaTIha <- c(as.numeric(tis$areaTIm2) / 10000)


## 8. Juntando os dois shapefiles com base no campo "gid"

# Colocando o shapefile TIs na frente, garantimos que o shapefile unido vai ter a geometria das TIs e não do BT
shp_joined <- merge(tis, bt, by = "gid")

names(shp_joined@data)

## Salvar o novo shapefile, incluindo agora os nomes corrigidos e as colunas de área das TIs calculadas

writeOGR(shp_joined,"C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_analises.shp", "shp_joined", driver="ESRI Shapefile")


##############################################################################################################
##### Calculando os indicadores de Exposição


#1.Carrega o shapefile com os dados salvos logo acima
## Esse shapefile já possui os dados preenchidos para a TI e agora tem para o BT. Campos string estão corrigidos. 

tis<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_analises.shp")

colnames(tis)
head(tis)

#2. Criar um subconjunto com as colunas de interesse. Neste caso, apenas paras as colunas de identificação da TI e variáveis que darão origem aos indicadores de exposição


colunas_desejadas<-c("gid", "terrai_nom", "etnia_nome", "municipio_", "fase_ti",  "modalidade", "cr",  
                     "undadm_nom", "undadm_sig", "areaTIm2",   "areaTIha","terr_cd",  "uf_sigl",  
                     "mp23_30" , "btmap23_30",  
                     "dg2018", "dg2019", "dg2020", "dg2021", "dg2022", "dg2023",
                     "def_18", "def_19", "def_20", "def_21", "def_22", "def_23",
                     "fr_2018",    "fr_2019", "fr_2020",  "fr_2021",  "fr_2022",  "fr_2023",
                     "btdg18_1", "btdg19_1", "btdg20_1", "btdg21_1", "btdg22_1", "btdg23_1",
                     "btdef__18",  "btdef__19",  "btdef__20",  "btdef__21",  "btdef__22", "btdef__23",
                     "btmap23_30",
                     "btfire18",   "btfire19",   "btfire20",   "btfire21",   "btfire22",   "btfire23" ,
                     "areaBTm2",   "areaBTha" ,
                     "geometry")  

ti <- tis[, colunas_desejadas]
colnames(ti)

head(ti)

##3. calcular a area do BF em hectares (area BF = area BT - area TI)

ti$areaBFha = c(ti$areaBTha - ti$areaTIha)

#################################################################
####################### Cálculo do indicador de exposição à mineração


## 1. Calcular a área de mineração em hectares para a TI
## Aqui não precisa dividir por 100, pq o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)
# ti$mp23_30 é o valor de mineracao na TI calculado anteriormente para o poligono da TI

ti$ami23ti<-c(round(ti$mp23_30*ti$areaTIha, digits = 2))

#2. Calcular a área de mineração em hectares para o BT (area de mineração no BT x área do BT)
## Aqui não precisa dividir por 100, pq o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)
#"btmap23_30" é o valor de mineracao no BT (buffer + TI) calculado anteriormente para o poligono do BT

ti$ami23bt<-c(round(ti$btmap23_30*ti$areaBTha, digits = 2))

##3. Calcular a área de mineração em hectares para o BF (BF = BT - TI)

ti$ami23bf<-c(round(ti$ami23bt - ti$ami23ti, digits = 1))

##4. Calcular a área de mineração em % para o BF em 2023
# Não multiplicar por 100, para manter o indicador entre 0-1

### Indicador de exposição à mineração
ti$ex_mine<-c(ti$ami23bf / ti$areaBFha)

##################################################################################
####################### Cálculo do indicador de exposição à degradação florestal

##1. Calcular a área de degradação florestal em hectares para a TI
## Aqui não precisa dividir por 100, para o valor da % ficar entre 0 e 1
# São os valores de degradação na TI calculados anteriormente para o poligono da TI

ti$adg18ti<-c(round(ti$dg2018*ti$areaTIha, digits = 2))
ti$adg19ti<-c(round(ti$dg2019*ti$areaTIha, digits = 2))
ti$adg20ti<-c(round(ti$dg2020*ti$areaTIha, digits = 2))
ti$adg21ti<-c(round(ti$dg2021*ti$areaTIha, digits = 2))
ti$adg22ti<-c(round(ti$dg2022*ti$areaTIha, digits = 2))
ti$adg23ti<-c(round(ti$dg2023*ti$areaTIha, digits = 2))


#2.Calcular a área de degradação florestal em hectares para o BT
## Aqui não precisa dividir por 100, para o valor da % ficar entre 0 e 1

# area de degradação florestal no BT x área do BT
ti$adg18bt<-c(round(ti$btdg18_1*ti$areaBTha, digits = 2))
ti$adg19bt<-c(round(ti$btdg19_1*ti$areaBTha, digits = 2))
ti$adg20bt<-c(round(ti$btdg20_1*ti$areaBTha, digits = 2))
ti$adg21bt<-c(round(ti$btdg21_1*ti$areaBTha, digits = 2))
ti$adg22bt<-c(round(ti$btdg22_1*ti$areaBTha, digits = 2))
ti$adg23bt<-c(round(ti$btdg23_1*ti$areaBTha, digits = 2))

##3. Calcular a área de degradação florestal anual em hectares para o BF (BF = BT - TI)

ti$adg18bf<-c(round(ti$adg18bt - ti$adg18ti, digits = 2))
ti$adg19bf<-c(round(ti$adg19bt - ti$adg19ti, digits = 2))
ti$adg20bf<-c(round(ti$adg20bt - ti$adg20ti, digits = 2))
ti$adg21bf<-c(round(ti$adg21bt - ti$adg21ti, digits = 2))
ti$adg22bf<-c(round(ti$adg22bt - ti$adg22ti, digits = 2))
ti$adg23bf<-c(round(ti$adg23bt - ti$adg23ti, digits = 2))


##4. Calcular a área acumulada de degradação florestal no periodo (2018-2023) em ha no BF

ti$adgperbf<-c(ti$adg18bf + ti$adg19bf + ti$adg20bf + ti$adg21bf + ti$adg22bf + ti$adg23bf)


##5. Calcular a % de área com degradação florestal acumulada no período (2018-2023) no BF
## Não multiplicar por 100, para manter o indicador entre 0-1

# Indicador de exposição à degradação florestal 
ti$ex_degrad<-c((ti$adgperbf) / ti$areaBFha)

#######################################################################################
####################### Cálculo do indicador de exposição ao desmatamento

##1. Calcular a área de desmatamento em hectares para a TI
## Aqui não precisa dividir por 100, para o valor da % ficar entre 0 e 1
# São os valores de desmatamento na TI calculados anteriormente para o poligono da TI

ti$adf18ti<-c(round(ti$def_18*ti$areaTIha, digits = 2))
ti$adf19ti<-c(round(ti$def_19*ti$areaTIha, digits = 2))
ti$adf20ti<-c(round(ti$def_20*ti$areaTIha, digits = 2))
ti$adf21ti<-c(round(ti$def_21*ti$areaTIha, digits = 2))
ti$adf22ti<-c(round(ti$def_22*ti$areaTIha, digits = 2))
ti$adf23ti<-c(round(ti$def_23*ti$areaTIha, digits = 2))

#2.Calcular a área de desmatamento em hectares para o BT
## Aqui não precisa dividir por 100, para o valor da % ficar entre 0 e 1

# area de desmatamento no BT x área do BT
ti$adf18bt<-c(round(ti$btdef__18*ti$areaBTha, digits = 2))
ti$adf19bt<-c(round(ti$btdef__19*ti$areaBTha, digits = 2))
ti$adf20bt<-c(round(ti$btdef__20*ti$areaBTha, digits = 2))
ti$adf21bt<-c(round(ti$btdef__21*ti$areaBTha, digits = 2))
ti$adf22bt<-c(round(ti$btdef__22*ti$areaBTha, digits = 2))
ti$adf23bt<-c(round(ti$btdef__23*ti$areaBTha, digits = 2))


##3. Calcular a área de desmatamento em hectares para o BF (BF = BT - TI)

ti$adf18bf<-c(round(ti$adf18bt - ti$adf18ti, digits = 2))
ti$adf19bf<-c(round(ti$adf19bt - ti$adf19ti, digits = 2))
ti$adf20bf<-c(round(ti$adf20bt - ti$adf20ti, digits = 2))
ti$adf21bf<-c(round(ti$adf21bt - ti$adf21ti, digits = 2))
ti$adf22bf<-c(round(ti$adf22bt - ti$adf22ti, digits = 2))
ti$adf23bf<-c(round(ti$adf23bt - ti$adf23ti, digits = 2))

##4. Calcular a área acumulada de desmatamento no periodo (2018-2023) em ha no BF

ti$adfperbf<-c(ti$adf18bf + ti$adf19bf + ti$adf20bf + ti$adf21bf + ti$adf22bf + ti$adf23bf)

##5. Calcular a % de área com desmatamento acumulado no período (2018-2023) no BF
## Não multiplicar por 100, para manter o indicador entre 0-1

ti$ex_defor<-c((ti$adfperbf) / ti$areaBFha)

############################################################################################
####################### Cálculo do indicador de exposição ao fogo

#1. Soma do número de focos de calor acumulados no período na TI

ti$fr1823ti<- c(ti$fr_2018 + ti$fr_2019 + ti$fr_2020 + ti$fr_2021 + ti$fr_2022 + ti$fr_2023)

#2. Soma do número de focos de calor acumulados no período no BT

ti$fr1823bt<- c(ti$btfire18 + ti$btfire19 + ti$btfire20 + ti$btfire21 + ti$btfire22 + ti$btfire23)

#3. Cálculo do número de focos de calor acumulados no período no BF (focos no BT - focos na TI)

ti$fr1823bf<- c(ti$fr1823bt - ti$fr1823ti)

##4. Calcular a densidade de focos de calor no período no BF (numero focos no bf / area do BF)

### Indicador de exposição ao fogo
ti$ex_fire<- c(ti$fr1823bf / ti$areaBFha)


###############################################################################################################

### Normalização de todos os indicadores entre 0-1 usando o método Min-Max
#### Importante para combinar os indicadores no índice composto de exposição ambiental

ti$exdeforn <- c((ti$ex_defor - min(ti$ex_defor)) / (max(ti$ex_defor) - min(ti$ex_defor)))

ti$exdegran <- c((ti$ex_degrad - min(ti$ex_degrad)) / (max(ti$ex_degrad) - min(ti$ex_degrad)))

ti$exminen <- c((ti$ex_mine - min(ti$ex_mine)) / (max(ti$ex_mine) - min(ti$ex_mine)))

ti$exfiren <- c((ti$ex_fire - min(ti$ex_fire)) / (max(ti$ex_fire) - min(ti$ex_fire)))


###################################### Cálculo do Índice composto de exposição ambiental 

### Soma dos produtos de cada indice de exposição x o peso da ameaça atribuído por especialistas
### Pesos adaptados de Rorato et al, 2022

#### Pesos:
#defor = 0.282
#degrad = 0.261
#mine = 0.290
#fogo = 0.166

#Índice composto de exposição ambiental
ti$ex_amb <- c((ti$exdeforn * 0.282) + (ti$exdegran * 0.261)  + (ti$exminen * 0.290) + (ti$exfiren * 0.166))

min(ti$ex_amb)
max(ti$ex_amb)

#Para converter o dado do shapefile (com as colunas antigas e as novas) em uma tabela e salvar como .XLSX
dados_tabela <- st_drop_geometry(ti)

write.xlsx(dados_tabela, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_analises_indicadores_exposicao.xlsx")


###. Salvar o shapefile final com os indicadores de exposição gerados
st_write(ti,"C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_ti_10km/resultados/buffer_tis_bt_10km_amazonia_legal_poligonais_policonic_analises_indicadores_exposicao.shp", layer_options = "ENCODING=UTF-8")

####### FIM