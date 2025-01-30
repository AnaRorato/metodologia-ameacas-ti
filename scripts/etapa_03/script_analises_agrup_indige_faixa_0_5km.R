########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

### Zonas de buffer (faixas) concêntricas no entorno dos agrupamentos indígenas 
# 0-5km

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

#3.Carrega o shapefile com os dados
bf_0_5<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_zones_agrupamentos_indigenas/buffer_0_5_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_polyconic.shp")

#4. Verificar a estrutura do shapefile para entender como as colunas estão organizadas
head(bf_0_5)
colnames(bf_0_5) 

#5. Criar um subconjunto com as colunas de interesse: 
#Neste caso, selecionando apenas as colunas de identificação dos agrupamentos indígenas
#e para as variáveis ambientais considerando apenas as ameaças e anos de interesse para criação dos indicadores
#prefixo def__ para desmatamento
#prefixo dg para degradação florestal
#prefixo map23_ para mineração 
#prefixo fire_ para fogo
# v0001 é o dado de total de pessoas no agrupamento indígena (dado do censo do IBGE 2022)


colunas_desejadas<-c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                     "def__18",    "def__19",    "def__20", "def__21",    "def__22",    "def__23",
                     "map23_30",
                     "dg2018_1",  "dg2019_1", "dg2020_1", "dg2021_1", "dg2022_1", "dg2023_1",
                     "fire_2018",  "fire_2019",  "fire_2020",  "fire_2021",  "fire_2022",  "fire_2023",  "geometry")

bf5 <- bf_0_5[, colunas_desejadas]
colnames(bf5)


## Renomear as colunas para identificar a faixa/buffer para posteriormente unir os arquivos das diferentes faixas.
# Sufixo "_05" identifica a faixa/buffer 0 a 5 km

colnames(bf5) <- c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                   "df18_05",    "df19_05",    "df20_05", "df21_05",    "df22_05",    "df23_05", 
                   "map23_05",
                   "dg18_05",  "dg19_05", "dg20_05", "dg21_05", "dg22_05", "dg23_05",
                   "fg18_05",  "fg19_05",  "fg20_05",  "fg21_05",  "fg22_05",  "fg23_05", "geometry")

colnames(bf5)

## 6. Calcular a área do buffer de 0 a 5 km:

# Calcular a área do buffer 0 a 5 km em m²

bf5$arbf05m <- st_area(bf5)

# Se quiser em hectares (1 ha = 10.000 m²)

bf5$arbf05ha <- as.numeric(st_area(bf5)) / 10000

## 7. Calcular a área de mineração em ha: 
# Multiplicar a área do bf pela % de mineração 
## Aqui não precisa dividir por 100, pois o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf5$mi23ha05<-c(bf5$map23_05*bf5$arbf05ha)

##8. Calcular a área desmatada em hectares em cada ano:
# Multiplicar a área do bf pela % de desmatamento que já está entre 0 e 1
## Aqui não precisa dividir por 100, pois o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf5$df18ha05<-c(bf5$df18_05*bf5$arbf05ha)
bf5$df19ha05<-c(bf5$df19_05*bf5$arbf05ha)
bf5$df20ha05<-c(bf5$df20_05*bf5$arbf05ha)
bf5$df21ha05<-c(bf5$df21_05*bf5$arbf05ha)
bf5$df22ha05<-c(bf5$df22_05*bf5$arbf05ha)
bf5$df23ha05<-c(bf5$df23_05*bf5$arbf05ha)

##9. Calcular a área desmatada em hectares no período acumulado. 
#Apenas somar a área de desmatamento de cada ano

bf5$dfacha05<-c(bf5$df18ha05 + bf5$df19ha05 + bf5$df20ha05 + bf5$df21ha05 + bf5$df22ha05 + bf5$df23ha05)


##10. Calcular a área de degradação florestal em hectares em cada ano
# Multiplicar a área do bf pela % de degradação florestal que já está entre 0 e 1
## Aqui não precisa dividir por 100, pois o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf5$dg18ha05<-c(bf5$dg18_05*bf5$arbf05ha)
bf5$dg19ha05<-c(bf5$dg19_05*bf5$arbf05ha)
bf5$dg20ha05<-c(bf5$dg20_05*bf5$arbf05ha)
bf5$dg21ha05<-c(bf5$dg21_05*bf5$arbf05ha)
bf5$dg22ha05<-c(bf5$dg22_05*bf5$arbf05ha)
bf5$dg23ha05<-c(bf5$dg23_05*bf5$arbf05ha)


##11. Calcular a área de degradação florestal acumulada no período 
#Apenas somar a área de degradação florestal de cada ano

bf5$dgacha05<-c(bf5$dg18ha05 + bf5$dg19ha05 + bf5$dg20ha05 + bf5$dg21ha05 + bf5$dg22ha05 + bf5$dg23ha05)


##12. Calcular o número total de focos de calor acumulado no período
#Apenas somar o número de focos de calor de cada ano

bf5$fgacnu05<- c(bf5$fg18_05 + bf5$fg19_05 + bf5$fg20_05 + bf5$fg21_05 + bf5$fg22_05 + bf5$fg23_05)



##13. Criando uma coluna de ID númerica sequencial para cada agrupamento

library(dplyr)


bf5 <- bf5 %>%
  mutate(ID = as.character(seq_len(n())))


head(bf5)

###14. Salvando um novo shapefile apenas com essas informações (colunas selecionadas no bf5 e novas colunas criadas posteriormente)


st_write(bf5,"C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_5/faixa_5_variaveis.shp", "bf5", driver="ESRI Shapefile")


# 15. Para converter o dado em uma tabela e salvar como .XLSX

dados_tabela <- st_drop_geometry(bf5)

write.xlsx(dados_tabela, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_5/faixa_5_variaveis.xlsx")

############################################## FIM   ############################



