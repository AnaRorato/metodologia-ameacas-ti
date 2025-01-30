########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

### Zonas de buffer (faixas) concêntricas no entorno dos agrupamentos indígenas 
# 0-10km

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
bf_0_10<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_zones_agrupamentos_indigenas/buffer_0_10_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_polyconic.shp")

#4. Verificar a estrutura do shapefile para entender como as colunas estão organizadas
head(bf_0_10)
colnames(bf_0_10) 

#5. Criar um subconjunto com as colunas de interesse: 
#Neste caso, selecionando apenas as colunas de identificação dos agrupamentos indígenas
#e para as variáveis ambientais considerando apenas as ameaças e anos de interesse para criação dos indicadores
#prefixo def__ para desmatamento
#prefixo dg para degradação florestal
#prefixo map23_ para mineração 
#prefixo fire_ para fogo
# v0001 é o dado de total de pessoas no agrupamento indígena (dado do censo do IBGE 2022)

colunas_desejadas10<-c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                     "def__18",    "def__19",    "def__20", "def__21",    "def__22",    "def__23", 
                     "map23__30",
                     "dg2018_1",  "dg2019_1", "dg2020_1", "dg2021_1", "dg2022_1", "dg2023_1",
                     "fire_2018",  "fire_2019",  "fire_2020",  "fire_2021",  "fire_2022",  "fire_2023",  "geometry")

bf10 <- bf_0_10[, colunas_desejadas10]
colnames(bf10)

## Renomear as colunas para identificar a faixa/buffer para posteriormente unir os arquivos das diferentes faixas.
# Sufixo "_10" identifica a faixa/buffer 0 a 10 km

colnames(bf10) <- c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                    "df18_10",  "df19_10",  "df20_10", "df21_10",  "df22_10",  "df23_10", 
                    "map23_10",
                    "dg18_10",  "dg19_10", "dg20_10", "dg21_10", "dg22_10", "dg23_10",
                    "fg18_10",  "fg19_10",  "fg20_10",  "fg21_10",  "fg22_10",  "fg23_10", "geometry")

colnames(bf10)

###6. Calcular a área do buffer de 0 a 10 km:

# Calcular a área do buffer de 0 a 10 km em m²

bf10$arbf10m <- st_area(bf10)

# Se quiser em hectares (1 ha = 10.000 m²)

bf10$arbf10ha <- as.numeric(st_area(bf10)) / 10000

##7. Calcular a área de mineração em ha. 
#Multiplicar a área do bf pela % de mineração que já está entre 0 e 1
## Aqui não precisa dividir por 100, pois o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf10$mi23ha10<-c(bf10$map23_10*bf10$arbf10ha)

##8. Calcular a área desmatada em hectares em cada ano
## Aqui não precisa dividir por 100, pq o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf10$df18ha10<-c(bf10$df18_10*bf10$arbf10ha)
bf10$df19ha10<-c(bf10$df19_10*bf10$arbf10ha)
bf10$df20ha10<-c(bf10$df20_10*bf10$arbf10ha)
bf10$df21ha10<-c(bf10$df21_10*bf10$arbf10ha)
bf10$df22ha10<-c(bf10$df22_10*bf10$arbf10ha)
bf10$df23ha10<-c(bf10$df23_10*bf10$arbf10ha)

##9. Calcular a área desmatada em hectares no período acumulado. 
#Apenas somar a área de desmatamento de cada ano

bf10$dfacha10<-c(bf10$df18ha10 + bf10$df19ha10 + bf10$df20ha10 + bf10$df21ha10 + bf10$df22ha10 + bf10$df23ha10)


##10. Calcular a área de degradação florestal em hectares em cada ano

bf10$dg18ha10<-c(bf10$dg18_10*bf10$arbf10ha)
bf10$dg19ha10<-c(bf10$dg19_10*bf10$arbf10ha)
bf10$dg20ha10<-c(bf10$dg20_10*bf10$arbf10ha)
bf10$dg21ha10<-c(bf10$dg21_10*bf10$arbf10ha)
bf10$dg22ha10<-c(bf10$dg22_10*bf10$arbf10ha)
bf10$dg23ha10<-c(bf10$dg23_10*bf10$arbf10ha)

##11. Calcular a área de degradação florestal acumulada no período 
#Apenas somar a área de degradação florestal de cada ano

bf10$dgacha10<-c(bf10$dg18ha10 + bf10$dg19ha10 + bf10$dg20ha10 + bf10$dg21ha10 + bf10$dg22ha10 + bf10$dg23ha10)


##12. Calcular o número total de focos de calor acumulado no período
#Apenas somar o número de focos de calor de cada ano


bf10$fgacnu10<- c(bf10$fg18_10 + bf10$fg19_10 + bf10$fg20_10 + bf10$fg21_10 + bf10$fg22_10 + bf10$fg23_10)


###13. Criando uma coluna de ID númerica sequencial para cada agrupamento

library(dplyr)


bf10 <- bf10 %>%
  mutate(ID = as.character(seq_len(n())))


head(bf10)

###14. Salvando o shape apenas com essas informações (colunas selecionadas no bf5 e novas colunas criadas posteriormente)

st_write(bf10,"C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/faixa_10_variaveis.shp", "bf10", driver="ESRI Shapefile")

# 15. Para converter o dado em uma tabela e salvar como .XLSX

dados_tabela <- st_drop_geometry(bf10)

write.xlsx(dados_tabela, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_10/faixa_10_variaveis.xlsx")

############################################## FIM   ############################
