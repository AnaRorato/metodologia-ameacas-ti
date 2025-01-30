########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

### Zonas de buffer (faixas) concêntricas no entorno dos agrupamentos indígenas 
# 0-25km 

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
bf_0_25<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/buffer_zones_agrupamentos_indigenas/buffer_0_25_km_agrupamentos_indigenas_BR_censo_2022_plus_POP_AMZL_new_polyconic.shp")

#4. Verificar a estrutura do shapefile para entender como as colunas estão organizadas
head(bf_0_25)
colnames(bf_0_25) 

#5. Criar um subconjunto com as colunas de interesse: 
#Neste caso, selecionando apenas as colunas de identificação dos agrupamentos indígenas
#e para as variáveis ambientais considerando apenas as ameaças e anos de interesse para criação dos indicadores
#prefixo def__ para desmatamento
#prefixo dg para degradação florestal
#prefixo map23_ para mineração 
#prefixo fire_ para fogo
# v0001 é o dado de total de pessoas no agrupamento indígena (dado do censo do IBGE 2022)

colunas_desejadas25<-c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                     "def__18",    "def__19",    "def__20", "def__21",    "def__22",    "def__23", 
                     "map23_30",
                     "dg2018_1",  "dg2019_1", "dg2020_1", "dg2021_1", "dg2022_1", "dg2023_1",
                     "fire_2018",  "fire_2019",  "fire_2020",  "fire_2021",  "fire_2022",  "fire_2023",  "geometry")

bf25 <- bf_0_25[, colunas_desejadas25]

colnames(bf25)

## Renomear as colunas para identificar a faixa/buffer para posteriormente unir os arquivos das diferentes faixas.
# Sufixo "_25" identifica a faixa/buffer 0 a 25 km

colnames(bf25) <- c("SITUACAO", "AREA_KM2", "NM_UF","NM_MUN", "NM_AGLOM", "CD_AGLOM", "v0001",
                    "df18_25",  "df19_25",  "df20_25", "df21_25",  "df22_25",  "df23_25",
                    "map23_25",
                    "dg18_25",  "dg19_25", "dg20_25", "dg21_25", "dg22_25", "dg23_25",
                    "fg18_25",  "fg19_25",  "fg20_25",  "fg21_25",  "fg22_25",  "fg23_25", "geometry")

colnames(bf25)

###6. Calcular a área do buffer de 0 a 25 km:

# Calcular a área do buffer de 0 a 25 km em m²

bf25$arbf25m <- st_area(bf25)

# Se quiser em hectares (1 ha = 10.000 m²)

bf25$arbf25ha <- as.numeric(st_area(bf25)) / 10000

##7. Calcular a área de mineração em ha. 
#Multiplicar a área do bf pela % de mineração que já está entre 0 e 1
## Aqui não precisa dividir por 100, pq o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf25$mi23ha25<-c(bf25$map23_25*bf25$arbf25ha)

##8. Calcular a área desmatada em hectares em cada ano
## Aqui não precisa dividir por 100, pq o valor da % já está entre 0 e 1. (Já veio assim do FillCell - Etapa 01)

bf25$df18ha25<-c(bf25$df18_25*bf25$arbf25ha)
bf25$df19ha25<-c(bf25$df19_25*bf25$arbf25ha)
bf25$df20ha25<-c(bf25$df20_25*bf25$arbf25ha)
bf25$df21ha25<-c(bf25$df21_25*bf25$arbf25ha)
bf25$df22ha25<-c(bf25$df22_25*bf25$arbf25ha)
bf25$df23ha25<-c(bf25$df23_25*bf25$arbf25ha)

##9. Calcular a área desmatada em hectares no período acumulado. 
#Apenas somar a área de desmatamento de cada ano

bf25$dfacha25<-c(bf25$df18ha25 + bf25$df19ha25 + bf25$df20ha25 + bf25$df21ha25 + bf25$df22ha25 + bf25$df23ha25)


##10. Calcular a área de degradação florestal em hectares em cada ano

bf25$dg18ha25<-c(bf25$dg18_25*bf25$arbf25ha)
bf25$dg19ha25<-c(bf25$dg19_25*bf25$arbf25ha)
bf25$dg20ha25<-c(bf25$dg20_25*bf25$arbf25ha)
bf25$dg21ha25<-c(bf25$dg21_25*bf25$arbf25ha)
bf25$dg22ha25<-c(bf25$dg22_25*bf25$arbf25ha)
bf25$dg23ha25<-c(bf25$dg23_25*bf25$arbf25ha)

##11. Calcular a área de degradação florestal acumulada no período 
#Apenas somar a área de degradação florestal de cada ano

bf25$dgacha25<-c(bf25$dg18ha25 + bf25$dg19ha25 + bf25$dg20ha25 + bf25$dg21ha25 + bf25$dg22ha25 + bf25$dg23ha25)


##12. Calcular o número total de focos de calor acumulado no período
#Apenas somar o número de focos de calor de cada ano

bf25$fgacnu25<- c(bf25$fg18_25 + bf25$fg19_25 + bf25$fg20_25 + bf25$fg21_25 + bf25$fg22_25 + bf25$fg23_25)


###13. Criando uma coluna de ID númerica sequencial para cada agrupamento

library(dplyr)

bf25 <- bf25 %>%
  mutate(ID = as.character(seq_len(n())))


head(bf25)

###14. Salvando o shape apenas com essas informações (colunas selecionadas no bf5 e novas colunas criadas posteriormente)


st_write(bf25,"C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/faixa_25_variaveis.shp", "bf25", driver="ESRI Shapefile")

# 15. Para converter o dado em uma tabela e salvar como .XLSX

dados_tabela <- st_drop_geometry(bf25)

write.xlsx(dados_tabela, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/faixa_0_25/faixa_25_variaveis.xlsx")


############################################## FIM   ############################


