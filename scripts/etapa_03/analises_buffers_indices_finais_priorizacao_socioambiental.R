########################
#Projeto: Protegendo os Territórios Indígenas na Amazônia Brasileira: Uma Metodologia Espacial Baseada em Múltiplos Indicadores para Priorizar Áreas sob Pressão Ambiental
#Rorato et al., 2025.

#### Análises dos resultados e geração da matriz de priorização dos agrupamentos indígenas baseada em quintil

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
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/graficos")

#3.Carrega o shapefile com os índices finais para os buffers de agrupamentos indígenas

gi<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/buffers_agrupamentos_indigenas_indices_finais_risco_socio_ambiental.shp")


colnames(gi)

####4. Análises exploratórias dos dados

## População 

summary(gi$v0001)

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = v0001)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(title = "Histograma de população",
       x = "Total de pessoas",
       y = "Frequência") +
  theme_minimal()

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = log10(v0001 ))) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de população (log10)",
       x = "Total de pessoas",
       y = "Frequência") +
  theme_minimal()


#### Desmatamento

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = i_def_n)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de I_defor_norm",
       x = "i_def_n",
       y = "Frequência") +
  theme_minimal()

#### Degradação florestal

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = i_dg_n)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de I_degrad_norm",
       x = "i_dg_n",
       y = "Frequência") +
  theme_minimal()

#### Fogo

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = i_fg_n)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de I_fire_norm",
       x = "i_fg_n",
       y = "Frequência") +
  theme_minimal()

#### Mineração

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = i_mi_n)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de I_mine_norm",
       x = "i_mi_n",
       y = "Frequência") +
  theme_minimal()



#### CETI

# Gerar histograma básico com ggplot2
ggplot(gi, aes(x = CETI)) +
  geom_histogram(fill = "steelblue", color = "black") +
  labs(title = "Histograma de CETI",
       x = "CETI",
       y = "Frequência") +
  theme_minimal()


################################ GRÁFICO DE QUADRANTES ########################################## 
################################ MATRIZ DE PRIORIZAÇÃO ################################################
# Carregar pacotes necessários
library(openxlsx)
library(dplyr)
library(ggplot2)


# Remove geometria (transforma sf em data.frame)
gis <- st_drop_geometry(gi)

####################### Analisando os histogramas das variáveis
library(patchwork)  # Para combinar gráficos

### Desmatamento e população
 
# Histogramas para i_def_n
 p1 <- ggplot(gis, aes(x = i_def_n)) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "i_def_n (original)", x = "i_def_n", y = "Frequência") +
   theme_minimal()
 
 p2 <- ggplot(gis, aes(x = log10(i_def_n + 1))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_def_n + 1)", x = "log10(i_def_n + 1)", y = "Frequência") +
   theme_minimal()
 
 p3 <- ggplot(gis, aes(x = log10(i_def_n))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_def_n)", x = "log10(i_def_n)", y = "Frequência") +
   theme_minimal()
 
 # Combina os gráficos em um layout
 layout_final <- (p1 | p2 | p3)
 
 # Salva o layout em um arquivo PNG
 ggsave("histogramas_comparacoes_transformacoes_indicadores_defor.png", layout_final, width = 12, height = 10, dpi = 300)
 
 
 ### População
 
 # Histogramas para pop_norm
 p1 <- ggplot(gis, aes(x = pop_norm)) +
   geom_histogram(bins = 50, fill = "darkorange", color = "white") +
   labs(title = "pop_norm (original)", x = "pop_norm", y = "Frequência") +
   theme_minimal()
 
 p2 <- ggplot(gis, aes(x = log10(pop_norm + 1))) +
   geom_histogram(bins = 50, fill = "darkorange", color = "white") +
   labs(title = "log10(pop_norm + 1)", x = "log10(pop_norm + 1)", y = "Frequência") +
   theme_minimal()
 
 p3 <- ggplot(gis, aes(x = log10(pop_norm))) +
   geom_histogram(bins = 50, fill = "darkorange", color = "white") +
   labs(title = "log10(pop_norm)", x = "log10(pop_norm)", y = "Frequência") +
   theme_minimal()
 
 # Combina os gráficos em um layout
 layout_final <- (p1 | p2 | p3)
 
 # Salva o layout em um arquivo PNG
 ggsave("histogramas_comparacoes_transformacoes_indicadores_pop.png", layout_final, width = 12, height = 10, dpi = 300)
 
 
##### Degradação florestal
 
# Histogramas para i_dg_n
 p1 <- ggplot(gis, aes(x = i_dg_n)) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "i_dg_n (original)", x = "i_dg_n", y = "Frequência") +
   theme_minimal()
 
 p2 <- ggplot(gis, aes(x = log10(i_dg_n + 1))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_dg_n + 1)", x = "log10(i_dg_n + 1)", y = "Frequência") +
   theme_minimal()
 
 p3 <- ggplot(gis, aes(x = log10(i_dg_n))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_dg_n)", x = "log10(i_dg_n)", y = "Frequência") +
   theme_minimal()
 
 # Exibir em 3 linhas por 2 colunas
 (p1 | p2 | p3) 
 
 # Combina os gráficos em um layout
 layout_final <- (p1 | p2 | p3)
 
 # Salva o layout em um arquivo PNG
 ggsave("histogramas_comparacoes_transformacoes_indicadores_degrad.png", layout_final, width = 12, height = 10, dpi = 300)
 
 
### FOGO
 
# Histogramas para i_fg_n
p1 <- ggplot(gis, aes(x = i_fg_n)) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "i_fg_n (original)", x = "i_fg_n", y = "Frequência") +
   theme_minimal()
 
 p2 <- ggplot(gis, aes(x = log10(i_fg_n + 1))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_fg_n + 1)", x = "log10(i_fg_n + 1)", y = "Frequência") +
   theme_minimal()
 
 p3 <- ggplot(gis, aes(x = log10(i_fg_n))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_fg_n)", x = "log10(i_fg_n)", y = "Frequência") +
   theme_minimal()
 
 # Combina os gráficos em um layout
 layout_final <- (p1 | p2 | p3)
 
 # Salva o layout em um arquivo PNG
 ggsave("histogramas_comparacoes_transformacoes_indicadores_fogo.png", layout_final, width = 12, height = 10, dpi = 300)
 
#### MINERAÇÃO
 
# Histogramas para i_mi_n
 p1 <- ggplot(gis, aes(x = i_mi_n)) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "i_mi_n (original)", x = "i_mi_n", y = "Frequência") +
   theme_minimal()
 
 p2 <- ggplot(gis, aes(x = log10(i_mi_n + 1))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_mi_n + 1)", x = "log10(i_mi_n + 1)", y = "Frequência") +
   theme_minimal()
 
 p3 <- ggplot(gis, aes(x = log10(i_mi_n))) +
   geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
   labs(title = "log10(i_mi_n)", x = "log10(i_mi_n)", y = "Frequência") +
   theme_minimal()
 
 # Combina os gráficos em um layout
 layout_final <- (p1 | p2 | p3)
 
 # Salva o layout em um arquivo PNG
 ggsave("histogramas_comparacoes_transformacoes_indicadores_mineracao.png", layout_final, width = 12, height = 10, dpi = 300)
 
 
 ######################################################################
 # MATRIZ DE PRIORIZAÇÃO SOCIOAMBIENTAL COM QUINTIS – 25 QUADRANTES
 ######################################################################
 
 # Objetivo:
 # Este script constrói uma matriz de priorização com base na combinação de dois indicadores:
 # 1) Índice de Desmatamento normalizado (i_def_n)
 # 2) População Normalizada (pop_norm)
 
 # Cada eixo (X e Y) é dividido em quintis (Q1 a Q5), totalizando 25 combinações possíveis (Q1-1 a Q5-5).
 # Cada ponto da base de dados (agrupamento indígena) é então alocado em um desses 25 quadrantes.
 
 ## A notação "Q2-4" significa que aquele ponto está localizado no quintil 2 do eixo X (desmatamento) e no quintil 4 do eixo Y (população normalizada).
 # Especificamente:
 #Q2 → o valor de i_def_n (índice de desmatamento) daquele ponto está entre os 20% e 40% inferiores da distribuição (ou seja, no segundo quintil).
 # 4 → o valor de pop_norm (população normalizada) daquele ponto está entre os 60% e 80% superiores da distribuição (ou seja, no quarto quintil).
 
 #O gráfico resultante permite visualizar os pontos com maior prioridade, por exemplo,
 # aqueles com altos valores de desmatamento e população (Q5-5). 
 #Mas também permite visualizar pontos com baixos valores de população, mas altos valores de desmatamento.
 
 # Remove geometria (transforma sf em data.frame)
 gis <- st_drop_geometry(gi)
 
 
 ######################################################################
 # ETAPA 1 – Cálculo dos Quintis de Cada Indicador
 ######################################################################
 
 # Calcula os quintis (20%, 40%, 60%, 80%) para os dois indicadores de interesse.
 q_defor <- quantile(gis$i_def_n, probs = seq(0, 1, 0.2), na.rm = TRUE)  # Quintis do índice de desmatamento normalizado
 q_pop   <- quantile(gis$pop_norm, probs = seq(0, 1, 0.2), na.rm = TRUE) # Quintis da população normalizada
 
 ######################################################################
 # ETAPA 2 – Classificação dos Pontos em Quintis
 ######################################################################
 
 # Função auxiliar que recebe um valor e retorna o número do quintil correspondente,
 # com base nos pontos de corte calculados anteriormente.
 get_quintil <- function(valor, cortes) {
   findInterval(valor, cortes, rightmost.closed = TRUE, all.inside = TRUE)
 }
 
  # Aplica a função para categorizar cada ponto com base nos dois indicadores.
 # Cria uma coluna "grupo_quintil" com a notação Qx-y (ex: Q2-4).
 gis_quintis_defor <- gis %>% 
    mutate(
      quintil_def = if_else(i_def_n == 0, 1L, get_quintil(i_def_n, q_defor)),   # Quintil do índice de desmatamento
      quintil_pop = get_quintil(pop_norm, q_pop),        # Quintil da população normalizada
     grupo_quintil = paste0("Q", quintil_def, "-", quintil_pop)  # Combinação dos dois
   )
 
 
 ######################################################################
 # ETAPA 3 – Preparação de Paleta de Cores e Coordenadas
 ######################################################################
 
 # Lista completa dos 25 grupos possíveis
 quintis_possiveis <- paste0(
   rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5)
 )
 
 # Reordena o fator com os níveis completos (isso força a manutenção da ordem e cores)
 gis_quintis_defor$grupo_quintil <- factor(gis_quintis_defor$grupo_quintil, levels = quintis_possiveis)
 
 # Paleta de cores com 25 cores mapeadas diretamente aos grupos
 cores_25 <- setNames(
   colorRampPalette(c("skyblue", "forestgreen","orange", "firebrick","purple"))(25),
   quintis_possiveis
 )
 
 # Define coordenadas mínimas dos eixos (log-transformadas) para posicionar os rótulos dos eixos
 x_min <- min(log10(gis_quintis_defor$i_def_n), na.rm = TRUE)
 y_min <- min(log10(gis_quintis_defor$pop_norm), na.rm = TRUE)
 
 ######################################################################
 # ETAPA 4 – Construção do Gráfico com as Linhas de Quintis e Anotações
 ######################################################################
 
grafico1<- ggplot(gis_quintis_defor, aes(x = log10(i_def_n), y = log10(pop_norm), color = grupo_quintil)) +
   geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
   
   # Adiciona linhas pontilhadas nos pontos de corte dos quintis
   geom_vline(xintercept = log10(q_defor[-c(1,6)] ), linetype = "dotted", color = "gray10") +
   geom_hline(yintercept = log10(q_pop[-c(1,6)] ), linetype = "dotted", color = "gray10") +
   
   # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
   annotate("text", x = log10(q_defor[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
   
   annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
   
   # Ajustes estéticos com subscrito nas labels dos eixos
   scale_color_manual(values = cores_25) +
   labs(
     title = "Matriz de Priorização de Agrupamentos Indígenas com Quintis - Desmatamento",
     x = expression(paste("Índice de desmatamento (log" [10], "(I"["Defor_norm"], "))")),
     y = expression(paste("Total de pessoas (log" [10], "(Pop"["norm"], "))")),
     color = "Grupo quintil"
   ) +
   theme_minimal(base_size = 14) +
   theme(
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
     axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
   )
 
 
 plot(grafico1)
 
 # Salvar como PNG
 ggsave(
   filename = "grafico_priorizacao_quintis_defor_PORT.png",
   plot = grafico1,
   width = 10,       # largura em polegadas
   height = 7,       # altura em polegadas
   bg = "white",  # <- ESSENCIAL para fundo branco no PNG
   dpi = 300         # resolução para impressão
 )
 
 
 ### Gráfico em inglês
 
 grafico2<- ggplot(gis_quintis_defor, aes(x = log10(i_def_n), y = log10(pop_norm), color = grupo_quintil)) +
   geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
   
   # Adiciona linhas pontilhadas nos pontos de corte dos quintis
   geom_vline(xintercept = log10(q_defor[-c(1,6)]), linetype = "dotted", color = "gray10") +
   geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
   
   # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
   annotate("text", x = log10(q_defor[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
   annotate("text", x = log10(q_defor[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
   
   annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
   annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
   
  # Ajustes estéticos com subscrito nas labels dos eixos
   scale_color_manual(values = cores_25) +
   labs(
     title = "Prioritization Matrix of Indigenous Groupings with Quintiles – Deforestation",
     x = expression(paste("Deforestation Index (log" [10], "(I"["Defor_norm"], "))")),
     y = expression(paste("Total Population (log" [10], "(Pop"["norm"], "))")),
     color = "Quintile group"
   ) +
   theme_minimal(base_size = 14) +
   theme(
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
     axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
   )
 
 
 plot(grafico2)
 
 # Salvar como PNG
 ggsave(
   filename = "grafico_priorizacao_quintis_defor_EN.png",
   plot = grafico2,
   width = 10,       # largura em polegadas
   height = 7,       # altura em polegadas
   bg = "white",  # <- ESSENCIAL para fundo branco no PNG
   dpi = 300         # resolução para impressão
 )
 
 ######################################################################
 # ETAPA 5 – Geração de Tabela de Frequência por Grupo de Quintil
 ######################################################################
 
 # Conta quantos registros estão em cada grupo_quintil
 tabela_quadrantes_defor <- gis_quintis_defor %>%
   count(grupo_quintil) %>%
   arrange(grupo_quintil)
 
 # Exibe a tabela no console
 print(tabela_quadrantes_defor)
 
 # (Opcional) Exporta a tabela como arquivo XLSX
 write.xlsx(tabela_quadrantes_defor, "contagem_grupo_quintil_defor.xlsx", rowNames = FALSE)
 
 ######################################################################
 # ETAPA 6 – Geração Nova Coluna Usando o Grupo de Quintil
 ######################################################################
 
# Criar nova colunacom a combinação dos dois quintis
 gis_quintis_defor <- gis_quintis_defor %>%
   mutate(Q_defor = grupo_quintil)
 
#Criar nova coluna, no shapefile original 'gi', com a combinação dos dois quintis
gi$Q_defor <- gis_quintis_defor$Q_defor


###################################################################################################
#########################################################################################
#####   MATRIZ de PRIORIZAÇÃO QUINTIS - 25 quadrantes - MINERAÇÃO

hist(log10(gis$i_mi_n))
summary(gis$i_mi_n)
median(gis$i_mi_n)


######################################################################
# ETAPA 1 – Cálculo dos Quintis de Cada Indicador
######################################################################

# Calcula os quintis (20%, 40%, 60%, 80%) para os dois indicadores de interesse.
q_mine <- quantile(gis$i_mi_n, probs = seq(0, 1, 0.2), na.rm = TRUE)  # Quintis do índice de mineração normalizado
q_pop   <- quantile(gis$pop_norm, probs = seq(0, 1, 0.2), na.rm = TRUE) # Quintis da população normalizada

######################################################################
# ETAPA 2 – Classificação dos Pontos em Quintis
######################################################################

# Função auxiliar que recebe um valor e retorna o número do quintil correspondente,
# com base nos pontos de corte calculados anteriormente.
get_quintil <- function(valor, cortes) {
  findInterval(valor, cortes, rightmost.closed = TRUE, all.inside = TRUE)
}

# Aplica a função para categorizar cada ponto com base nos dois indicadores.
# Cria uma coluna "grupo_quintil" com a notação Qx-y (ex: Q2-4).

gis_quintis_mine <- gis %>% 
  mutate(
    quintil_mi = if_else(i_mi_n == 0, 1L, get_quintil(i_mi_n, q_mine)),  # Quintil do índice de mineração normalizado
    quintil_pop = get_quintil(pop_norm, q_pop),                # Quintil da população normalizada
    grupo_quintil = paste0("Q", quintil_mi, "-", quintil_pop)    # Combinação dos dois
  )


######################################################################
# ETAPA 3 – Preparação de Paleta de Cores e Coordenadas
######################################################################

# Lista completa dos 25 grupos possíveis
quintis_possiveis <- paste0(
  rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5)
)

# Reordena o fator com os níveis completos (isso força a manutenção da ordem e cores)
gis_quintis_mine$grupo_quintil <- factor(gis_quintis_mine$grupo_quintil, levels = quintis_possiveis)

# Paleta de cores com 25 cores mapeadas diretamente aos grupos
cores_25 <- setNames(
  colorRampPalette(c("skyblue", "forestgreen", "orange", "firebrick", "purple"))(25),
  quintis_possiveis
)

# Define coordenadas mínimas dos eixos (log-transformadas) para posicionar os rótulos dos eixos
x_min <- min(log10(gis_quintis_mine$i_mi_n), na.rm = TRUE)
y_min <- min(log10(gis_quintis_mine$pop_norm ), na.rm = TRUE)

######################################################################
# ETAPA 4 – Construção do Gráfico com as Linhas de Quintis e Anotações
######################################################################

grafico3 <- ggplot(gis_quintis_mine, aes(x = log10(i_mi_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_mine[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_mine[2]), y = y_min, label = "Q1", vjust = -2, size = 3) +
  annotate("text", x = log10(q_mine[3]), y = y_min, label = "Q2", vjust = -4, size = 3) +
  annotate("text", x = log10(q_mine[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_mine[5]), y = y_min, label = "Q4", vjust = -8, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
 # Ajustes estéticos com subscrito nas labels dos eixos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Matriz de Priorização de Agrupamentos Indígenas com Quintis - Mineração",
    x = expression(paste("Índice de mineração (log" [10], "(I"["Mining_norm"], "))")),
    y = expression(paste("Total de pessoas (log" [10], "(Pop"["norm"], "))")),
    color = "Grupo quintil"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )


plot(grafico3)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_mine_PORT.png",
  plot = grafico3,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)
  

## Gráfico em inglês

grafico4 <- ggplot(gis_quintis_mine, aes(x = log10(i_mi_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_mine[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_mine[2]), y = y_min, label = "Q1", vjust = -2, size = 3) +
  annotate("text", x = log10(q_mine[3]), y = y_min, label = "Q2", vjust = -4, size = 3) +
  annotate("text", x = log10(q_mine[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_mine[5]), y = y_min, label = "Q4", vjust = -8, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos com subscrito nas labels dos eixos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Prioritization Matrix of Indigenous Groupings with Quintiles – Mining",
    x = expression(paste("Mining Index (log" [10], "(I"["Mining_norm"], "))")),
    y = expression(paste("Total population (log" [10], "(Pop"["norm"], "))")),
    color = "Quintile group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
    
  )


plot(grafico4)

summary(gis$i_mi_n)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_mine_ENG.png",
  plot = grafico4,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)
 

######################################################################
# ETAPA 5 – Geração de Tabela de Frequência por Grupo de Quintil
######################################################################

# Conta quantos registros estão em cada grupo_quintil
tabela_quadrantes_mine <- gis_quintis_mine %>%
  count(grupo_quintil) %>%
  arrange(grupo_quintil)

# Exibe a tabela no console
print(tabela_quadrantes_mine)

# (Opcional) Exporta a tabela como arquivo XLSX
write.xlsx(tabela_quadrantes_mine, "contagem_grupo_quintil_mine.xlsx", rowNames = FALSE)


######################################################################
# ETAPA 6 – Geração Nova Coluna Usando o Grupo de Quintil
######################################################################

# Criar nova coluna Q_mine com a combinação dos dois quintis
gis_quintis_mine <- gis_quintis_mine %>%
  mutate(Q_mine = grupo_quintil)

# Criar nova coluna Q_mine, no shapefile original 'gi', com a combinação dos dois quintis
gi$Q_mine <- gis_quintis_mine$Q_mine


######################################################################################################
#########################################################################################
#####   MATRIZ de PRIORIZAÇÂO QUINTIS - 25 quadrantes - DEGRADAÇÃO FLORESTAL

hist(log10(gis$i_dg_n))
summary(gis$i_dg_n)
median(gis$i_dg_n)

######################################################################
# ETAPA 1 – Cálculo dos Quintis de Cada Indicador
######################################################################

# Calcula os quintis (20%, 40%, 60%, 80%) para os dois indicadores de interesse.
q_degrad <- quantile(gis$i_dg_n, probs = seq(0, 1, 0.2), na.rm = TRUE)  # Quintis do índice de degradação florestal normalizado
q_pop   <- quantile(gis$pop_norm, probs = seq(0, 1, 0.2), na.rm = TRUE) # Quintis da população normalizada

######################################################################
# ETAPA 2 – Classificação dos Pontos em Quintis
######################################################################

# Função auxiliar que recebe um valor e retorna o número do quintil correspondente,
# com base nos pontos de corte calculados anteriormente.
get_quintil <- function(valor, cortes) {
  findInterval(valor, cortes, rightmost.closed = TRUE, all.inside = TRUE)
}

# Aplica a função para categorizar cada ponto com base nos dois indicadores.
# Cria uma coluna "grupo_quintil" com a notação Qx-y (ex: Q2-4).
gis_quintis_degrad <- gis %>% 
  mutate(
    quintil_dg = if_else(i_dg_n == 0, 1L, get_quintil(i_dg_n, q_degrad)),  # Quintil da degradação florestal normalizada
    quintil_pop = get_quintil(pop_norm, q_pop),        # Quintil da população normalizada
    grupo_quintil = paste0("Q", quintil_dg, "-", quintil_pop)  # Combinação dos dois
  )


######################################################################
# ETAPA 3 – Preparação de Paleta de Cores e Coordenadas
######################################################################

# Lista completa dos 25 grupos possíveis
quintis_possiveis <- paste0(
  rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5)
)

# Reordena o fator com os níveis completos (isso força a manutenção da ordem e cores)
gis_quintis_degrad$grupo_quintil <- factor(gis_quintis_degrad$grupo_quintil, levels = quintis_possiveis)

# Paleta de cores com 25 cores mapeadas diretamente aos grupos
cores_25 <- setNames(
  colorRampPalette(c("skyblue", "forestgreen", "orange", "firebrick", "purple"))(25),
  quintis_possiveis
)

# Define coordenadas mínimas dos eixos (log-transformadas) para posicionar os rótulos dos eixos
x_min <- min(log10(gis_quintis_degrad$i_dg_n ), na.rm = TRUE)
y_min <- min(log10(gis_quintis_degrad$pop_norm ), na.rm = TRUE)

######################################################################
# ETAPA 4 – Construção do Gráfico com as Linhas de Quintis e Anotações
######################################################################

grafico5 <- ggplot(gis_quintis_degrad, aes(x = log10(i_dg_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_degrad[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_degrad[2]), y = y_min, label = "Q1", vjust = -2, size = 3) +
  annotate("text", x = log10(q_degrad[3]), y = y_min, label = "Q2", vjust = -4, size = 3) +
  annotate("text", x = log10(q_degrad[4]), y = y_min, label = "Q3", vjust = -4, size = 3) +
  annotate("text", x = log10(q_degrad[5]), y = y_min, label = "Q4", vjust = -4, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Matriz de Priorização de Agrupamentos Indígenas com Quintis - Degradação Florestal",
    x = expression(paste("Índice de degradação florestal (log" [10], "(I"["Degrad_norm"], "))")),
    y = expression(paste("Total de pessoas (log" [10], "(Pop"["norm"], "))")),
    color = "Grupo Quintil"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico5)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_degrad_PORT.png",
  plot = grafico5,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)

### Gráfico em inglês

grafico6 <- ggplot(gis_quintis_degrad, aes(x = log10(i_dg_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_degrad[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_degrad[2]), y = y_min, label = "Q1", vjust = -2, size = 3) +
  annotate("text", x = log10(q_degrad[3]), y = y_min, label = "Q2", vjust = -4, size = 3) +
  annotate("text", x = log10(q_degrad[4]), y = y_min, label = "Q3", vjust = -4, size = 3) +
  annotate("text", x = log10(q_degrad[5]), y = y_min, label = "Q4", vjust = -4, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Prioritization Matrix of Indigenous Groupings with Quintiles – Forest Degradation",
    x = expression(paste("Forest Degradation Index (log" [10], "(I"["Degrad_norm"], "))")),
    y = expression(paste("Total population (log" [10], "(Pop"["norm"], "))")),
    color = "Quintile group"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico6)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_degrad_ENG.png",
  plot = grafico6,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)


######################################################################
# ETAPA 5 – Geração de Tabela de Frequência por Grupo de Quintil
######################################################################

# Conta quantos registros estão em cada grupo_quintil
tabela_quadrantes_degrad <- gis_quintis_degrad %>%
  count(grupo_quintil) %>%
  arrange(grupo_quintil)

# Exibe a tabela no console
print(tabela_quadrantes_degrad)

# (Opcional) Exporta a tabela como arquivo XLSX
write.xlsx(tabela_quadrantes_degrad, "contagem_grupo_quintil_degrad.xlsx", rowNames = FALSE)


######################################################################
# ETAPA 6 – Geração Nova Coluna Usando o Grupo de Quintil
######################################################################

# Criar nova coluna Q_degrad com a combinação dos dois quintis
gis_quintis_degrad <- gis_quintis_degrad %>%
  mutate(Q_degrad = grupo_quintil)

# Criar nova coluna Q_mine, no shapefile original 'gi', com a combinação dos dois quintis
gi$Q_degrad <- gis_quintis_degrad$Q_degrad


#########################################################################################
#########################################################################################
#####   MATRIZ de PRIORIZAÇÂO QUINTIS - 25 quadrantes - FOGO

hist(log10(gis$i_fg_n))
hist(gis$i_fg_n)
summary(gis$i_fg_n)
median(gis$i_fg_n)

######################################################################
# ETAPA 1 – Cálculo dos Quintis de Cada Indicador
######################################################################

# Calcula os quintis (20%, 40%, 60%, 80%) para os dois indicadores de interesse.
q_fire <- quantile(gis$i_fg_n, probs = seq(0, 1, 0.2), na.rm = TRUE)  # Quintis do índice de fogo normalizado
q_pop   <- quantile(gis$pop_norm, probs = seq(0, 1, 0.2), na.rm = TRUE) # Quintis da população normalizada

######################################################################
# ETAPA 2 – Classificação dos Pontos em Quintis
######################################################################

# Função auxiliar que recebe um valor e retorna o número do quintil correspondente,
# com base nos pontos de corte calculados anteriormente.
get_quintil <- function(valor, cortes) {
  findInterval(valor, cortes, rightmost.closed = TRUE, all.inside = TRUE)
}

# Aplica a função para categorizar cada ponto com base nos dois indicadores.
# Cria uma coluna "grupo_quintil" com a notação Qx-y (ex: Q2-4).
gis_quintis_fire <- gis %>% 
  mutate(
    quintil_fire = if_else(i_fg_n == 0, 1L, get_quintil(i_fg_n, q_fire)), # Quintil do índice de fogo normalizado
    quintil_pop = get_quintil(pop_norm, q_pop),        # Quintil da população normalizada
    grupo_quintil = paste0("Q", quintil_fire, "-", quintil_pop)  # Combinação dos dois
  )

######################################################################
# ETAPA 3 – Preparação de Paleta de Cores e Coordenadas
######################################################################

# Lista completa dos 25 grupos possíveis
quintis_possiveis <- paste0(
  rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5)
)

# Reordena o fator com os níveis completos (isso força a manutenção da ordem e cores)
gis_quintis_fire$grupo_quintil <- factor(gis_quintis_fire$grupo_quintil, levels = quintis_possiveis)

# Paleta de cores com 25 cores mapeadas diretamente aos grupos
cores_25 <- setNames(
  colorRampPalette(c("skyblue", "forestgreen","orange", "firebrick", "purple"))(25),
  quintis_possiveis
)

# Define coordenadas mínimas dos eixos (log-transformadas) para posicionar os rótulos dos eixos
x_min <- min(log10(gis_quintis_fire$i_fg_n), na.rm = TRUE)
y_min <- min(log10(gis_quintis_fire$pop_norm), na.rm = TRUE)

######################################################################
# ETAPA 4 – Construção do Gráfico com as Linhas de Quintis e Anotações
######################################################################

grafico7 <- ggplot(gis_quintis_fire, aes(x = log10(i_fg_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_fire[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_fire[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Matriz de Priorização de Agrupamentos Indígenas com Quintis - Fogo ",
    x = expression(paste("Índice de fogo (log" [10], "(I"["Fire_norm"], "))")),
    y = expression(paste("Total de pessoas (log" [10], "(Pop"["norm"], "))")),
    color = "Grupo Quintil"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico7)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_fogo_PORT.png",
  plot = grafico7,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)


### Grafico em ingles

grafico8 <- ggplot(gis_quintis_fire, aes(x = log10(i_fg_n), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_fire[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_fire[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_fire[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Prioritization Matrix of Indigenous Groupings with Quintiles – Fire",
    x = expression(paste("Fire Index (log" [10], "(I"["Fire_norm"], "))")),
    y = expression(paste("Total population (log" [10], "(Pop"["norm"], "))")),
    color = "Quintile group"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico8)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_fogo_ENG.png",
  plot = grafico8,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)




######################################################################
# ETAPA 5 – Geração de Tabela de Frequência por Grupo de Quintil
######################################################################

# Conta quantos registros estão em cada grupo_quintil
tabela_quadrantes_fire <- gis_quintis_fire %>%
  count(grupo_quintil) %>%
  arrange(grupo_quintil)

# Exibe a tabela no console
print(tabela_quadrantes_fire)

# (Opcional) Exporta a tabela como arquivo XLSX
write.xlsx(tabela_quadrantes_fire, "contagem_grupo_quintil_fire.xlsx", rowNames = FALSE)


######################################################################
# ETAPA 6 – Geração Nova Coluna Usando o Grupo de Quintil
######################################################################

# Criar nova coluna Q_degrad com a combinação dos dois quintis
gis_quintis_fire <- gis_quintis_fire %>%
  mutate(Q_fire = grupo_quintil)

# Criar nova coluna Q_mine, no shapefile original 'gi', com a combinação dos dois quintis
gi$Q_fire <- gis_quintis_fire$Q_fire



##########################################################################################
#####   MATRIZ de PRIORIZAÇÃO QUINTIS - 25 quadrantes - CETI x pop
#################### INDICE COMPOSTO - CETI #######################################

hist(log10(gis$CETI))
hist(gis$CETI)
summary(gis$CETI)
median(gis$CETI)

######################################################################
# ETAPA 1 – Cálculo dos Quintis de Cada Indicador
######################################################################

# Calcula os quintis (20%, 40%, 60%, 80%) para os dois indicadores de interesse.
q_ceti <- quantile(gis$CETI, probs = seq(0, 1, 0.2), na.rm = TRUE)  # Quintis do índice CETI
q_pop   <- quantile(gis$pop_norm, probs = seq(0, 1, 0.2), na.rm = TRUE) # Quintis da população normalizada

######################################################################
# ETAPA 2 – Classificação dos Pontos em Quintis
######################################################################

# Função auxiliar que recebe um valor e retorna o número do quintil correspondente,
# com base nos pontos de corte calculados anteriormente.
get_quintil <- function(valor, cortes) {
  findInterval(valor, cortes, rightmost.closed = TRUE, all.inside = TRUE)
}

# Aplica a função para categorizar cada ponto com base nos dois indicadores.
# Cria uma coluna "grupo_quintil" com a notação Qx-y (ex: Q2-4).
gis_quintis_ceti <- gis %>% 
  mutate(
    quintil_ceti = if_else(CETI == 0, 1L, get_quintil(CETI, q_ceti)), #Quintis do índice CETI
    quintil_pop = get_quintil(pop_norm, q_pop),        # Quintil da população normalizada
    grupo_quintil = paste0("Q", quintil_ceti, "-", quintil_pop)  # Combinação dos dois
  )

######################################################################
# ETAPA 3 – Preparação de Paleta de Cores e Coordenadas
######################################################################

# Lista completa dos 25 grupos possíveis
quintis_possiveis <- paste0(
  rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5)
)

# Reordena o fator com os níveis completos (isso força a manutenção da ordem e cores)
gis_quintis_ceti$grupo_quintil <- factor(gis_quintis_ceti$grupo_quintil, levels = quintis_possiveis)

# Paleta de cores com 25 cores mapeadas diretamente aos grupos
cores_25 <- setNames(
  colorRampPalette(c("skyblue", "forestgreen","orange", "firebrick", "purple"))(25),
  quintis_possiveis
)

# Define coordenadas mínimas dos eixos (log-transformadas) para posicionar os rótulos dos eixos
x_min <- min(log10(gis_quintis_ceti$CETI), na.rm = TRUE)
y_min <- min(log10(gis_quintis_ceti$pop_norm), na.rm = TRUE)

######################################################################
# ETAPA 4 – Construção do Gráfico com as Linhas de Quintis e Anotações
######################################################################

grafico8 <- ggplot(gis_quintis_ceti, aes(x = log10(CETI), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_ceti[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_ceti[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Matriz de Priorização de Agrupamentos Indígenas com Quintis - CETI ",
    x = expression(paste("Índice composto de ameaças ambientais (log" [10], "(CETI)")),
    y = expression(paste("Total de pessoas (log" [10], "(Pop"["norm"], "))")),
    color = "Grupo Quintil"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico8)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_CETI_PORT.png",
  plot = grafico8,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)

####### parei aqui, checar e salvar e tbm gerar o mapa para CETI


### Grafico em ingles

grafico9 <- ggplot(gis_quintis_ceti, aes(x = log10(CETI), y = log10(pop_norm), color = grupo_quintil)) +
  geom_point(size = 3, alpha = 0.8) +  # Plota os pontos com cor por grupo_quintil
  
  # Adiciona linhas pontilhadas nos pontos de corte dos quintis
  geom_vline(xintercept = log10(q_ceti[-c(1,6)]), linetype = "dotted", color = "gray10") +
  geom_hline(yintercept = log10(q_pop[-c(1,6)]), linetype = "dotted", color = "gray10") +
  
  # Adiciona rótulos nos eixos para indicar os quintis (Q1 a Q4; Q5 é o restante)
  annotate("text", x = log10(q_ceti[2]), y = y_min, label = "Q1", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[3]), y = y_min, label = "Q2", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[4]), y = y_min, label = "Q3", vjust = -6, size = 3) +
  annotate("text", x = log10(q_ceti[5]), y = y_min, label = "Q4", vjust = -6, size = 3) +
  
  annotate("text", x = x_min, y = log10(q_pop[2]), label = "Q1", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[3]), label = "Q2", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[4]), label = "Q3", hjust = -0.1, size = 3) +
  annotate("text", x = x_min, y = log10(q_pop[5]), label = "Q4", hjust = -0.1, size = 3) +
  
  # Ajustes estéticos
  scale_color_manual(values = cores_25) +
  labs(
    title = "Prioritization Matrix of Indigenous Groupings with Quintiles – CETI",
    x = expression(paste("CETI (log" [10], "(CETI)")),
    y = expression(paste("Total population (log" [10], "(Pop"["norm"], "))")),
    color = "Quintile group"
  )   +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 16),  # tamanho do título do eixo X
    axis.title.y = element_text(size = 16)   # tamanho do título do eixo Y
  )

plot(grafico9)

# Salvar como PNG
ggsave(
  filename = "grafico_priorizacao_quintis_CETI_ENG.png",
  plot = grafico9,
  width = 10,       # largura em polegadas
  height = 7,       # altura em polegadas
  bg = "white",  # <- ESSENCIAL para fundo branco no PNG
  dpi = 300         # resolução para impressão
)




######################################################################
# ETAPA 5 – Geração de Tabela de Frequência por Grupo de Quintil
######################################################################

# Conta quantos registros estão em cada grupo_quintil
tabela_quadrantes_ceti <- gis_quintis_ceti %>%
  count(grupo_quintil) %>%
  arrange(grupo_quintil)

# Exibe a tabela no console
print(tabela_quadrantes_ceti)

# (Opcional) Exporta a tabela como arquivo XLSX
write.xlsx(tabela_quadrantes_ceti, "contagem_grupo_quintil_ceti.xlsx", rowNames = FALSE)


######################################################################
# ETAPA 6 – Geração Nova Coluna Usando o Grupo de Quintil
######################################################################

# Criar nova coluna Q_degrad com a combinação dos dois quintis
gis_quintis_ceti <- gis_quintis_ceti %>%
  mutate(Q_ceti = grupo_quintil)

# Criar nova coluna Q_mine, no shapefile original 'gi', com a combinação dos dois quintis
gi$Q_ceti <- gis_quintis_ceti$Q_ceti

colnames(gi)


###### Salvar o shapefile original com as novas colunas geradas referentes aos 'grupos quintil' de todos os índices 
# Q_defor, Q_degrad, Q_mining, Q_fire e Q_ceti

st_write(gi, "C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/buffers_agrupamentos_indigenas_indices_finais_risco_socio_ambiental_classif_grupos_quintis.shp", append=FALSE)


########################################################################################################
########################################### MAPAS ###########################################
# Produção de mapas a partir da classificação por quintis gerada acima
# Usando as colunas Q_defor, Q_degrad, Q_mining, Q_fire e Q_ceti

##### Mapas

# Carrega o shapefile salvo logo acima com as as colunas Q_defor, Q_degrad, Q_mining, Q_fire e Q_ceti

g_ind<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/buffers_agrupamentos_indigenas_indices_finais_risco_socio_ambiental_classif_grupos_quintis.shp")

colnames(g_ind)
# Carrega outros shapefiles bases para a produção dos mapas 

# Limite da Amazônia legal
amz<- st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/limites_espaciais/amazlegal_sirgas_4326.shp")

# Limite dos estados
states<- st_read("C:/Users/ANA/OneDrive - inpe.br/database/shapefiles/limits/amazon_states/UFS_AMZLEG_Polyconic_p_mapa_final.shp")

# Limite das TIs
tis<-st_read("C:/Users/ANA/Dropbox/Auditoria_TCU/DADOS/TIS/tis_amazonia_legal_poligonais/tis_amazonia_legal_poligonaisPolygon_polyconic_indicadores_nomes.shp")

# Carrega o diretório para salvar os mapas
setwd("C:/Users/ANA/Dropbox/Auditoria_TCU/FAP_DF/Dados/resultados/indices_finais/mapas")


### MAPAS FINAIS

### DEFOR x POP


### Vamos usar a mesma paleta por grupo quintil gerada para os gráficos acima

library(scales)  # carrega o pacote scales

cores_base <- c("skyblue", "forestgreen", "orange", "firebrick", "purple")
paleta_25 <- colorRampPalette(cores_base)(25)

show_col(paleta_25)  # visualize as cores geradas


# Nomeando as cores com os grupos dos quintis
quintis_possiveis <- paste0(rep(paste0("Q", 1:5), each = 5), "-", rep(1:5, times = 5))
cores_25 <- setNames(paleta_25, quintis_possiveis)

print(cores_25)


### MAPA: Índice de Desmatamento e Total de Pessoas

g_ind_coords <- g_ind %>%
  st_centroid() %>%  # Usa o centroide do polígono
  st_transform(4674) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()


# Plot
p1 <- ggplot() + 
  # Base
  geom_sf(data = states, fill = NA, color = 'gray30', size = 0.2) +
  geom_sf(data = amz, fill = NA, color = 'black', size = 0.3) +
  geom_sf(data = tis, fill = "lightgray", color = NA, size = 0.2) + 
  
  # Pontos como bolinhas com legenda de shape correta
  geom_point(data = g_ind_coords, aes(x = lon, y = lat, color = Q_defor), 
             shape = 16, size = 1.5, alpha = 0.8) +
  
  # Escala de cores
  scale_color_manual(
    values = cores_25,
    name = "Grupo quintil",
   # name = "Quintile Group",
    guide = guide_legend(
      override.aes = list(shape = 16, size = 5),
      title.position = "top",
      title.theme = element_text(size = 12),
      label.theme = element_text(size = 9)
    )
  ) +
  
  labs(
   # title = "Deforestation Index and Total Population",
    title = "Índice de Desmatamento e Total de Pessoas",
    ) +
  
  # Escala e seta
  annotation_scale(location = "bl", style = "ticks",
                   pad_x = unit(1, "cm"), pad_y = unit(0.5, "cm"),
                   height = unit(0.4, "cm"), width = unit(1.8, "cm"), 
                   text_cex = 1) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
p1


# Salvar
ggsave("mapa_defor_grupos_quintis_PORT.png", plot = p1, width = 10, height = 8, dpi = 300, bg = "white")


#### Mapa: Índice de Degradação Florestal e Total de Pessoas


p2 <- ggplot() + 
  # Base
  geom_sf(data = states, fill = NA, color = 'gray30', size = 0.2) +
  geom_sf(data = amz, fill = NA, color = 'black', size = 0.3) +
  geom_sf(data = tis, fill = "lightgray", color = NA, size = 0.2) + 
  
  # Pontos como bolinhas com legenda de shape correta
  geom_point(data = g_ind_coords, aes(x = lon, y = lat, color = Q_degrad), 
             shape = 16, size = 1.5, alpha = 0.8) +
  
  # Escala de cores
  scale_color_manual(
    values = cores_25,
   # name = "Quintile Group",
   name = "Grupo quintil",
    guide = guide_legend(
      override.aes = list(shape = 16, size = 5),
      title.position = "top",
      title.theme = element_text(size = 12),
      label.theme = element_text(size = 9)
    )
  ) +
  
  labs(
  #  title = "Forest Degradation Index and Total Population",
    title = "Índice de Degradação Florestal e Total de Pessoas",
  ) +
  
  # Escala e seta
  annotation_scale(location = "bl", style = "ticks",
                   pad_x = unit(1, "cm"), pad_y = unit(0.5, "cm"),
                   height = unit(0.4, "cm"), width = unit(1.8, "cm"), 
                   text_cex = 1) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Salvar
ggsave("mapa_degrad_grupos_quintis_PORT.png", plot = p2, width = 10, height = 8, dpi = 300, bg = "white")


#### Mapa: Índice de Fogo e Total de Pessoas


p3 <- ggplot() + 
  # Base
  geom_sf(data = states, fill = NA, color = 'gray30', size = 0.2) +
  geom_sf(data = amz, fill = NA, color = 'black', size = 0.3) +
  geom_sf(data = tis, fill = "lightgray", color = NA, size = 0.2) + 
  
  # Pontos como bolinhas com legenda de shape correta
  geom_point(data = g_ind_coords, aes(x = lon, y = lat, color = Q_fire), 
             shape = 16, size = 1.5, alpha = 0.8) +
  
  # Escala de cores
  scale_color_manual(
    values = cores_25,
  #  name = "Quintile Group",
    name = "Grupo quintil",
    guide = guide_legend(
      override.aes = list(shape = 16, size = 5),
      title.position = "top",
      title.theme = element_text(size = 12),
      label.theme = element_text(size = 9)
    )
  ) +
  
  labs(
   # title = "Fire Index and Total Population",
    title = "Índice de Fogo e Total de Pessoas",
  ) +
  
  
  # Escala e seta
  annotation_scale(location = "bl", style = "ticks",
                   pad_x = unit(1, "cm"), pad_y = unit(0.5, "cm"),
                   height = unit(0.4, "cm"), width = unit(1.8, "cm"), 
                   text_cex = 1) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Salvar
ggsave("mapa_fire_grupos_quintis_PORT.png", plot = p3, width = 10, height = 8, dpi = 300, bg = "white")



# Mapa: Índice de Mineração e Total de Pessoas

p4 <- ggplot() + 
  # Base
  geom_sf(data = states, fill = NA, color = 'gray30', size = 0.2) +
  geom_sf(data = amz, fill = NA, color = 'black', size = 0.3) +
  geom_sf(data = tis, fill = "lightgray", color = NA, size = 0.2) + 
  
  # Pontos como bolinhas com legenda de shape correta
  geom_point(data = g_ind_coords, aes(x = lon, y = lat, color = Q_mine), 
             shape = 16, size = 1.5, alpha = 0.8) +
  
  # Escala de cores
  scale_color_manual(
    values = cores_25,
  #  name = "Quintile Group",
    name = "Grupo quintil",
    guide = guide_legend(
      override.aes = list(shape = 16, size = 5),
      title.position = "top",
      title.theme = element_text(size = 12),
      label.theme = element_text(size = 9)
    )
  ) +
  
  labs(
    #title = "Mining Index and Total Population",
    title = "Índice de Mineração e Total de Pessoas",
  ) +
  
  
  # Escala e seta
  annotation_scale(location = "bl", style = "ticks",
                   pad_x = unit(1, "cm"), pad_y = unit(0.5, "cm"),
                   height = unit(0.4, "cm"), width = unit(1.8, "cm"), 
                   text_cex = 1) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Salvar
ggsave("mapa_mining_grupos_quintis_PORT.png", plot = p4, width = 10, height = 8, dpi = 300, bg = "white")



#### Mapa: Índice Composto de Ameaças Ambientais (CETI) e Total de Pessoas

p6 <- ggplot() + 
  # Base
  geom_sf(data = states, fill = NA, color = 'gray30', size = 0.2) +
  geom_sf(data = amz, fill = NA, color = 'black', size = 0.3) +
  geom_sf(data = tis, fill = "lightgray", color = NA, size = 0.2) + 
  
  # Pontos como bolinhas com legenda de shape correta
  geom_point(data = g_ind_coords, aes(x = lon, y = lat, color = Q_ceti), 
             shape = 16, size = 1.5, alpha = 0.8) +
  
  # Escala de cores
  scale_color_manual(
    values = cores_25,
      name = "Quintile Group",
   # name = "Grupo quintil",
    guide = guide_legend(
      override.aes = list(shape = 16, size = 5),
      title.position = "top",
      title.theme = element_text(size = 12),
      label.theme = element_text(size = 9)
    )
  ) +
  
  labs(
    #title = "Índice Composto de Ameaças Ambientais (CETI) a total de pessoas",
    title = "Composite Index of Environmental Threats and Total People",
  ) +
  
  
  # Escala e seta
  annotation_scale(location = "bl", style = "ticks",
                   pad_x = unit(1, "cm"), pad_y = unit(0.5, "cm"),
                   height = unit(0.4, "cm"), width = unit(1.8, "cm"), 
                   text_cex = 1) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm"),
                         height = unit(1.2, "cm"), width = unit(1.2, "cm")) +
  
  coord_sf(datum = NA) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Salvar
ggsave("mapa_CETI_grupos_quintis_ENG.png", plot = p6, width = 10, height = 8, dpi = 300, bg = "white")


############################### FIM #################################################################





