library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(reactR)
library(htmltools)
library(reactable)
library(readxl)

##------------------------------BOXPLOT DE IDADE POR ANO-----------------------
#BOXPLOT IDADE POR ANO NO ESPÍRITO SANTO, GERAL
#boxplot.anoidad.es <- dados_es_total %>%
#  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
#  geom_errorbar(stat = "boxplot", width = 0.2)+
##  geom_boxplot( fill = "#a6cee3" )+
#  labs(
#    title = "Óbitos de Idade x ano no Espírito Santo",
#    x = "Ano",
#    y = "Idade"
# ) +
# theme_classic() +
#  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
#  theme(
#    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
#    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
#    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
#    legend.title = element_text(size = 15),               # Tamanho do título da legenda
#    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
# )
#print(boxplot.anoidad.es)

#BOXPLOT IDADE POR ANO NO ESPÍRITO SANTO, PSICOATIVO
#boxplot.anoidad.es.psic <- dados_es_psic %>%
#  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
#  geom_errorbar(stat = "boxplot", width = 0.2)+
# geom_boxplot( fill = "#EFA1AE" )+
# labs(
# title = "Óbitos de Idade x ano no Espírito Santo, por psicoativos",
    #   x = "Ano",
    #   y = "Idade"
    # ) +
    # theme_classic() +
    # scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
    # theme(
    #  plot.title = element_text(size = 13, face = "bold"),   # Tamanho e estilo do título do gráfico
    #  axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    # axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    # legend.title = element_text(size = 15),               # Tamanho do título da legenda
    # legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
    #)
    #boxplot.anoidad.es.psic

#BOXPLOT IDADE POR ANO NO BRASIL, GERAL
#boxplot.anoidad.br <- dados_br_total %>%
# ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
# geom_errorbar(stat = "boxplot", width = 0.2)+
### geom_boxplot( fill = "#a6cee3" )+
#labs(
#   title = "Óbitos de Idade x ano no Brasil",
#   x = "Ano",
#   y = "Idade"
#  ) +
#  theme_classic() +
#  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
##  theme(
#   plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
#   axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
#  axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
#  legend.title = element_text(size = 15),               # Tamanho do título da legenda
#   legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
# )
#boxplot.anoidad.br

#BOXPLOT IDADE POR ANO NO BRASIL, PSICOATIVO
#boxplot.anoidad.br.psic <- dados_br_psic %>%
#  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
#  geom_errorbar(stat = "boxplot", width = 0.2)+
#  geom_boxplot( fill = "#EFA1AE" )+
#  labs(
#   title = "Óbitos de Idade x ano no Brasil, por psicoativos",
#   x = "Ano",
#   y = "Idade"
# ) +
#  theme_classic()+
# scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  #  theme(
#    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
#   axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
#   axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
##   legend.title = element_text(size = 15),               # Tamanho do título da legenda
#   legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
    # )
#boxplot.anoidad.br.psic



# --------------------NOVO BANCO DE DADOS-----------------
# BR TOTAL ----------------------------------------------------------------
base_boxplot_br <- dados_br_total %>%
  select(ANOOBITO, IDADE2) %>%
  filter(!is.na(ANOOBITO) & !is.na(IDADE2))

#gráfico
boxplot.anoidad.br <- base_boxplot_br %>%
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#a6cee3" )+
  labs(
    title = "Óbitos de Idade x ano no Brasil",
    x = "Ano",
    y = "Idade"
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),               
    legend.text = element_text(size = size_texto_legenda)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.br

# BR PSIC ---------------------------------------------------------------------
base_boxplot_br_psic <- dados_br_psic %>%
  select(ANOOBITO, IDADE2) %>%
  filter(!is.na(ANOOBITO) & !is.na(IDADE2))

boxplot.anoidad.br.psic <- base_boxplot_br_psic %>%
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#EFA1AE" )+
  labs(
    title = "Óbitos de Idade x ano no Brasil, por psicoativos",
    x = "Ano",
    y = "Idade"
  ) +
  theme_classic()+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),               
    legend.text = element_text(size = size_texto_legenda)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.br.psic

#ES TOTAL --------------------------------------------------------------

base_boxplot_es <- dados_es_total %>%
  select(ANOOBITO, IDADE2) %>%
  filter(!is.na(ANOOBITO) & !is.na(IDADE2))

boxplot.anoidad.es <- base_boxplot_es %>%
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#a6cee3" )+
  labs(
    title = "Óbitos de Idade x ano no Espírito Santo",
    x = "Ano",
    y = "Idade"
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),               
    legend.text = element_text(size = size_texto_legenda)                 # Tamanho do texto da legenda
  )
print(boxplot.anoidad.es)

# ES PSIC --------------------------------------------------------------------

base_boxplot_es_psic <- dados_es_psic %>%
  select(ANOOBITO, IDADE2) %>%
  filter(!is.na(ANOOBITO) & !is.na(IDADE2))

boxplot.anoidad.es.psic <- base_boxplot_es_psic %>%
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#EFA1AE" )+
  labs(
    title = "Óbitos de Idade x ano no Espírito Santo, por psicoativos",
    x = "Ano",
    y = "Idade"
  ) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 13, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),               
    legend.text = element_text(size = size_texto_legenda)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.es.psic

#Salvar gráficos como rda
save(boxplot.anoidad.es, file="GRAFICOS_RDA/boxplot.anoidad.es.RData")
save(boxplot.anoidad.es.psic, file="GRAFICOS_RDA/boxplot.anoidad.es.psic.RData")
save(boxplot.anoidad.br, file="GRAFICOS_RDA/boxplot.anoidad.br.RData")
save(boxplot.anoidad.br.psic, file="GRAFICOS_RDA/boxplot.anoidad.br.psic.RData")
