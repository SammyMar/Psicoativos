library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(reactR)
library(htmltools)
library(reactable)
getwd()
cores <-  brewer.pal(10, "Set1")
cores2 <- c("#4357AD","#EFA9AE","#9A3D6A", "#6a3d9a", "#a6cee3", "#1d9a55", "#9A3DA7", "#Dab2d6", "#6A3DA7", "#EFA1AE")

### -------------------------TABELA DINÂMICA DE CIDS-- ------------###
### PREPARAÇÃO ### 
#Importar o DataSet CIDs na pasta bases_de_dados antes de rodar o codigo 
# Tabela dinâmica
 reactable::reactable(CIDs, 
                      groupBy = c("CID", "subdivisao"),
                      filterable = TRUE,
                      showSortable = TRUE,
                      searchable = TRUE,
                      showPageSizeOptions = TRUE,
                      pageSizeOptions = c(10, 15, 27),
                      defaultPageSize = 27,
                      striped = TRUE,
                      highlight = TRUE,
                      theme = reactable::reactableTheme(
                        color = "#000000",
                        borderColor = "#dfe2e5",
                        stripedColor = "#f6f8fa",
                        highlightColor = "#f0f5f9",
                        cellPadding = "8px 12px",
                        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                        searchInputStyle = list(width = "100%")
                      ))


### ------------------ GRÁFICOS IDADE ----------------------###

## BOXPLOT SIMPLES DE IDADE PARA CADA BASE DE DADO

#Boxplot de idade no Brasil, geral

boxplot_idade_br <-  ggplot(subset(dados_br_total, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Óbitos por Idade no Brasil",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

boxplot_idade_br

#Boxplot de idade no brasil por psicoativo
boxplot_idade_br_psic <- ggplot(subset(dados_br_psic, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = ("#EFA9AE")) +
  labs(
    title = "Óbitos causados pelo uso de psicoativos por Idade no Brasil",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

boxplot_idade_br_psic

dados_filtrados <- subset(dados_br_psic, IDADE2 >= 0)
summary(dados_filtrados$IDADE2)

# Boxplot geral de idade no Espírito santo
#Boxplot de idade no espírito santo, filtrada por psicoativo
dados_es_total$x <- factor(1)
boxplot_idade_es <- ggplot(subset(dados_es_total, IDADE2 >= 0), aes(x = x, y = IDADE2)) +
  geom_boxplot(width = 0.5, fill = "skyblue") +
  labs(
    title = "Óbitos por Idade no Espírito Santo",
    x = "",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    axis.ticks.x = element_blank(),                       # Remover ticks do eixo x
    axis.text.x = element_blank()                         # Remover textos do eixo x
  )


dados_filtrados <- subset(dados_es_total, IDADE2 >= 0)
summary(dados_filtrados$IDADE2)

#Boxplot de idade no espírito santo, filtrada por psicoativo

dados_es_psic$x <- factor(1)
boxplot_idade_es_psic <- ggplot(subset(dados_es_psic, IDADE2 >= 0), aes(x = x, y = IDADE2)) +
  geom_boxplot(width = 0.5, fill = "#EFA9AE") +
  labs(
    title = "Óbitos causados pelo uso de psicoativos por Idade no ES",
    x = "",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20)) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    axis.ticks.x = element_blank(),                       # Remover ticks do eixo x
    axis.text.x = element_blank()                         # Remover textos do eixo x
  )




## GRÁFICO DE SÉRIE ANO X NÚMERO DE ÓBITOS PARA IDADE, SEPARADA POR INTERVALOS

#criando variavel categórica para idade
intervalos <- c(0, 17, 30, 55, Inf)  # Limites dos intervalos: 0-12, 13-24, 25-55, 56+
categorias <- c("Menor de idade", "Jovem-Adulto", "Adulto", "Idoso")  # Rótulos das categorias
#PALETA DE CORES PARA SÉRIES
cores <-  brewer.pal(8, "Set1")

#GRÁFICO DE SÉRIE ANO X OBITO POR IDADE NO ESPÍRITO SANTO, GERAL
# MONTANDO DADOS
dados_es_total$categoria_idade <- cut(dados_es_total$IDADE2, intervalos, labels = categorias)
dados_es_total$DTOBITO <- ymd(dados_es_total$DTOBITO)
dados_es_total$ANOOBITO <- year(dados_es_total$DTOBITO)

dados.grafico.series.esto <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, categoria_idade) %>%
    summarise(N.obitos = n())
)
# GRAFICO
graf.serie.idade.es.t <- ggplot(data = dados.grafico.series.esto, aes(x = ANOOBITO, y = N.obitos,
                                                                      colour = categoria_idade)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_idade)) +
  labs(title = "Número de Óbitos Totais no Espírito Santo de 2013-2022 por Faixa etária",
       x="Anos", y="Óbitos Totais", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.esto$ANOOBITO,
    labels = dados.grafico.series.esto$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = cores,
                      labels = c("Menor de idade (0-17 anos)", "Jovem-Adulto (18 - 30 anos)", "Adulto (31 - 55 anos)", "Idoso(56+ anos)"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

graf.serie.idade.es.t

#GRÁFICO DE SÉRIE ANO  X   OBITO POR IDADE NO ESPÍRITO SANTO, POR PSICOATIVOS
# MONTANDO DADOS

dados_es_psic$categoria_idade <- cut(dados_es_psic$IDADE2, intervalos, labels = categorias)
dados_es_psic$DTOBITO <- ymd(dados_es_psic$DTOBITO)
dados_es_psic$ANOOBITO <- year(dados_es_psic$DTOBITO)

dados.grafico.series.esps <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, categoria_idade) %>%
    summarise(N.obitos = n())
)

# GRAFICO
graf.serie.idade.es.p <- ggplot(data = dados.grafico.series.esps, aes(x = ANOOBITO, y = N.obitos,
                                                                      colour = categoria_idade)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_idade)) +
  labs(title = "Número de Óbitos pelo uso de psicoativos no Espírito Santo de 2013-2022 por Faixa etária",
       x="Anos", y="Número de óbitos", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.esps$ANOOBITO,
    labels = dados.grafico.series.esps$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = cores,
                      labels = c("Menor de idade (0-17 anos)", "Jovem-Adulto (18 - 30 anos)", "Adulto (31 - 55 anos)", "Idoso(56+ anos)"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

graf.serie.idade.es.p

#GRÁFICO DE SÉRIE ANO X OBITO POR IDADE NO BRASIL, GERAL
dados_br_total$categoria_idade <- cut(dados_br_total$IDADE2, intervalos, labels = categorias)
dados_br_total$DTOBITO <- ymd(dados_br_total$DTOBITO)
dados_br_total$ANOOBITO <- year(dados_br_total$DTOBITO)

dados.grafico.series.brto <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, categoria_idade) %>%
    summarise(N.obitos = n()/1000)
)

# GRAFICO
graf.serie.idade.br.t <- ggplot(data = dados.grafico.series.brto, aes(x = ANOOBITO, y = N.obitos,
                                                                      colour = categoria_idade)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_idade)) +
  labs(title = "Número de Óbitos Totais no Brasil de 2013-2022 por Faixa etária",
       x="Anos", y="Óbitos Totais x 1000", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.brto$ANOOBITO,
    labels = dados.grafico.series.brto$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = cores,
                      labels = c("Menor de idade (0-17 anos)", "Jovem-Adulto (18 - 30 anos)", "Adulto (31 - 55 anos)", "Idoso(56+ anos)"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

graf.serie.idade.br.t

# #GRÁFICO DE SÉRIE ANOS OBITO POR IDADE NO ESPÍRITO SANTO, POR PSICOATIVO
dados_br_psic$categoria_idade <- cut(dados_br_psic$IDADE2, intervalos, labels = categorias)
dados_br_psic$DTOBITO <- ymd(dados_br_psic$DTOBITO)
dados_br_psic$ANOOBITO <- year(dados_br_psic$DTOBITO)

dados.grafico.series.brps <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, categoria_idade) %>%
    summarise(N.obitos = n())
)

# GRAFICO
graf.serie.idade.br.p <- ggplot(data = dados.grafico.series.brps, aes(x = ANOOBITO, y = N.obitos,
                                                                      colour = categoria_idade)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = categoria_idade)) +
  labs(title = "Número de Óbitos pelo uso de psicoativos no Brasil de 2013-2022 por Faixa etária",
       x="Anos", y="Número de óbitos", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.brps$ANOOBITO,
    labels = dados.grafico.series.brps$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = cores,
                      labels = c("Menor de idade (0-17 anos)", "Jovem-Adulto (18 - 30 anos)", "Adulto (31 - 55 anos)", "Idoso(56+ anos)"))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

graf.serie.idade.br.p

##BOXPLOT DE IDADE POR ANO
#BOXPLOT IDADE POR ANO NO ESPÍRITO SANTO, GERAL
boxplot.anoidad.es <- dados_es_total %>% 
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "gray", size = 1)+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#a6cee3" )+
  labs(
    title = "Óbitos de Idade por ano no Espírito Santo",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))
boxplot.anoidad.es

#BOXPLOT IDADE POR ANO NO ESPÍRITO SANTO, PSICOATIVO
boxplot.anoidad.es.psic <- dados_es_psic %>% 
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#EFA1AE" )+
  labs(
    title = "Óbitos de Idade por ano no Espírito Santo, causados por psicoativos",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.es.psic

#BOXPLOT IDADE POR ANO NO BRASIL, GERAL
boxplot.anoidad.br <- dados_br_total %>% 
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#a6cee3" )+
  labs(
    title = "Óbitos de Idade por ano no Brasil",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.br

#BOXPLOT IDADE POR ANO NO BRASIL, PSICOATIVO
boxplot.anoidad.br.psic <- dados_br_psic %>% 
  ggplot(aes( x = as.factor(ANOOBITO), y=IDADE2, group = ANOOBITO))+
  geom_errorbar(stat = "boxplot", width = 0.2)+
  geom_boxplot( fill = "#EFA1AE" )+
  labs(
    title = "Óbitos de Idade por ano no Brasil, causados por psicoativos",
    x = "Ano",
    y = "Idade"
  ) +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )
boxplot.anoidad.br.psic

###----------------------- GRÁFICOS ESTADO CIVIL ---------------------###

# gráfico de barras de estado civil, BR GERAL

dados_prop_estciv_br <- dados_br_total %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n), LOC = "Brasil")


dados_prop_estciv_es <- dados_es_total %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n), LOC = "Espírito Santo")
#preparacao
dados_estciv <- data.frame(
  dados_br_total %>% group_by(ESTCIV) %>%
    summarise(N.obitos = n())
)

dados_prop_estciv_br <- dados_br_total %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_br <-  ggplot(dados_prop_estciv_br, aes(x = ESTCIV, y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
   geom_text(
     aes(label = paste0(p, "%") ),
     position = position_dodge(width = 0.9),
     vjust = -0.4,
     size = 4,)+
  theme_minimal() +
  labs(title = "Óbitos por estado civil no Brasil",
       x = " ",
       y = "Proporção (%) ")+
   theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )
barplot_estciv_br

# gráfico de barras de estado civil, BR PSICOATIVOS
dados_prop_estciv_br.psic <- dados_br_psic %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_br_psic <- ggplot(dados_prop_estciv_br.psic, aes(x = ESTCIV, y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(p, "%") ),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 4,

  )+
  theme_minimal() +
  labs(title = "Óbitos por psicoativos a partir do estado civil no Brasil",
       x = " ",
       y = "Proporção (%) ")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


barplot_estciv_br_psic

# gráfico de barras de estado civil, ES PSICOATIVOS
dados_prop_estciv_es.psic <- dados_es_psic %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_es_psic <- ggplot(dados_prop_estciv_es.psic, aes(x = ESTCIV, y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(p, "%") ),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 4,

  )+
  theme_minimal() +
  labs(title = "Óbitos por psicoativos a partir do estado civil no Espírito Santo",
       x = " ",
       y = "Proporção (%) ")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

barplot_estciv_es_psic

# gráfico de barras de estado civil, ES TOTAL
dados_prop_estciv_es <- dados_es_total %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_es <- ggplot(dados_prop_estciv_es, aes(x = ESTCIV, y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(p, "%") ),
    position = position_dodge(width = 0.9),
    vjust = -0.4,
    size = 4)+
  theme_minimal() +
  labs(title = "Óbitos por  estado civil no Espírito Santo",
       x = " ",
       y = "Proporção (%) ")+
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 11),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

barplot_estciv_es

##### ----------------SERIES ANO x NUMERO DE OBITOS CONTROLADO POR UMA VARIAVEL -----------####
# ÓBITOS NO BRASIL PELO USO DE PSICOATIVOS 

# MONTANDO DADOS
dados_br_psic$DTOBITO <- ymd(dados_br_psic$DTOBITO)
dados_br_psic$ANOOBITO <- year(dados_br_psic$DTOBITO)

dados.grafico.series <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, ESTCIV) %>%
    summarise(N.obitos = n())
)

# GRAFICO

serie_obt_psic_br <- ggplot(data = dados.grafico.series, aes(x = ANOOBITO, y = N.obitos,
                                                         colour = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESTCIV)) +
  labs(title = "óbitos pelo uso de psicoativo no Brasil por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

serie_obt_psic_br

# ÓBITOS NO ESPÍRITO SANTO PELO USO DE PSICOATIVOS 

dados_es_psic$DTOBITO <- ymd(dados_es_psic$DTOBITO)
dados_es_psic$ANOOBITO <- year(dados_es_psic$DTOBITO)

dados.grafico.series <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, ESTCIV) %>%
    summarise(N.obitos = n())
)

# GRAFICO

serie_obt_psic_es <- ggplot(data = dados.grafico.series, aes(x = ANOOBITO, y = N.obitos,
                                                             colour = ESTCIV)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESTCIV)) +
  labs(title = "Óbitos pelo uso de psicoativo no Espírito Santo por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


serie_obt_psic_es

### ------------- GRÁFICOS DE PROPORÇÃO -------------
## ESTADO CIVIL GERAL
## ES
#1 Criando data frame com proporção
dados.estciv.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, ESTCIV) %>% 
    summarise(N.obitos = n())
)
dados.estciv.es.series <- dados.estciv.es.series %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_estciv_es <- ggplot(dados.estciv.es.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESTCIV)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Casado" = "#A6CEE3", 
    "Separado judicialmente" = "#1F78B4", 
    "Solteiro" = "#B2DF8A", 
    "União consensual" = "#33A02C", 
    "Viúvo" = "#FB9A99", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes no Espírito Santo por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_estciv_es)

## BR
#1 Criando data frame com proporção
dados.estciv.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, ESTCIV) %>% 
    summarise(N.obitos = n())
)
dados.estciv.br.series <- dados.estciv.br.series %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_estciv_br <- ggplot(dados.estciv.br.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESTCIV)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Casado" = "#A6CEE3", 
    "Separado judicialmente" = "#1F78B4", 
    "Solteiro" = "#B2DF8A", 
    "União consensual" = "#33A02C", 
    "Viúvo" = "#FB9A99", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes no Brasil por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_estciv_br)

### ESTADO CIVIL POR PSICOATIVO
## ES PSIC
#1 Criando data frame com proporção
dados.estciv.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, ESTCIV) %>% 
    summarise(N.obitos = n())
)
dados.estciv.es.series.psic <- dados.estciv.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_estciv_es_psic <- ggplot(dados.estciv.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESTCIV)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Casado" = "#A6CEE3", 
    "Separado judicialmente" = "#1F78B4", 
    "Solteiro" = "#B2DF8A", 
    "União consensual" = "#33A02C", 
    "Viúvo" = "#FB9A99", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes por Psicoativos no Espírito Santo por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_estciv_es_psic)

## BR PSIC
#1 Criando data frame com proporção
dados.estciv.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, ESTCIV) %>% 
    summarise(N.obitos = n())
)
dados.estciv.br.series.psic <- dados.estciv.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_estciv_br_psic <- ggplot(dados.estciv.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESTCIV)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Casado" = "#A6CEE3", 
    "Separado judicialmente" = "#1F78B4", 
    "Solteiro" = "#B2DF8A", 
    "União consensual" = "#33A02C", 
    "Viúvo" = "#FB9A99", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes por psicoativos no Brasil por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_estciv_br_psic)

## IDADE PSIC
## ES psic
#1 Criando data frame com proporção
dados.idade.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, categoria_idade) %>% 
    summarise(N.obitos = n())
)
dados.idade.es.series.psic <- dados.idade.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_idade_es_psic <- ggplot(dados.idade.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Adulto" = "#A6CEE3", 
    "Idoso" = "#B2DF8A", 
    "Jovem-Adulto" = "#FB9A99", 
    "Menor de idade" = "#1F78B4", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes por psicoativo no Espírito Santo por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_idade_es_psic)


# BR PSIC
#1 Criando data frame com proporção
dados.idade.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, categoria_idade) %>% 
    summarise(N.obitos = n())
)
dados.idade.br.series.psic <- dados.idade.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_idade_br_psic <- ggplot(dados.idade.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Adulto" = "#A6CEE3", 
    "Idoso" = "#B2DF8A", 
    "Jovem-Adulto" = "#FB9A99", 
    "Menor de idade" = "#1F78B4", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes por psicoativo no Brasil por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_idade_br_psic)

## IDADE GERAL
## IDADE ES GERAL
#1 Criando data frame com proporção
dados.idade.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, categoria_idade) %>% 
    summarise(N.obitos = n())
)
dados.idade.es.series <- dados.idade.es.series %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_idade_es <- ggplot(dados.idade.es.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Adulto" = "#A6CEE3", 
    "Idoso" = "#B2DF8A", 
    "Jovem-Adulto" = "#FB9A99", 
    "Menor de idade" = "#1F78B4", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes no Espírito Santo por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )


print(grafico_prop_idade_es)

## idade BR GERAL

#1 Criando data frame com proporção
dados.idade.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, categoria_idade) %>% 
    summarise(N.obitos = n())
)
dados.idade.br.series <- dados.idade.br.series %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o gráfico

grafico_prop_idade_br <- ggplot(dados.idade.br.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Adulto" = "#A6CEE3", 
    "Idoso" = "#B2DF8A", 
    "Jovem-Adulto" = "#FB9A99", 
    "Menor de idade" = "#1F78B4", 
    "NA" = "#D3D3D3"
  )) +
  labs(
    title = "Proporção de Mortes no Brasil por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

print(grafico_prop_idade_br)
