library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(reactR)
library(htmltools)
library(reactable)
library(readxl)
getwd()

### -------------------------TABELA DINÂMICA DE CIDS-- ------------###
### PREPARAÇÃO ###
#Importar o DataSet CIDs na pasta bases_de_dados antes de rodar o codigo:

CIDs <- read_excel('CIDs.xlsx')

# Tabela dinâmica
 tabela_redutiva <- reactable::reactable(CIDs,
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

 save(tabela_redutiva, file="GRAFICOS_RDA/tabela_redutiva.RData")
### ------------------ GRÁFICOS IDADE ----------------------###

## BOXPLOT SIMPLES DE IDADE PARA CADA BASE DE DADO

#Boxplot de idade no Brasil, geral

#boxplot_idade_br <-  ggplot(subset(dados_br_total, IDADE2 >= 0), aes(y = IDADE2)) +
  #3geom_boxplot(fill = "skyblue") +
 #labs(
 #  title = "Óbitos por Idade no Brasil",
 #   y = "Idade"
 # ) +
 # theme_minimal() +
 # scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

 #boxplot_idade_br

#Boxplot de idade no brasil por psicoativo
 #boxplot_idade_br_psic <- ggplot(subset(dados_br_psic, IDADE2 >= 0), aes(y = IDADE2)) +
 # geom_boxplot(fill = ("#EFA9AE")) +
 #labs(
 ## title = "Óbitos causados pelo uso de psicoativos por Idade no Brasil",
 # y = "Idade"
 # ) +
 # theme_minimal() +
 # scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

 #boxplot_idade_br_psic

 #dados_filtrados <- subset(dados_br_psic, IDADE2 >= 0)
 #summary(dados_filtrados$IDADE2)

# Boxplot geral de idade no Espírito santo
#Boxplot de idade no espírito santo, filtrada por psicoativo
 #dados_es_total$x <- factor(1)
 #boxplot_idade_es <- ggplot(subset(dados_es_total, IDADE2 >= 0), aes(x = x, y = IDADE2)) +
 #  geom_boxplot(width = 0.5, fill = "skyblue") +
 #  labs(
 #   title = "Óbitos por Idade no Espírito Santo",
 #  x = "",
 #  y = "Idade"
 # ) +
 # theme_minimal() +
 # scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20)) +
 # theme(
 #   plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
 #  axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
     #   axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
 #  axis.ticks.x = element_blank(),                       # Remover ticks do eixo x
 #  axis.text.x = element_blank()                         # Remover textos do eixo x
 #  )


 #dados_filtrados <- subset(dados_es_total, IDADE2 >= 0)
 #summary(dados_filtrados$IDADE2)

#Boxplot de idade no espírito santo, filtrada por psicoativo

 #dados_es_psic$x <- factor(1)
 #boxplot_idade_es_psic <- ggplot(subset(dados_es_psic, IDADE2 >= 0), aes(x = x, y = IDADE2)) +
 #  geom_boxplot(width = 0.5, fill = "#EFA9AE") +
 #  labs(
 ##   title = "Óbitos causados pelo uso de psicoativos por Idade no ES",
 #  x = "",
    #  y = "Idade"
 # ) +
 # theme_minimal() +
 # scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20)) +
 # theme(
 # plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
 #  axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
 #  axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
 #  axis.ticks.x = element_blank(),                       # Remover ticks do eixo x
 #  axis.text.x = element_blank()                         # Remover textos do eixo x
 # )




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
  geom_point(shape = 15, aes(colour = categoria_idade,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Faixa Etária: ", categoria_idade))) +
  labs(title = "Óbitos Totais no Espírito Santo por Faixa etária",
       x="Anos", y="Óbitos Totais", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.esto$ANOOBITO,
    labels = dados.grafico.series.esto$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = paleta_series(5),
                      labels = c("Menor de idade (0-17 anos)",
                                 "Jovem-Adulto (18 - 30 anos)",
                                 "Adulto (31 - 55 anos)",
                                 "Idoso(56+ anos)"))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))                 # Tamanho do texto da legenda


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
  geom_point(shape = 15, aes(colour = categoria_idade,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Faixa Etária: ", categoria_idade))) +
  labs(title = "Óbitos pelo uso de psicoativos no Espírito Santo por Faixa etária",
       x="Anos", y="Número de óbitos", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.esps$ANOOBITO,
    labels = dados.grafico.series.esps$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = paleta_series(5),
                      labels = c("Menor de idade (0-17 anos)",
                                 "Jovem-Adulto (18 - 30 anos)",
                                 "Adulto (31 - 55 anos)",
                                 "Idoso(56+ anos)"))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

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
  geom_point(shape = 15, aes(colour = categoria_idade,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Faixa Etária: ", categoria_idade))) +
  labs(title = " Óbitos Totais no Brasil por Faixa etária",
       x="Anos", y="Óbitos Totais x 1000", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.brto$ANOOBITO,
    labels = dados.grafico.series.brto$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = paleta_series(5),
                      labels = c("Menor de idade (0-17 anos)",
                                 "Jovem-Adulto (18 - 30 anos)",
                                 "Adulto (31 - 55 anos)",
                                 "Idoso(56+ anos)"))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

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
  geom_point(shape = 15, aes(colour = categoria_idade,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Faixa Etária: ", categoria_idade))) +
  labs(title = " Óbitos pelo uso de psicoativos no Brasil por Faixa etária",
       x="Anos", y="Número de óbitos", colour = "Faixa etária") +
  scale_x_continuous(
    breaks = dados.grafico.series.brps$ANOOBITO,
    labels = dados.grafico.series.brps$ANOOBITO)+
  scale_colour_manual(name = "Faixa etária", values = paleta_series(5),
                      labels = c("Menor de idade (0-17 anos)",
                                 "Jovem-Adulto (18 - 30 anos)",
                                 "Adulto (31 - 55 anos)",
                                 "Idoso(56+ anos)"))+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

graf.serie.idade.br.p
save(graf.serie.idade.es.t, file="GRAFICOS_RDA/graf.serie.idade.es.t.RData")
save(graf.serie.idade.es.p, file="GRAFICOS_RDA/graf.serie.idade.es.p.RData")
save(graf.serie.idade.br.t, file="GRAFICOS_RDA/graf.serie.idade.br.t.RData")
save(graf.serie.idade.br.p, file="GRAFICOS_RDA/graf.serie.idade.br.p.RData")


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

barplot_estciv_br <-  ggplot(dados_prop_estciv_br, aes(x = fct_reorder(ESTCIV,p), y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity", aes(text  = paste("<br>Percentual: ", p, "%",
                                                "<br>Estado Civil: ", ESTCIV)))+

  scale_fill_manual(values=paleta_hist(5))+
  theme_minimal() +
  labs(title = "Óbitos por estado civil no Brasil",
       x = " Estado Civil",
       y = "Proporção (%) ",
       fill = "Estado civil")+
  theme_classic()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),      # Tamanho dos textos dos eixos
    axis.text.x = element_blank(),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


ggplotly(barplot_estciv_br, tooltip = "text")


# gráfico de barras de estado civil, BR PSICOATIVOS
dados_prop_estciv_br.psic <- dados_br_psic %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_br_psic <- ggplot(dados_prop_estciv_br.psic, aes(x = fct_reorder(ESTCIV, p), y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity", aes(text  = paste("<br>Percentual: ", p, "%",
                                                "<br>Estado Civil: ", ESTCIV)))+
  scale_fill_manual(values=paleta_hist(5))+
  theme_minimal() +
  labs(title = "Óbitos por psicoativos a partir do estado civil no Brasil",
       x = "Estado Civil ",
       y = "Proporção (%) ",
       fill= 'Estado civil')+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),# Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


barplot_estciv_br_psic

# gráfico de barras de estado civil, ES PSICOATIVOS
dados_prop_estciv_es.psic <- dados_es_psic %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_es_psic <- ggplot(dados_prop_estciv_es.psic, aes(x = fct_reorder(ESTCIV,p), y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity", aes(text  = paste("<br>Percentual: ", p, "%",
                                                "<br>Estado Civil: ", ESTCIV)))+
  scale_fill_manual(values=paleta_hist(5))+
  theme_classic() +
  labs(title = "Óbitos por psicoativos a partir do estado civil no ES",
       x = "Estado Civil ",
       y = "Proporção (%) ",
       fill= 'Estado civil')+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),# Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

barplot_estciv_es_psic

# gráfico de barras de estado civil, ES TOTAL
dados_prop_estciv_es <- dados_es_total %>%
  group_by(ESTCIV) %>%
  summarise(n = n()) %>%
  mutate(p= round((n/sum(n))*100, 2), LOC = "Brasil")

barplot_estciv_es <- ggplot(dados_prop_estciv_es, aes(x = fct_reorder(ESTCIV,p), y = p, fill = ESTCIV)) +
  geom_bar(stat = "identity", aes(text  = paste("<br>Percentual: ", p, "%",
                                                "<br>Estado Civil: ", ESTCIV)))+
  scale_fill_manual(values=paleta_hist(5))+
  theme_classic() +
  labs(title = "Óbitos no Espírito Santo por estado civil",
       x = "Estado Civil ",
       y = "Proporção (%) ",
       fill= 'Estado civil')+
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),# Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

barplot_estciv_es
save(barplot_estciv_br, file="GRAFICOS_RDA/barplot_estciv_br.RData")
save(barplot_estciv_br_psic, file="GRAFICOS_RDA/barplot_estciv_br_psic.RData")
save(barplot_estciv_es_psic, file="GRAFICOS_RDA/barplot_estciv_es_psic.RData")
save(barplot_estciv_es, file="GRAFICOS_RDA/barplot_estciv_es.RData")
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
  geom_point(shape = 15, aes(colour = ESTCIV,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Estado Civil: ", ESTCIV))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "óbitos pelo uso de psicoativo no Brasil por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

ggplotly(serie_obt_psic_br, tooltip="text")

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
  geom_point(shape = 15, aes(colour = ESTCIV,
                             text  = paste("Ano: ", ANOOBITO,
                                           "<br>Quantidade: ", N.obitos,
                                           "<br>Estado Civil: ", ESTCIV))) +
  scale_colour_manual(values =  paleta_series(5))+
  labs(title = "Óbitos pelo uso de psicoativo no Espírito Santo por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


serie_obt_psic_es
save(serie_obt_psic_br, file="GRAFICOS_RDA/serie_obt_psic_br.RData")
save(serie_obt_psic_es, file="GRAFICOS_RDA/serie_obt_psic_es.RData")

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

grafico_prop_estciv_es <- ggplot(dados.estciv.es.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = ESTCIV,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                                "<br>Estado Civil: ", ESTCIV))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(5)))+
  labs(
    title = "Proporção de Mortes no Espírito Santo por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


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

grafico_prop_estciv_br <- ggplot(dados.estciv.br.series,
                                 aes(x = factor(ANOOBITO),
                                     y = porcentagem, fill = ESTCIV,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                  "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                  "<br>Estado Civil: ", ESTCIV))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(5)))+
  labs(
    title = "Proporção de Mortes no Brasil por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


print(grafico_prop_estciv_br)
save(grafico_prop_estciv_es, file="GRAFICOS_RDA/grafico_prop_estciv_es.RData")
save(grafico_prop_estciv_br, file="GRAFICOS_RDA/grafico_prop_estciv_br.RData")
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

grafico_prop_estciv_es_psic <- ggplot(dados.estciv.es.series.psic, aes(x = factor(ANOOBITO),
                                                                       y = porcentagem,
                                                                       fill = ESTCIV,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                   "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                   "<br>Estado Civil: ", ESTCIV))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(5)))+
  labs(
    title = "Proporção de Mortes por Psicoativos no ES por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


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

grafico_prop_estciv_br_psic <- ggplot(dados.estciv.br.series.psic, aes(x = factor(ANOOBITO),
                                                                       y = porcentagem,  fill = ESTCIV,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                     "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                     "<br>Estado Civil: ", ESTCIV))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(5)))+
  labs(
    title = "Proporção de Mortes por psicoativos no Brasil por Estado civil e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Estado Civil"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


print(grafico_prop_estciv_br_psic)
save(grafico_prop_estciv_es, file="GRAFICOS_RDA/grafico_prop_estciv_es.RData")
save(grafico_prop_estciv_br, file="GRAFICOS_RDA/grafico_prop_estciv_br.RData")
save(grafico_prop_estciv_es_psic, file="GRAFICOS_RDA/grafico_prop_estciv_es_psic.RData")
save(grafico_prop_estciv_br_psic, file="GRAFICOS_RDA/grafico_prop_estciv_br_psic.RData")
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

grafico_prop_idade_es_psic <- ggplot(dados.idade.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                                 "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                                                 "<br>Faixa Etária: ", categoria_idade))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(4)))+
  labs(
    title = "Proporção de Mortes por psicoativo no ES por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


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

grafico_prop_idade_br_psic <- ggplot(dados.idade.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                                 "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                                                 "<br>Faixa Etária: ", categoria_idade))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(4)))+
  labs(
    title = "Proporção de Mortes por psicoativo no Brasil por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


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

grafico_prop_idade_es <- ggplot(dados.idade.es.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                       "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                                       "<br>Faixa Etária: ", categoria_idade))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(4)))+
  labs(
    title = "Proporção de Mortes no Espírito Santo por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))


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

grafico_prop_idade_br <- ggplot(dados.idade.br.series, aes(x = factor(ANOOBITO), y = porcentagem, fill = categoria_idade,text  = paste("Ano: ", factor(ANOOBITO),
                                                                                                                                       "<br>Percentual: ",round(porcentagem, 2),"%",
                                                                                                                                       "<br>Faixa Etária: ", categoria_idade))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values =  (paleta_hist(4)))+
  labs(
    title = "Proporção de Mortes no Brasil por Idade e Ano",
    y = "Porcentagem (%)",
    x = "",
    fill = "Idade"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = size_titulo_legenda),
    legend.text = element_text(size = size_texto_legenda))

print(grafico_prop_idade_br)
save(grafico_prop_idade_es_psic, file="GRAFICOS_RDA/grafico_prop_idade_es_psic.RData")
save(grafico_prop_idade_br_psic, file="GRAFICOS_RDA/grafico_prop_idade_br_psic.RData")
save(grafico_prop_idade_es, file="GRAFICOS_RDA/grafico_prop_idade_es.RData")
save(grafico_prop_idade_br, file="GRAFICOS_RDA/grafico_prop_idade_br.RData")

