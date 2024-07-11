library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
getwd()
# ano teste


#idade

# Criar o boxplot
#Idade br total
dados_br_total <- dados_br_total[!is.na(dados_br_total$IDADE2), ]

boxplot_idade_br <-  ggplot(subset(dados_br_total, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Óbitos por Idade no Brasil",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

dados_filtrados <- subset(dados_br_total, IDADE2 >= 0)
summary(dados_filtrados$IDADE2)
#idade br psic
boxplot_idade_br_psic <- ggplot(subset(dados_br_psic, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = ("#EFA9AE")) +
  labs(
    title = "Óbitos causados pelo uso de psicoativos por Idade no Brasil",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

dados_filtrados <- subset(dados_br_psic, IDADE2 >= 0)
summary(dados_filtrados$IDADE2)

# idade es total

boxplot_idade_es <- ggplot(subset(dados_es_total, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Óbitos por Idade no Espírito Santo",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))


dados_filtrados <- subset(dados_es_total, IDADE2 >= 0)
summary(dados_filtrados$IDADE2)

#idade es psic
boxplot_idade_es_psic <- ggplot(subset(dados_es_psic, IDADE2 >= 0), aes(y = IDADE2)) +
  geom_boxplot(fill = ("#EFA9AE")) +
  labs(
    title = "Óbitos causados pelo uso de psicoativos por Idade no ES",
    y = "Idade"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 135, by = 20))

dados_filtrados <- subset(dados_es_psic, IDADE2 >= 0)
summary(dados_es_psic$IDADE2)


#criando variavel categórica para idade
intervalos <- c(0, 12, 24, 55, Inf)  # Limites dos intervalos: 0-12, 13-24, 25-55, 56+
categorias <- c("Infantil", "Jovem", "Adulto", "Idoso")  # Rótulos das categorias

# Criar a variável categórica baseada na idade

cores <-  brewer.pal(8, "Set1")
#### SERIES ANO x NUMERO DE OBITOS CONTROLADO POR UMA VARIAVEL ####
#infantil (0-12 anos), JOVEM (13-24 anos), adulto (25-55 anos) e idoso (56+ anos)
# MONTANDO DADOS
#ES TOTAL
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
                    labels = c("Infantil (0-12 anos)", "Jovem (13-24 anos)", 'Adulto (25-55 anos)', 'Idoso (56+ anos)'))+
  theme_minimal()

graf.serie.idade.es.t
##ES psic
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
                      labels = c("Infantil (0-12 anos)", "Jovem (13-24 anos)", 'Adulto (25-55 anos)', 'Idoso (56+ anos)'))+
  theme_minimal()

graf.serie.idade.es.p

#BR TOTAL

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
                      labels = c("Infantil (0-12 anos)", "Jovem (13-24 anos)", 'Adulto (25-55 anos)', 'Idoso (56+ anos)'))+
  theme_minimal()

graf.serie.idade.br.t

# Br psic


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
                      labels = c("Infantil (0-12 anos)", "Jovem (13-24 anos)", 'Adulto (25-55 anos)', 'Idoso (56+ anos)'))+
  theme_minimal()

graf.serie.idade.br.p

#estado civil
#BR

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
   theme(legend.position = "none")


# BR PSIC
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
  theme(legend.position = "none")




# ES PSIC
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
  theme(legend.position = "none")



# ES TOTAL
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
    size = 4,

  )+
  theme_minimal() +
  labs(title = "Óbitos por  estado civil no Espírito Santo",
       x = " ",
       y = "Proporção (%) ")+
  theme(legend.position = "none")



##### SERIES ANO x NUMERO DE OBITOS CONTROLADO POR UMA VARIAVEL ####
#br po psic
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
  labs(title = "Número de Óbitos pelo uso de psicoativo no Brasil de 2013 a 2022 por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_minimal()

serie_obt_psic_br

#es por psic
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
  labs(title = "Número de Óbitos pelo uso de psicoativo no Espírito Santo de 2013 a 2022 por Estado civil",
       x="Anos", y="Número de óbitos ", colour = "Estado Civil") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,
    labels = dados.grafico.series$ANOOBITO)+
  theme_minimal()

serie_obt_psic_es




#grafico proprcao




# grafico por local de óbito

local_obit_bR_t <- dados_br_total %>%
  group_by(LOCOCOR) %>%
  summarise(N.obitos = sum(N.obitos))

local_obit_bR_t <- data.frame(
  dados_br_total %>% group_by(LOCOCOR) %>%
    summarise(N.obitos = sum(N.obitos))
)

 ggplot(local_obit_bR_t, aes(x = LOCOCOR, y = N.obitos, fill = LOCOCOR)) +
  geom_bar(stat = "identity") +
   geom_text(
     aes(label = N.obitos),
     position = position_dodge(width = 0.9),
     vjust = -0.4,
     size = 4,

   )+
  theme_minimal() +
   theme(axis.text.x = element_blank(), # Remove os nomes das colunas no eixo x
         axis.ticks.x = element_blank()) +
  labs(title = "Números de óbitos por locais no Brail",
       x = "Locais",
       y = "Número de óbitos")


table(dados_br_total$LOCOCOR)
#GRÁFICOS
frequencia_genero_br <- dados_br_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)
 #construir tabela de frequencia de cad auma das variáveis
  nome_da_tabela <- base_de_dados %>%
  group_by(VARIAVEL) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

#plotar grafico
nome_do_grafico <- ggplot(nome_da_tabela, aes(x = VARIAVEL, y = Quantidade, fill = VARIAVEL)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Título que vai aparecer no seu gráfico",
       x = "Título do eixo x",
       y = "Título do eixo x")

