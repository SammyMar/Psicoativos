library(remotes) 
library(microdatasus) 
library(dplyr)
library(stringr) 
library(ggplot2) 
library(lubridate) 



# -------------- ANALISE ESCOLARIDADE  -------------

#HISTOGRAMA

#3.1 quantidade de mortes por escolaridade no brasil
freq_escolaridade_br_total <- dados_br_total %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_total <- ggplot(freq_escolaridade_br_total, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes totais no Brasil entre o período de 2013 a 2022 de acordo com a Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_br_total)

#3.2 quantidade de mortes por escolaridade no espirito santo

freq_escolaridade_br_psic <- dados_br_psic %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_psic <- ggplot(freq_escolaridade_br_psic, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por psicoativo no Brasil entre o período de 2013 a 2022 de acordo com a Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_br_psic)

#3.3 quantidade de mortes por psicoatvos por escolaridade no brasil

freq_escolaridade_es_total <- dados_es_total %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_total <- ggplot(freq_escolaridade_es_total, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes totais no Espírito Santo entre o período de 2013 a 2022 de acordo com a Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_es_total)

#3.4 quantidade de mortes por psicoativos por escolaridade no espirito santo 

freq_escolaridade_es_psic <- dados_es_psic %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_psic <- ggplot(freq_escolaridade_es_psic, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por psicoativo no ES entre o período de 2013 a 2022 de acordo com a Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")

print(hist_escolaridade_es_psic)







#SERIE

#2.2 quantidade de mortes TOTAIS por escolaridade no Brasil

# montando os dados
dados.escolaridade.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, ESC) %>% 
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_br_total <- ggplot(data = dados.escolaridade.br.series, aes(x = ANOOBITO, y = N.obitos, 
                                                                                colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC)) +
  labs(title = "Número de Óbitos Totais no Brasil de 2013 a 2022 de acordo com a Escolaridade", 
       x="Anos", y="Óbitos Totais", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.br.series$ANOOBITO,  
    labels = dados.escolaridade.br.series$ANOOBITO)+ 
  theme_classic()

print (series_escolaridade_br_total)

#2.2 quantidade de mortes TOTAIS por escolaridade no espirito santo

# montando os dados
dados.escolaridade.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, ESC) %>% 
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_es_total <- ggplot(data = dados.escolaridade.es.series, aes(x = ANOOBITO, y = N.obitos, 
                                                                    colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC)) +
  labs(title = "Número de Óbitos Totais no ES de 2013 a 2022 de acordo com a Escolaridade", 
       x="Anos", y="Óbitos Totais", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.es.series$ANOOBITO,  
    labels = dados.escolaridade.es.series$ANOOBITO)+ 
  theme_classic()

print (series_escolaridade_es_total)



#2.1 quantidade de mortes por psicoativos por escolaridade no brasil

# montando os dados
dados.escolaridade.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, ESC) %>% 
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_br_psic <- ggplot(data = dados.escolaridade.br.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                        colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC)) +
  labs(title = "Número de Óbitos por psicativos no Brasil de 2013 a 2022 de acordo com a Escolaridade", 
       x="Anos", y="Óbitos por psicativos", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.br.series.psic$ANOOBITO,  
    labels = dados.escolaridade.br.series.psic$ANOOBITO)+ 
  theme_classic()

print (series_escolaridade_br_psic)


#2.1 quantidade de mortes por psicoativos por escolaridade no espirito santo

# montando os dados
dados.escolaridade.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, ESC) %>% 
    summarise(N.obitos = n())
)

# grafico
series_escolaridade_es_psic <- ggplot(data = dados.escolaridade.es.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                                    colour = ESC)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = ESC)) +
  labs(title = "Número de Óbitos por psicativos no Espírito Santo de 2013 a 2022 de acordo com a Escolaridade", 
       x="Anos", y="Óbitos por psicativos", colour = "Escolaridade") +
  scale_x_continuous(
    breaks = dados.escolaridade.es.series.psic$ANOOBITO,  
    labels = dados.escolaridade.es.series.psic$ANOOBITO)+ 
  theme_classic()

print (series_escolaridade_es_psic)


