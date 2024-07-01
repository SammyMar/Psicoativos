library(remotes) 
library(microdatasus) 
library(dplyr)
library(stringr) 
library(ggplot2) 
library(lubridate) 



# -------------- ANALISE GENERO -------------

#HISTOGRAMA

#2.1 quantidade de mortes por genero no brasil
freq_genero_br_total <- dados_br_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_br_total <- ggplot(freq_genero_br_total, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por genero no Brasil entre o período de 2013 a 2022",
       x = "Gênero",
       y = "Número de Pessoas")

print(hist_genero_br_total)

#2.2 quantidade de mortes por genero no espirito santo

freq_genero_br_psic <- dados_br_psic %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_br_psic <- ggplot(freq_genero_br_psic, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por psicoativo no Brasil entre o período de 2013 a 2022 de acordo com o gênero",
       x = "Gênero",
       y = "Número de Pessoas")

print(hist_genero_br_psic)

#2.3 quantidade de mortes por psicoatvos por genero no brasil

freq_genero_es_total <- dados_es_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_es_total <- ggplot(freq_genero_es_total, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por gênero no ES entre o período de 2013 a 2022",
       x = "Gênero",
       y = "Número de Pessoas")

print(hist_genero_es_total)

#2.4 quantidade de mortes por psicoativos por genero no espirito santo 

freq_genero_es_psic <- dados_es_psic %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_genero_es_psic <- ggplot(freq_genero_es_psic, aes(x = SEXO, y = Quantidade, fill = SEXO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Quantidades de mortes por psicoativo no ES entre o período de 2013 a 2022 de acordo com o gênero",
       x = "Gênero",
       y = "Número de Pessoas")

print(hist_genero_es_psic)







#SERIE

#2.1 quantidade de mortes por genero no brasil

# montando os dados
dados.genero.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_br_total <- ggplot(data = dados.genero.br.series, aes(x = ANOOBITO, y = N.obitos, 
                                                         colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Número de Óbitos Totais no Brasil de 2013 a 2022 por Sexo", 
       x="Anos", y="Óbitos Totais", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.br.series$ANOOBITO,  
    labels = dados.genero.br.series$ANOOBITO)+ 
  theme_classic()

print (series_genero_br_total)


#2.2 quantidade de mortes por genero no espirito santo

# montando os dados
dados.genero.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_es_total <- ggplot(data = dados.genero.es.series, aes(x = ANOOBITO, y = N.obitos, 
                                                                 colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Número de Óbitos Totais no ES de 2013 a 2022 por Sexo", 
       x="Anos", y="Óbitos Totais", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.series$ANOOBITO,  
    labels = dados.genero.es.series$ANOOBITO)+ 
  theme_classic()

print (series_genero_es_total)





#2.1 quantidade de mortes por psicoativos por genero no brasil

# montando os dados
dados.genero.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_br_psic <- ggplot(data = dados.genero.br.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                    colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Número de Óbitos por psicativos no Brasil de 2013 a 2022 de acordo com o gênero", 
       x="Anos", y="Óbitos por psicativos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.br.series.psic$ANOOBITO,  
    labels = dados.genero.br.series.psic$ANOOBITO)+ 
  theme_classic()

print (series_genero_br_psic)


#2.2 quantidade de mortes por psicoativos por genero no espirito santo

# montando os dados
dados.genero.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_es_psic <- ggplot(data = dados.genero.es.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                        colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Número de Óbitos por psicativos no ES de 2013 a 2022 de acordo com o gênero", 
       x="Anos", y="Óbitos por psicativos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.series.psic$ANOOBITO,  
    labels = dados.genero.es.series.psic$ANOOBITO)+ 
  theme_classic()

print (series_genero_es_psic)
