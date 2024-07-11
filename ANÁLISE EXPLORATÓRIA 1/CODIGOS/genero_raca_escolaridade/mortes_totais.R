library(remotes) 
library(microdatasus) 
library(dplyr)
library(stringr) 
library(ggplot2) 
library(lubridate) 


# -------------- ANALISE MORTES TOTAIS  -------------

#HISTOGRAMA

# BR TOTAIS

# agrupamento pelo ano do obito

freq_obito_br_totais <- dados_br_total %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_br_totais <- ggplot(freq_obito_br_totais, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes Totais no Brasil de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")


print(hist_obito_br_totais)


# BR PSICOATIVOS

freq_obito_br_psic <- dados_br_psic %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_br_psic <- ggplot(freq_obito_br_psic, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por Psicoativos no Brasil de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")


print(hist_obito_br_psic)


# ES TOTAIS

freq_obito_es_totais <- dados_es_total %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_es_totais <- ggplot(freq_obito_es_totais, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes Totais no Espírito Santo de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")


print(hist_obito_es_totais)



#ES PSICOATIVOS

freq_obito_es_psic <- dados_es_psic %>%
  group_by(ANOOBITO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_obito_es_psic <- ggplot(freq_obito_es_psic, aes(x = ANOOBITO, y = Quantidade, fill = ANOOBITO)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mortes por Psicoativos no Espírito Santo de 2013 a 2022",
       x = "Ano",
       y = "Óbitos")


print(hist_obito_es_psic)








