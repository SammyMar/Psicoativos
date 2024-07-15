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





#GRAFICO DE PROPORCAO

# Juntando as duas bases de dados por ano
obitos_combinados_br_es <- freq_obito_br_psic %>%
  rename(Quantidade_BR = Quantidade) %>%
  inner_join(freq_obito_es_psic %>% rename(Quantidade_ES = Quantidade), by = "ANOOBITO")

# Calculando as proporções de mortes no ES e no restante do Brasil
obitos_combinados_br_es <- obitos_combinados_br_es %>%
  mutate(Quantidade_Restante_BR = Quantidade_BR - Quantidade_ES,
         total_mortes = Quantidade_BR,
         porcentagem_ES = (Quantidade_ES / total_mortes) * 100,
         porcentagem_Restante_BR = (Quantidade_Restante_BR / total_mortes) * 100) %>%
  select(ANOOBITO, porcentagem_ES, porcentagem_Restante_BR) %>%
  pivot_longer(cols = c(porcentagem_ES, porcentagem_Restante_BR), 
               names_to = "Regiao", 
               values_to = "Porcentagem")

# Plotando o gráfico de proporção
grafico_proporcao <- ggplot(obitos_combinados_br_es, aes(x = factor(ANOOBITO), y = Porcentagem, fill = Regiao)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Região",
       title = "Proporção de Mortes por Psicoativos no Espírito Santo em relação ao Restante do Brasil anualmente") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "orange"), labels = c("Espírito Santo", "Restante do Brasil"))

# Mostrar o gráfico
print(grafico_proporcao)

