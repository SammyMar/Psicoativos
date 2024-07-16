#library(remotes) 
#library(microdatasus) 
#library(dplyr)
#library(stringr) 
#library(ggplot2) 
#library(lubridate) 



# -------------- ANALISE RACA  -------------

#HISTOGRAMA

#1.1 quantidade de mortes por raca no brasil
freq_raca_br_total <- dados_br_total %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_br_total <- ggplot(freq_raca_br_total, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15)) +
  labs(title = "Quantidades de mortes por raça no Brasil",
       x = "Raça",
       y = "Número de Pessoas")

print(hist_raca_br_total)

#1.2 quantidade de mortes por raca no espirito santo

freq_raca_br_psic <- dados_br_psic %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_br_psic <- ggplot(freq_raca_br_psic, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 20)) +
  labs(title = "Quantidade de mortes por psicoativo no Brasil por raça",
       x = "Raça",
       y = "Número de Pessoas")

print(hist_raca_br_psic)

#1.3 quantidade de mortes por psicoatvos por raca no brasil

freq_raca_es_total <- dados_es_total %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_es_total <- ggplot(freq_raca_es_total, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15)) +
  labs(title = "Quantidade de mortes por raça no ES",
       x = "Raça",
       y = "Número de Pessoas")

print(hist_raca_es_total)

#1.4 quantidade de mortes por psicoativos por raca no espirito santo 

freq_raca_es_psic <- dados_es_psic %>%
  group_by(RACACOR) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_raca_es_psic <- ggplot(freq_raca_es_psic, aes(x = RACACOR, y = Quantidade, fill = RACACOR)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(size = 15)) +
  labs(title = "Quantidade de mortes por psicoativo no ES por raça",
       x = "Raça",
       y = "Número de Pessoas")

print(hist_raca_es_psic)






#SERIE

#SERIE

#2.1 quantidade de mortes por raca no brasil

# montando os dados
dados.raca.br.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_br_total <- ggplot(data = dados.raca.br.series, aes(x = ANOOBITO, y = N.obitos, 
                                                                    colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Número de óbitos Totais no Brasil por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.br.series$ANOOBITO,  
    labels = dados.raca.br.series$ANOOBITO)+ 
  theme(plot.title = element_text(size = 15)) 

print (series_raca_br_total)


#2.2 quantidade de mortes por genero no espirito santo

# montando os dados
dados.raca.es.series <- data.frame(
  dados_es_total %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_es_total <- ggplot(data = dados.raca.es.series, aes(x = ANOOBITO, y = N.obitos, 
                                                                    colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Número de óbitos Totais no ES por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.es.series$ANOOBITO,  
    labels = dados.raca.es.series$ANOOBITO)+ 
  theme(plot.title = element_text(size = 15)) 

print (series_raca_es_total)


#2.1 quantidade de mortes por psicoativos por genero no brasil

# montando os dados
dados.raca.br.series.psic <- data.frame(
  dados_br_psic %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_br_psic <- ggplot(data = dados.raca.br.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                        colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Número de óbitos por psicativos no Brasil por Raça", 
       x="Anos", y="Óbitos por psicoativos", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.br.series.psic$ANOOBITO,  
    labels = dados.raca.br.series.psic$ANOOBITO)+ 
  theme(plot.title = element_text(size = 15)) 

print (series_raca_br_psic)


#2.2 quantidade de mortes por psicoativos por raca no espirito santo

# montando os dados
dados.raca.es.series.psic <- data.frame(
  dados_es_psic %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_es_psic <- ggplot(data = dados.raca.es.series.psic, aes(x = ANOOBITO, y = N.obitos, 
                                                                        colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Número de óbitos por psicoativos no ES por genero", 
       x="Anos", y="Óbitos por psicoativos", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.es.series.psic$ANOOBITO,  
    labels = dados.raca.es.series.psic$ANOOBITO)+ 
  theme(plot.title = element_text(size = 15)) 

print (series_raca_es_psic)



#GRAFICO dE PROPORCAO Brasil

#1 Criando coluna de proporC'C#o

dados.raca.br.series.psic <- dados.raca.br.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o grC!fico
proporcao_raca_br_psic <- ggplot(dados.raca.br.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = RACACOR)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raca",
       title = "Proporção de Mortes por Psicoativos no BR por Raça e Ano") +
  theme(plot.title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Paired")

print(proporcao_raca_br_psic)

#GRAFICOD E PROPORCAO ES

#1 Criando coluna de proporC'C#o

dados.raca.es.series.psic <- dados.raca.es.series.psic %>%
  group_by(ANOOBITO) %>%
  mutate(total_mortes = sum(N.obitos),
         porcentagem = (N.obitos / total_mortes) * 100) %>%
  ungroup()

#2 Criar o grC!fico
proporcao_raca_es_psic <- ggplot(dados.raca.es.series.psic, aes(x = factor(ANOOBITO), y = porcentagem, fill = RACACOR)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Ano", y = "Porcentagem (%)", fill = "Raça",
       title = "Proporção de Mortes por Psicoativos no ES por Raça e Ano") +
  theme(plot.title = element_text(size = 14)) +
  scale_fill_brewer(palette = "Paired")

print(proporcao_raca_es_psic)

