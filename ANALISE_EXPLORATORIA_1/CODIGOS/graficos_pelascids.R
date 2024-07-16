#------------- ORGANIZANDO A BASE DE DADOS DAS CIDS COM MAIS MORTES-------------------------------
# gráfico dos tipos de mortes, usando causabas

#criando bases filtradas pelas cidis com maior número de mortos no es
dados.F10.ES <- subset(dados_es_psic, grepl("^F10[0-9]$", CAUSABAS))
dados.F17.ES <- subset(dados_es_psic, grepl("^F17[0-9]$", CAUSABAS))
dados.F10.BR <- subset(dados_br_psic, grepl("^F10[0-9]$", CAUSABAS))
dados.F17.BR <- subset(dados_br_psic, grepl("^F17[0-9]$", CAUSABAS))

  #------------- GRÁFICOS SOCIODEMOGRÁFICOS PELAS CIDS MAIS COMUNS (ÁLCOOL E FUMO)-------------------------------

#------------ ÓBITOS X IDADE ---------------

#1.histograma de mortos x idade por F10 (álcool)
#1.1 es
hist.F10.es <- ggplot(data = dados.F10.ES, aes(x = IDADE2)) +
  geom_histogram(binwidth = 4, fill = "#9A3D6A", color = "black", alpha = 0.8) +
  labs(
    title = "Óbitos por idade causadas pelo uso de álcool no Espírito Santo",
    x = 'Idade',
    y= "Número de mortos"
  )+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 12),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

hist.F10.es

#1.2 br
hist.F10.br <- ggplot(data = dados.F10.BR, aes(x = IDADE2)) +
  geom_histogram(binwidth = 4, fill = "#9A3D6A", color = "black", alpha = 0.8) +
  labs(
    title = "Óbitos por idade causadas pelo uso de álcool no Brasil",
    x = 'Idade',
    y= "Número de mortos"
  )+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

hist.F10.br


# 2. histograma de mortos x idade de f17 (fumo) 
# 2.1 es
hist.F17.es <- ggplot(data = dados.F17.ES, aes(x = IDADE2)) +
  geom_histogram(binwidth = 4, fill = "#9A3D6A", color = "black", alpha = 0.8) +
  labs(
    title = "Óbitos por idade causadas pelo fumo no Espírito Santo",
    x = 'Idade',
    y= "Número de mortos"
  )+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

hist.F17.es

#2.2 br
hist.F17.br <- ggplot(data = dados.F17.BR, aes(x = IDADE2)) +
  geom_histogram(binwidth = 4, fill = "#9A3D6A", color = "black", alpha = 0.8) +
  labs(
    title = "Óbitos por idade causadas pelo fumo no Brasil",
    x = 'Idade',
    y= "Número de mortos"
  )+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 13),   # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),                 # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),                  # Tamanho dos textos dos eixos
    legend.title = element_text(size = 15),               # Tamanho do título da legenda
    legend.text = element_text(size = 12)                 # Tamanho do texto da legenda
  )

hist.F17.br

#------------ ÓBITOS X GêNERO ---------------
#SERIE


#3. quantidade de mortes por genero - PELO CONSUMO DE ÁLCOOL (F10)
# 3.1 BR
# montando os dados
dados.genero.br.F10 <- data.frame(
  dados.F10.BR %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_br_f10 <- ggplot(data = dados.genero.br.F10, aes(x = ANOOBITO, y = N.obitos, 
                                                                    colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Óbitos causados pelo uso de álcool no Brasil por Gênero", 
       x="Anos", y="Número óbtos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.br.F10$ANOOBITO,  
    labels = dados.genero.br.F10$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 12),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 13),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_genero_br_f10)

# 3.2 ES
# montando os dados
dados.genero.es.F10 <- data.frame(
  dados.F10.ES %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_es_f10 <- ggplot(data = dados.genero.es.F10, aes(x = ANOOBITO, y = N.obitos, 
                                                               colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Óbitos causados pelo uso de álcool no Espírito Santo por Gênero", 
       x="Anos", y="Número óbtos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.F10$ANOOBITO,  
    labels = dados.genero.es.F10$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 13),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_genero_es_f10)


# 4. quantidade de mortes por genero - PELO FUMO (F17)
#4.1 BR
# montando os dados
dados.genero.br.F17 <- data.frame(
  dados.F17.BR %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_br_f17 <- ggplot(data = dados.genero.br.F17, aes(x = ANOOBITO, y = N.obitos, 
                                                               colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Número de Óbitos causados pelo fumo no Brasil por Gênero", 
       x="Anos", y="Número óbtos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.br.F17$ANOOBITO,  
    labels = dados.genero.br.F17$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 13),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_genero_br_f17)

#4.2 ES
# montando os dados
dados.genero.es.F17 <- data.frame(
  dados.F17.ES %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)

# grafico
series_genero_es_f17 <- ggplot(data = dados.genero.es.F17, aes(x = ANOOBITO, y = N.obitos, 
                                                               colour = SEXO)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = SEXO)) +
  labs(title = "Óbitos causados pelo fumo no Espírito Santo por Gênero", 
       x="Anos", y="Número óbtos", colour = "Sexo") +
  scale_x_continuous(
    breaks = dados.genero.es.F17$ANOOBITO,  
    labels = dados.genero.es.F17$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 13),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 13),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_genero_es_f17)

#------------ ÓBITOS X ESCOLARIDADE ---------------

#5. quantidade de mortes por escolaridade - PELO CONSUMO DE ÁLCOOL (F10)
# 5.1 BR
freq_escolaridade_br_f10 <- dados.F10.BR %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_f10 <- ggplot(freq_escolaridade_br_f10, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Óbitos pelo consumo de álcool no Brasil por Escolaridade",
    x = "Escolaridade",
    y = "Número de Pessoas"
  ) +
  theme(
    plot.title = element_text(size = 13),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text.y = element_text(size = 13),      # Tamanho dos textos dos eixos
    axis.text.x = element_blank(),              # Removendo o texto do eixo x
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print(hist_escolaridade_br_f10)

#5.2 ES

freq_escolaridade_es_f10 <- dados.F10.ES %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_f10 <- ggplot(freq_escolaridade_es_f10, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Óbitos pelo consumo de álcool no ES por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")+
  theme(
    plot.title = element_text(size = 13),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text.y = element_text(size = 13),      # Tamanho dos textos dos eixos
    axis.text.x = element_blank(),              # Removendo o texto do eixo x
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print(hist_escolaridade_es_f10)

# 6. quantidade de mortes por escolaridade - PELO FUMO(F17)

#6.1 BR
freq_escolaridade_br_f17 <- dados.F17.BR %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_br_f17 <- ggplot(freq_escolaridade_br_f17, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Óbitos pelo fumo no Brasil por Escolaridade",
    x = "Escolaridade",
    y = "Número de Pessoas"
  ) +
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text.y = element_text(size = 13),      # Tamanho dos textos dos eixos
    axis.text.x = element_blank(),              # Removendo o texto do eixo x
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print(hist_escolaridade_br_f17)

#6.2 ES

freq_escolaridade_es_f17 <- dados.F17.ES %>%
  group_by(ESC) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

hist_escolaridade_es_f17 <- ggplot(freq_escolaridade_es_f17, aes(x = ESC, y = Quantidade, fill = ESC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Óbitos pelo fumo no ES por Escolaridade",
       x = "Escolaridade",
       y = "Número de Pessoas")+
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text.y = element_text(size = 13),      # Tamanho dos textos dos eixos
    axis.text.x = element_blank(),              # Removendo o texto do eixo x
    legend.title = element_text(size = 13),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print(hist_escolaridade_es_f17)

#------------ ÓBITOS X raça ---------------
#SERIE

#7 quantidade de mortes por raca - PELO CONSUMO DE ALCOOL (F10) 

#7.1 BR
# montando os dados
dados.raca.br.f10 <- data.frame(
  dados.F10.BR %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_br_F10 <- ggplot(data = dados.raca.br.f10, aes(x = ANOOBITO, y = N.obitos, 
                                                                colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title ="Óbitos pelo consumo de álcool no Brasil por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.br.f10$ANOOBITO,  
    labels = dados.raca.br.f10$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 14),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_raca_br_F10)

# 7.2 ES

# montando os dados
dados.raca.es.f10 <- data.frame(
  dados.F10.ES %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_es_F10 <- ggplot(data = dados.raca.es.f10, aes(x = ANOOBITO, y = N.obitos, 
                                                           colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Óbitos pelo consumo de álcool no Espírito Santo por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.es.f10$ANOOBITO,  
    labels = dados.raca.es.f10$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 14),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_raca_es_F10)

# 8. quantidade de mortes por raca - PELO FUMO (F17)

#8.1 BR
# montando os dados
dados.raca.br.f17 <- data.frame(
  dados.F17.BR %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_br_F17 <- ggplot(data = dados.raca.br.f17, aes(x = ANOOBITO, y = N.obitos, 
                                                           colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Óbitos pelo fumo no Brasil por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.br.f17$ANOOBITO,  
    labels = dados.raca.br.f17$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 14),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_raca_br_F17)

# 8.2 ES

# montando os dados
dados.raca.es.f17 <- data.frame(
  dados.F17.ES %>% group_by(ANOOBITO, RACACOR) %>% 
    summarise(N.obitos = n())
)

# grafico
series_raca_es_F17 <- ggplot(data = dados.raca.es.f17, aes(x = ANOOBITO, y = N.obitos, 
                                                           colour = RACACOR)) +
  geom_line(linewidth = 0.5, linetype = "solid") +
  geom_point(shape = 15, aes(colour = RACACOR)) +
  labs(title = "Óbitos pelo fumo no Espírito Santo por Raça", 
       x="Anos", y="Óbitos Totais", colour = "Raça") +
  scale_x_continuous(
    breaks = dados.raca.es.f17$ANOOBITO,  
    labels = dados.raca.es.f17$ANOOBITO)+ 
  theme_classic()+
  theme(
    plot.title = element_text(size = 14),       # Tamanho e estilo do título do gráfico
    axis.title = element_text(size = 15),       # Tamanho dos títulos dos eixos
    axis.text = element_text(size = 12),      # Tamanho dos textos dos eixos
    legend.title = element_text(size = 14),     # Tamanho do título da legenda
    legend.text = element_text(size = 12)       # Tamanho do texto da legenda
  )

print (series_raca_es_F17)
