


# MONTANDO OS DADOS INDICADOR 1

dados_indic1 <- inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  data.frame(
    dados_es_total %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  
  by="ANOOBITO"
  
)

colnames(dados_indic1) <- c("ANOOBITO", "N_obitos_es_psic", "N_obitos_es_total")

attach(dados_indic1)

dados_indic1$indic1 <- (N_obitos_es_psic/N_obitos_es_total)*1000


# montando o grafico
serie_indic1 <- ggplot(data = dados_indic1, aes(x = ANOOBITO, y = indic1)) +
  geom_line(linetype = "solid" ,color = "black",
            linewidth = 0.5) +
  geom_point(shape = 15, color = "black") +
  labs(title = "Número de Óbitos por Psicoativos no ES/Número de Óbitos Totais no ES de 2013 a 2022", 
       x="Anos", y="Óbitos/1000") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,  
    labels = dados.grafico.series$ANOOBITO)+ 
  theme_classic()
serie_indic1





# MONTANDO OS DADOS INDICADOR 2

dados_indic2 <- inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  data.frame(
    dados_br_psic %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  
  by="ANOOBITO"
  
)

colnames(dados_indic2) <- c("ANOOBITO", "N_obitos_es_psic", "N_obitos_br_psic")

attach(dados_indic2)

dados_indic2$indic2 <- (N_obitos_es_psic/N_obitos_br_psic)*100


# montando o grafico
serie_indic2 <- ggplot(data = dados_indic2, aes(x = ANOOBITO, y = indic2)) +
  geom_line(linetype = "solid" ,color = "black",
            linewidth = 0.5) +
  geom_point(shape = 15, color = "black") +
  labs(title = "Número de Óbitos por Psicoativos no ES/Número de Óbitos por Psicoativos no Brasil 
de 2013 a 2022", 
       x="Anos", y="Óbitos/100") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,  
    labels = dados.grafico.series$ANOOBITO)+ 
  theme_classic()
serie_indic2





# MONTANDO OS DADOS INDICADOR 3

dados_indic3 <- inner_join(
  data.frame(
    dados_es_total %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  data.frame(
    dados_br_total %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  
  by="ANOOBITO"
  
)

colnames(dados_indic3) <- c("ANOOBITO", "N_obitos_es_total", "N_obitos_br_total")

attach(dados_indic3)

dados_indic3$indic3 <- (N_obitos_es_total/N_obitos_br_total)*100


# montando o grafico
serie_indic3 <- ggplot(data = dados_indic3, aes(x = ANOOBITO, y = indic3)) +
  geom_line(linetype = "solid" ,color = "black",
            linewidth = 0.5) +
  geom_point(shape = 15, color = "black") +
  labs(title = "Número de Óbitos Totais no ES/Número de Óbitos Totais no Brasil 
de 2013 a 2022", 
       x="Anos", y="Óbitos/100") +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,  
    labels = dados.grafico.series$ANOOBITO)+ 
  theme_classic()
serie_indic3




#DADOS INDIC 2 e 3 

dados_indic2_3 <- data.frame(
      Ano = rep(dados_indic2$ANOOBITO, 2),
      valor = c(dados_indic2$indic2, 
                dados_indic3$indic3),
      Indicador = c(rep("Indicador 2", 10), 
                    rep("Indicador 3", 10))
)


# montando o grafico
serie_indic2_3 <- ggplot(data = dados_indic2_3, aes(x = Ano, y = valor, 
                                                    colour = Indicador)) +
  geom_line(linetype = "solid",
            linewidth = 0.5) +
  geom_point(shape = 15) +
  labs(title = TeX("Indicador 2: $\\frac{Nº óbitos ES psic}{Nº óbitos BR psic}$ e Indicador 3: $\\frac{Nº \ óbitos \ ES \  total}{Nº \ óbitos \ BR \ total}$ de 2013 a 2022"), 
       x="Anos", y="Óbitos/100") +
  scale_colour_manual(values = c("red", "blue"), name = "Indicadores", 
            labels = c("indicador 2", "Indicador 3")) +
  ylim(1.5, 3.5) +
  scale_x_continuous(
    breaks = dados.grafico.series$ANOOBITO,  
    labels = dados.grafico.series$ANOOBITO)+ 
  theme_classic()

serie_indic2_3

### Indicador 4 obitos psic ES/pop ES
pop_es_2022 <- data.frame(brasil_e_unidade_da_federacao_codigo = c(32),
                          ano = c(2022),
                          valor  = c(
                            3833712
                          ))


dados_indic4 <- pop13a21 %>% 
  dplyr::filter(brasil_e_unidade_da_federacao_codigo == 32) %>% 
  rbind(pop_es_2022)

dados_indic4$ano <- as.numeric(dados_indic4$ano)

dados_indic4 <- dados_indic4 %>%  inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  
  by = c("ano" = "ANOOBITO")
)


### Indicador 5 obitos psic BR/pop BR
pop_br_2022 <- data.frame(brasil_e_unidade_da_federacao_codigo = c(1),
                          ano = c(2022),
                          valor  = c(
                            203080756))

dados_indic5 <- pop13a21 %>% 
  dplyr::filter(brasil_e_unidade_da_federacao_codigo == 1) %>% 
  rbind(pop_br_2022)

dados_indic5$ano <- as.numeric(dados_indic5$ano)

dados_indic5 <- dados_indic5 %>%  inner_join(
  data.frame(
    dados_br_psic %>% group_by(ANOOBITO) %>% 
      summarise(N.obitos = n())
  ), 
  
  by = c("ano" = "ANOOBITO")
)



### MONTANDO DADOS
dados_indic4_5 <- data.frame(
  Ano = rep(dados_indic4$ano, 2),
  valor = c(dados_indic4$valor, 
            dados_indic5$valor),
  N.obitos = c(dados_indic4$N.obitos,
               dados_indic5$N.obitos),
  Indicador = c(rep("Indicador 4", 10), 
                rep("Indicador 5", 10))
)


serie_indic4_5 <- ggplot(data = dados_indic4_5, aes(x = Ano, y = (N.obitos/valor)*100000, 
                                                    colour = Indicador)) +
  geom_line(linetype = "solid",
            linewidth = 0.5) +
  geom_point(shape = 15) +
  labs(title = TeX("Indicador 4: $\\frac{Nº óbitos ES psic}{PopulaçãoES}$ e Indicador 5: $\\frac{NºÓbitosBRpsic}{PopulaçãoBR}$ de 2013 a 2022"), 
       x="Anos", y="Óbitos/100000") +
  scale_colour_manual(values = c("red", "blue"), name = "Indicadores", 
                      labels = c("indicador 4", "Indicador 5")) +
  #ylim(1.5, 3.5) +
  scale_x_continuous(
    breaks = dados_indic4_5$Ano,  
    labels = dados_indic4_5$Ano)+ 
  theme_classic()

serie_indic4_5







serie_BR_total
serie_ES_total
serie_indic1
#serie_indic2
#serie_indic3
serie_indic2_3
serie_indic4_5

mapa_psic_obitos_uf
mapa_psic_pop

