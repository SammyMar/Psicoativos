
library(ggplot2)

dados_br_total$DTOBITO <- ymd(dados_br_total$DTOBITO)
dados_br_total$ANOOBITO <- year(dados_br_total$DTOBITO)



# SERIES ANO x NUMERO DE OBITOS

# MONTANDO OS DADOS

dados.grafico.series_br <- data.frame(
  dados_br_total %>% group_by(ANOOBITO) %>% 
    summarise(N.obitos = n())
)


# GRAFICO

serie_BR_total <- ggplot(data = dados.grafico.series_br, aes(x = ANOOBITO, y = N.obitos/1000000)) +
                geom_line(linetype = "solid" ,color = "black",
                          linewidth = 0.5) +
                geom_point(shape = 15, color = "black") +
                labs(title = "Número de Óbitos Totais no Brasil de 2013 a 2022", 
                     x="Anos", y="Óbitos Totais x 1000000") +
                scale_x_continuous(
                    breaks = dados.grafico.series_br$ANOOBITO,  
                    labels = dados.grafico.series_br$ANOOBITO)+ 
                theme_classic()



# MONTANDO OS DADOS

dados.grafico.series_es <- data.frame(
  dados_es_total %>% group_by(ANOOBITO) %>% 
    summarise(N.obitos = n())
)


serie_ES_total <- ggplot(data = dados.grafico.series_es, 
                         aes(x = ANOOBITO, y = N.obitos/1000)) +
  geom_line(linetype = "solid" ,color = "black",
            linewidth = 0.5) +
  geom_point(shape = 15, color = "black") +
  labs(title = "Número de Óbitos Totais no Espírito Santo de 2013 a 2022", 
       x="Anos", y="Óbitos Totais x 1000") +
  scale_x_continuous(
    breaks = dados.grafico.series_es$ANOOBITO,  
    labels = dados.grafico.series_es$ANOOBITO)+ 
  theme_classic()

#### SERIES ANO x NUMERO DE OBITOS CONTROLADO POR UMA VARIAVEL ####

# MONTANDO DADOS

dados.grafico2.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO, SEXO) %>% 
    summarise(N.obitos = n())
)



# GRAFICO

seu_grafico2 <- ggplot(data = dados.grafico2.series, aes(x = ANOOBITO, y = N.obitos, 
                                                       colour = SEXO)) +
              geom_line(linewidth = 0.5, linetype = "solid") +
              geom_point(shape = 15, aes(colour = SEXO)) +
              labs(title = "Número de Óbitos Totais no Brasil de 2013 a 2022 por Sexo", 
                       x="Anos", y="Óbitos Totais", colour = "Sexo") +
              scale_x_continuous(
                breaks = dados.grafico.series$ANOOBITO,  
                labels = dados.grafico.series$ANOOBITO)+ 
                theme_classic()

seu_grafico2




