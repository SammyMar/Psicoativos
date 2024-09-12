# Carregar pacotes
library(remotes)
library(microdatasus)
library(dplyr)
library(tidyr)
library(stringr) 
library(ggplot2) 
library(lubridate)

#HISTOGRAMA

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


#GRAFICO DE SERIE

library(ggplot2)

dados_br_total$DTOBITO <- ymd(dados_br_total$DTOBITO)
dados_br_total$ANOOBITO <- year(dados_br_total$DTOBITO)



# SERIES ANO x NUMERO DE OBITOS

# MONTANDO OS DADOS

dados.grafico.series <- data.frame(
  dados_br_total %>% group_by(ANOOBITO) %>% 
    summarise(N.obitos = n())
)


# GRAFICO

seu_grafico <- ggplot(data = dados.grafico.series, aes(x = ANOOBITO, y = N.obitos)) +
                geom_line(linetype = "solid" ,color = "black",
                          linewidth = 0.5) +
                geom_point(shape = 15, color = "black") +
                labs(title = "Número de Óbitos Totais no Brasil de 2013 a 2022", 
                     x="Anos", y="Óbitos Totais") +
                scale_x_continuous(
                    breaks = dados.grafico.series$ANOOBITO,  
                    labels = dados.grafico.series$ANOOBITO)+ 
                theme_classic()
seu_grafico



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