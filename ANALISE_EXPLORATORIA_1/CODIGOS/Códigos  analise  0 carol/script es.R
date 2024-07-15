install.packages("janitor")
install.packages("microdatasus")
library(janitor)
library(microdatasus)
library(dplyr)
library(lubridate)
#Baixar os dados do ES
dados.es <- fetch_datasus(year_start = 2014, year_end = 2017, uf = "ES", information_system = "SIM-DO")
dados.es <- process_sim(dados.es)
#preparação dos dados para filtragem
dados.es <- clean_names(dados.es)
attach(dados.es)
mortalidade.es <- subset(dados.es, grepl("^F1[0-9]{2}$", causabas))

# descobrar colunas inteiras de NA

colunas_todos_na <- which(colSums(is.na(mortalidade.es)) == nrow(mortalidade.es))
print(colunas_todos_na)
dados_filt_es <- mortalidade.es[, -colunas_todos_na]
colnames(dados_filt_es)                         
dados.filt.es <- dados_filt_es 
#Descartar variáveis desnecessárias

#colunas_para_usar <- c('contador','origem', 'dtobito', 'horaobito', 'natural', 'codmunnatu', 'dtnasc', 'idade', 
#'sexo', 'racacor', 'esc', 'esc2010','ocup','codmunres', 'lococor', 'codmunocor', 'necropsia', 'causabas', 'causabas_o',
#'atestado', 'escfalagr1', 'difdata')

#histograma com a idade


 
dados.filt.es$idad_eanos <- dados.filt.es$idad_eanos|> as.numeric()
dados.filt.es$idad_eanos
hist(dados.filt.es$idad_eanos, breaks = 18, 
     main = paste("Histograma de mortos por idade no ES"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))
#colunas do dataframe
colnames(dados.filt.es)

# gráficos coma escolaridade
dados.filt.es$escfalagr1<-dados.filt.es$escfalagr1|>as.numeric()

dados.filt.es$escfalagr1


#criação da variavel escolaridade, com base na variável escfalagr1
dados.filt.es$escfalagr1 <- dados.filt.es$escfalagr1|> as.numeric()
dados.filt.es$escolaridade <- ifelse(dados.filt.es$escfalagr1 == 00, "Sem escolaridade",
                                     ifelse(dados.filt.es$escfalagr1 == 01, "Ensino fundamental I incompleto",
                                            ifelse(dados.filt.es$escfalagr1 == 02, "Ensino fundamental I completo",
                                                   ifelse(dados.filt.es$escfalagr1 == 03, "Ensino fundamental II incompleto",
                                                          ifelse(dados.filt.es$escfalagr1 == 04, "Ensino fundamental II completo",
                                                                 ifelse(dados.filt.es$escfalagr1 == 05, "Ensino médio incompleto",
                                                                        ifelse(dados.filt.es$escfalagr1 == 06, "Ensino médio completo",
                                                                               ifelse(dados.filt.es$escfalagr1 == 07, "Ensino superior incompleto",
                                                                                      ifelse(dados.filt.es$escfalagr1 == 08, "Ensino superior completo",
                                                                                             ifelse(dados.filt.es$escfalagr1 == 09, "Ignorado",
                                                                                                    ifelse(dados.filt.es$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                           NA)))))))))))


dados.filt.es$escolaridade
table(dados.filt.es$escolaridade)

dados.filt.es$escfalagr1






dados.filt.es <- dados.filt.es %>% mutate(escolaridade = case_when( 
  escfalagr1 == "00" ~ "Sem escolaridade", 
  escfalagr1 == 01 ~ "Ensino fundamental I incompleto", 
  escfalagr1 == 02 ~ "Ensino fundamental I completo", escfalagr1 == 03 ~ "Ensino fundamental II incompleto", 
  escfalagr1 == 04 ~ "Ensino fundamental II completo", escfalagr1 == 05 ~ "Ensino médio incompleto", 
  escfalagr1 == 06 ~ "Ensino médio completo", escfalagr1 == 07 ~ "Ensino superior incompleto", escfalagr1 == 08 ~ "Ensino superior completo",
  escfalagr1 == 09 ~ "Ignorado", 
  escfalagr1 %in% c(10, 11, 12) ~ "Inespecífico", 
  is.na(escfalagr1) ~  NA))
# Calculando a contagem de cada categoria de escolaridade
contagem <- table(dados.filt.es$escolaridade)
#Criando o Gráfico de Barras:
barplot(table(dados.filt.es$escolaridade), main = "Número de óbtos por escolaridade",
        xlab = "Escolaridade", ylab = "Número de pessoas")

quantidade <- table(dados.filt.es$escfalagr1)
barplot(quantidade, main = "Contagem por Categoria de Escolaridade",
        xlab = "Categoria de Escolaridade", ylab = "Quantidade")

#nova tentativa para escolaridade
dados.filt.es <- dados.filt.es %>% mutate (escolaridade = case_when(
  escfalagr1 == 00 ~ "Sem escolaridade",
  escfalagr1== 01 ~ "Ensino fundamental I incompleto",
  escfalagr1== 02 ~ "Ensino fundamental I completo",
  escfalagr1 == 03 ~ "Ensino fundamental II incompleto",
  escfalagr1 == 04 ~ "Ensino fundamental II completo",
  escfalagr1 == 05 ~ "Ensino médio incompleto",
  escfalagr1 == 06 ~ "Ensino médio completo",
  escfalagr1 == 07 ~ "Ensino superior incompleto",
  escfalagr1 == 08 ~ "Ensino superior completo",
  escfalagr1 == 09 ~ "Ignorado",
  escfalagr1 == 10 ~ "Inespecífico",
  escfalagr1 == 11 ~ "Inespecífico",
  escfalagr1 == 12 ~ "Inespecífico",
  is.na(escfalagr1) ~ NA_character_))

dados.filt.es$escolaridade


# Visualizando o resultado
print(dados)

# grafico por sexo
barplot(table(dados.filt.es$sexo), main =  "Número de mortos por sexo", xlab = "Sexo", ylab = "Número de pessoas")
table(dados.filt.es$sexo)

#Ocupação
table(dados.filt.es$ocup)
ocupsignificante <- names(table(dados.filt.es$ocup)[table(dados.filt.es$ocup)>= 20]) 
ocupsignificante

#temos um pico em Trabalhador agropecuario em geral (125) 

#gráfico por situação civil
table(dados.filt.es$estciv)
barplot(table(dados.filt.es$estciv), main = "Número de mortos por estado civil", xlab = "Estado civil", ylab = "Número de pessoas")
# gráfico dos tipos de mortes, usando causabas
table(dados.filt.es$causabas)
barplot(table(dados.filt.es$causabas), main = "mortes por cada categoria de CID 10", xlab = "Categoria CID 10",ylab = "Número de mortes")
#F102 FOI A MAIS SIGNIFICATIVA Transtornos mentais e comportamentais devidos ao uso de álcool - síndrome de dependência

#relacionando idade com F102 
dados.F102 <- subset(dados.filt.es, causabas=='F102')

hist(dados.F102$idad_eanos, breaks = 18,
     main = paste("Histograma de mortos por idade na categoria F102 no ES"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))

#escolaridade por F102
barplot(table(dados.F102$escolaridade), main = " Mortos por escolaridade na categoria F102 no ES",
        xlab = 'Escolaridade', ylab = 'Número de mortos') 

#relacionando idade com F172 
dados.F172 <- subset(dados.filt.es, causabas=='F172')

hist(dados.F172$idad_eanos, breaks = 18,
     main = paste("Histograma de mortos por idade na categoria F172 no ES"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))
#média de mortes por ano 
media.es=nrow(dados.filt.es)/4
media.es

#filtrando por ano

class(dados.filt.es$dtobito)
dados.filt.es$dtobito <- as.Date(dados.filt.es$dtobito)

dados.filt.es$dtobito <- ymd(dados.filt.es$dtobito)
str(dados.filt.es$dtobito)

# Filtrar dados para o ano de 2014
dados_2014 <- dados.filt.es %>%
  filter(year(dtobito) == 2014)

# Visualizar as primeiras linhas dos dados filtrados
head(dados_2014)
#quantidade de obtos por ano
contagem_por_ano <- dados.filt.es %>%
  group_by(ano = year(dtobito)) %>%
  summarise(total_obs = n())

# Exibir a contagem por ano
print(contagem_por_ano)


plot(dados.filt.es$idad_eanos,dados.filt.es$escfalagr1)

