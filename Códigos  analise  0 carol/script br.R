install.packages("janitor")
library(janitor)
library(microdatasus)
library(dplyr)
library(lubridate)
#Baixar os dados do ES

dados.br <- fetch_datasus(year_start = 2014, year_end = 2017, information_system = "SIM-DO")
dados.br <- process_sim(dados.br)
#preparação dos dados para filtragem
dados.br <- clean_names(dados.br)
attach(dados.br)
mortalidade.br <- subset(dados.br, grepl("^F1[0-9]{2}$", causabas))


attach(mortalidade.br)
summary(idad_eanos)
# descobrar colunas inteiras de NA

colunas_todos_na_br <- which(colSums(is.na(mortalidade.br)) == nrow(mortalidade.br))
print(colunas_todos_na)
dados.filt.br <- mortalidade.br[, -colunas_todos_na_br]

linha_todos_na_br <- which(rowSums(is.na(mortalidade.br)) == nrow(mortalidade.br))
 

#GRÁFICOS 

#POR IDADE
dados.filt.br$idad_eanos <- dados.filt.br$idad_eanos|> as.numeric()
table(dados.filt.br$idad_eanos)
hist(dados.filt.br$idad_eanos, 
     main = paste("Histograma de mortos por idade no BR"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))

# POR ESCOLARIDADE
dados.filt.br$escfalagr1<-dados.filt.br$escfalagr1|>as.numeric()
 




barplot(table(dados.filt.br$escfalagr1), main = "Mortos por Escolaridade no BR",
        xlab = " Escolaridade", ylab = "Quantidade")






#criação da variavel escolaridade, com base na variável escfalagr1
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                    ifelse(dados.filt.br$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                           NA)))))))))))


dados.filt.br$escolaridade
table(dados.filt.br$escolaridade)



# Calculando a contagem de cada categoria de escolaridade
contagem <- table(dados.filt.br$escolaridade)
#Criando o Gráfico de Barras:
barplot(table(dados.filt.br$escolaridade), main = "Mortes por Escolaridade no BR",
        xlab = " Escolaridade", ylab = "Número de mortos")

#POR SEXO
barplot(table(dados.filt.br$sexo), main =  "Número de mortos por sexo no BR", xlab = "Sexo", ylab = "Número de pessoas")




table(dados.filt.br$sexo)

#POR OCUPACAO
table(dados.filt.br$ocup)
barplot(table(dados.filt.br$ocup))
ocupsignificante.br <- names(table(dados.filt.br$ocup)[table(dados.filt.br$ocup)>= 500])
ocupsignificante.br1 <-  (table(dados.filt.br$ocup)[table(dados.filt.br$ocup)>= 500])
ocupsignificante.br1

#gráfico por situação civil
table(dados.filt.br$estciv)
barplot(table(dados.filt.br$estciv), main = "Número de mortos por estado civil no BR", xlab = "Estado civil", ylab = "Número de pessoas")
# gráfico dos tipos de mortes, usando causabas
table(dados.filt.br$causabas)
barplot(table(dados.filt.br$causabas), main = "mortes por cada categoria de CID 10 no BR", xlab = "Categoria CID 10",ylab = "Número de mortes")

causabassignificativa <- (table(dados.filt.br$ocup)[table(dados.filt.br$ocup)>= 200])
causabassignificativa



condicao <- table(dados.filt.br$ocup)>= 200
causabassignificativa <- ifelse(dados.filt.br$ocup %in% names(condicao)[condicao], "Significativo", "Não Significativo")



  #F102 FOI A MAIS SIGNIFICATIVA Transtornos mentais e comportamentais devidos ao uso de álcool - síndrome de dependência
#gráfico F102 por idade
dados.F102.br <- subset(dados.filt.br, causabas=='F102')
table(dados.F102.br$idad_eanos)
a=663+670+633 +679+ 659
b=717+ 728 +622 +611 +639
hist(dados.F102.br$idad_eanos, breaks = 18,
     main = paste("Histograma de mortos por idade na categoria F102 no BR"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))

#gráfico F102 por idade
dados.F172.br <- subset(dados.filt.br, causabas=='F172')
table(dados.F172.br$idad_eanos)
hist(dados.F172.br$idad_eanos, breaks = 18,
     main = paste("Histograma de mortos por idade na categoria F172 no BR"),
     xlab = 'Idade', ylab = 'Número de mortos')
axis(1, seq(0, 100, by = 10))

#média de mortes por ano 
media.br=nrow(dados.filt.br)/4
media.br

#filtrar por ano
class(dados.filt.br$dtobito)
dados.filt.br$dtobito <- as.Date(dados.filt.br$dtobito)

dados.filt.br$dtobito <- ymd(dados.filt.br$dtobito)
str(dados.filt.br$dtobito)
#quantidade de obtos por ano
contagem_por_ano_br <- dados.filt.br %>%
  group_by(ano = year(dtobito)) %>%
  summarise(total_obs = n())

# Exibir a contagem por ano
print(contagem_por_ano_br)

