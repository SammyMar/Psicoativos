install.packages("janitor")
install.packages("microdatasus")
library(janitor)
library(microdatasus)
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
hist(dados.filt.es$idad_eanos,breaks = 18,axis(1, seq(0, 100, by = 10)),main = paste("Histograma de" , "mortos por idade no ES"),xlab = 'Idade', ylab='Numero de mortos')
dados.filt.es$idad_eanos

#colunas do dataframe
colnames(dados.filt.es)

# gráficos coma escolaridade
dados.filt.es$escfalagr1<-dados.filt.es$escfalagr1|>as.numeric()
dados.filt.es$escfalagr1


#criação da variavel escolaridade, com base na variável escfalagr1
dados.filt.es$escolaridade <- ifelse(dados.filt.es$escfalagr1 == 0, "Sem escolaridade",
                                     ifelse(dados.filt.es$escfalagr1 == 1, "Ensino fundamental I incompleto",
                                            ifelse(dados.filt.es$escfalagr1 == 2, "Ensino fundamental I completo",
                                                   ifelse(dados.filt.es$escfalagr1 == 3, "Ensino fundamental II incompleto",
                                                          ifelse(dados.filt.es$escfalagr1 == 4, "Ensino fundamental II completo",
                                                                 ifelse(dados.filt.es$escfalagr1 == 5, "Ensino médio incompleto",
                                                                        ifelse(dados.filt.es$escfalagr1 == 6, "Ensino médio completo",
                                                                               ifelse(dados.filt.es$escfalagr1 == 7, "Ensino superior incompleto",
                                                                                      ifelse(dados.filt.es$escfalagr1 == 8, "Ensino superior completo",
                                                                                             ifelse(dados.filt.es$escfalagr1 == 9, "Ignorado",
                                                                                                    ifelse(dados.filt.es$escfalagr1 %in% c(10, 11, 12), "Inespecífico",
                                                                                                           NA)))))))))))


dados.filt.es$escolaridade
table(dados.filt.es$escolaridade)


# Calculando a contagem de cada categoria de escolaridade
contagem <- table(dados.filt.es$escolaridade)
#Criando o Gráfico de Barras:
barplot(contagem, main = "Contagem por Categoria de Escolaridade",
        xlab = "Categoria de Escolaridade", ylab = "Contagem")

quantidade <- table(dados.filt.es$escfalagr1)
barplot(quantidade, main = "Contagem por Categoria de Escolaridade",
        xlab = "Categoria de Escolaridade", ylab = "Quantidade")

# gráfico dos tipos de mortes, usando causabas
table(dados.filt.es$causabas)
barplot(table(dados.filt.es$causabas), main = "mortes por cada categoria de CID 10", xlab = "Categoria CID 10",ylab = "Número de mortes")
#F102 FOI A MAIS SIGNIFICATIVA Transtornos mentais e comportamentais devidos ao uso de álcool - síndrome de dependência

#relacionando graus de escolaridade com tipo de morte 
plot(dados.filt.es$causabas(),dados.filt.es$escolaridade)
