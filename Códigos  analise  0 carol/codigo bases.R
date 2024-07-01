library(janitor)
library(microdatasus)
library(dplyr)
library(lubridate)

#es
dados.es <- fetch_datasus(year_start = 2014, year_end = 2017, uf = "ES", information_system = "SIM-DO")
dados.es <- process_sim(dados.es)
dados.es <- clean_names(dados.es)
attach(dados.es)
mortalidade.es <- subset(dados.es, grepl("^F1[0-9]{2}$", dados.es$causabas))
colunas_todos_na <- which(colSums(is.na(mortalidade.es)) == nrow(mortalidade.es))
print(colunas_todos_na)
dados.filt.es <- mortalidade.es[, -colunas_todos_na]




#br
dados.br <- fetch_datasus(year_start = 2014, year_end = 2017, information_system = "SIM-DO")
dados.br <- process_sim(dados.br)
dados.br <- clean_names(dados.br)
mortalidade.br <- subset(dados.br, grepl("^F1[0-9]{2}$", dados.br$causabas))
colunas_todos_na_br <- which(colSums(is.na(mortalidade.br)) == nrow(mortalidade.br))
dados.filt.br <- mortalidade.br[, -colunas_todos_na_br]
dados.filt.br <-as.data.frame(dados.filt.br)  
dados.filtrados.br <- as.data.frame(dados.filt.br)


dados.filt.es <- as.data.frame(dados.filt.es)
dados.fil.es <- dados.filt.es
dados.fil.br <- dados.filt.br
caminho <- "C:/Users/Alessandra/Documents/dados.csv"
write.csv(dados.fil.br, file = caminho, row.names = FALSE)

caminho.es <- "C:/Users/Alessandra/Downloads/dados.csv"
write.csv(dados.fil.es,file = caminho.es, row.names = FALSE)



freq_dados1 <- table(dados.final.es$sexo)
freq_dados2 <- table(dados.final.br$sexo)
dados_comparativos <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
  Feminino = c(freq_dados1["Feminino"], freq_dados2["Feminino"]))


# Juntar os dados em um único dataframe para facilitar a criação do gráfico
dados_comparativos <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
  Feminino = c(freq_dados1["Feminino"], freq_dados2["Feminino"])
)

# Gráfico de barras comparativo
barplot(as.matrix(dados_comparativos[, -1]), beside = TRUE, 
        legend.text = TRUE, col = c("#4357AD", "#EFA9AE"),
        main = "Comparação de óbitos por Sexo entre BR e ES",
        xlab = "Dataset", ylab = "Número de Pessoas")

# Adicionar legenda
legend("topright", legend =  c("Espírito Santo","Brasil"), fill = c("#4357AD", "#EFA9AE"))


#data frame sexo e cid