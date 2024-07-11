#comparando ES e BR
dados_comparativos <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
  Feminino = c(freq_dados1["Feminino"], freq_dados2["Feminino"]))
 
  
  
  #para apagar valores 
  rm(linha_todos_na_br)
  

  # Contar frequências de sexo nos dois datasets
  freq_dados1 <- table(dados.filt.es$sexo)
  freq_dados2 <- table(dados.filt.br$sexo)
  
  # Juntar os dados em um único dataframe para facilitar a criação do gráfico
  dados_comparativos <- data.frame(
    Dataset = c("ES", "BR"),
    Masculino = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
    Feminino = c(freq_dados1["Feminino"], freq_dados2["Feminino"])
  )
  
  # Gráfico de barras comparativo
  barplot(as.matrix(dados_comparativos[, -1]), beside = TRUE, 
          legend.text = TRUE, col = c("#4357AD", "#EFA9AE"),
          main = "Comparação de mortos por Sexo entre BR e ES",
          xlab = "Dataset", ylab = "Número de Pessoas")
  
  # Adicionar legenda
  legend("topright", legend =  c("Espírito Santo","Brasil"), fill = c("#4357AD", "#EFA9AE"))
  
  
 # indicador de morte es em relacao com br
  indicador.ps=(887/33806)*100
indicador  

#as mortes no es representam 2,6% das mortes no brasil durante periodo de 2014 a 2017

populacao.br.2010 <- 196400000 
populacao.es.2010 <- 3514952

populacão.relativa <- (populacao.es.2010/populacao.br.2010)*100




dados_comparativos <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
  Feminino = c(freq_dados1["Feminino"], freq_dados2["Feminino"]))

freq.F102.es <- table(dados.F102$sexo)
freq.F102.br <- table(dados.F102.br$sexo)


# sexo e F102

freq.F102.es <- table(dados.F102$sexo)
freq.F102.br <- table(dados.F102.br$sexo)

dados.comparativos.F102 <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq.F102.es["Masculino"], freq.F102.br["Masculino"]),
  Feminino = c(freq.F102.es["Feminino"], freq.F102.br["Feminino"]))

# Gráfico de barras comparativo
barplot(as.matrix(dados.comparativos.F102[, -1]), beside = TRUE, 
        legend.text = TRUE, col = c("#4357AD", "#EFA9AE"),
        main = "Comparação de óbtos por Sexo entre BR e ES na F102",
        xlab = "Dataset", ylab = "Número de Pessoas")

# Adicionar legenda
legend("topright", legend =  c("Espírito Santo","Brasil"), fill = c("#4357AD", "#EFA9AE"))
dados.comparativos.F102

# sexo e F172

freq.F172.es <- table(dados.F172$sexo)
freq.F172.br <- table(dados.F172.br$sexo)

dados.comparativos.F172 <- data.frame(
  Dataset = c("ES", "BR"),
  Masculino = c(freq.F172.es["Masculino"], freq.F172.br["Masculino"]),
  Feminino = c(freq.F172.es["Feminino"], freq.F172.br["Feminino"]))

# Gráfico de barras comparativo
barplot(as.matrix(dados.comparativos.F172[, -1]), beside = TRUE, 
        legend.text = TRUE, col = c("#4357AD", "#EFA9AE"),
        main = "Comparação de óbtos por Sexo entre BR e ES na F172",
        xlab = "Dataset", ylab = "Número de Pessoas")

# Adicionar legenda
legend("topright", legend =  c("Espírito Santo","Brasil"), fill = c("#4357AD", "#EFA9AE"))
dados.comparativos.F172



data.frame(Dataset = c("ES", "BR"),
           'Masculino Geral' = c(freq_dados1["Masculino"], freq_dados2["Masculino"]),
           'Feminino Geral' = c(freq_dados1["Feminino"], freq_dados2["Feminino"]),
           'Masculino F102' = c(freq.F102.es["Masculino"], freq.F102.br["Masculino"]),
          'Feminino F102' = c(freq.F102.es["Feminino"], freq.F102.br["Feminino"]),
         'Masculino F172'= c(freq.F172.es["Masculino"], freq.F172.br["Masculino"]),
          'Feminino F172'= c(freq.F172.es["Feminino"], freq.F172.br["Feminino"]))
