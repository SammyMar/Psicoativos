library(remotes) 
library(microdatasus) 
library(dplyr)
library(stringr) 
library(ggplot2) 
library(lubridate) 

#ler a base de dados
dados_br_total=c()
for (ano in 2013:2022) {
  
  df <- readRDS(paste0("base_de_dados/dados_", ano,".rds"))
  dados_br_total <- bind_rows(df,dados_br_total)
  
}

# -------------- ANALISE GENERO -------------

#1.1 quantidade de mortes por genero no brasil
frequencia_genero_br <- dados_br_total %>%
  group_by(SEXO) %>%
  summarise(Quantidade = n(), .groups = "drop") %>%
  mutate(Porcentagem = Quantidade / sum(Quantidade) * 100)

#1.2 quantidade de mortes por genero no espirito santo
#1.3 quantidade de mortes por psicoatvos por genero no brasil
#1.4 quantidade de mortes por psicoativos por genero no espirito santo 



