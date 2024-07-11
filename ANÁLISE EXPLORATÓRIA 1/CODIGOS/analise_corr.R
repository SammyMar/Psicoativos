data.selec <- dplyr::select(dados_es_psic, RACACOR , ESTCIV , SEXO)

data.selec

col_data.selec=1:3
C_ajust.RACACOR  <- c()
C_ajust.ESTCIV <- c()
C_ajust.SEXO   <- c()


colunas.selec <- c()
C <- c()

for (i in col_data.selec){
  
  tabela <- table(data.selec$RACACOR , data.selec[[i]])
  qui2 <- stats::chisq.test(tabela)$statistic
  k <- min(nrow(tabela), ncol(tabela))
  C[i] <- sqrt(qui2/(qui2+sum(tabela)))
  C_ajust.RACACOR [i] <- sqrt(qui2/(qui2+sum(tabela)))/sqrt((k-1)/k)
}

for (i in col_data.selec){
  
  tabela <- table(data.selec$ESTCIV , data.selec[[i]])
  qui2 <- stats::chisq.test(tabela)$statistic
  k <- min(nrow(tabela), ncol(tabela))
  C[i] <- sqrt(qui2/(qui2+sum(tabela)))
  C_ajust.ESTCIV [i] <- sqrt(qui2/(qui2+sum(tabela)))/sqrt((k-1)/k)
}

for (i in col_data.selec){
  
  tabela <- table(data.selec$SEXO  , data.selec[[i]])
  qui2 <- stats::chisq.test(tabela)$statistic
  k <- min(nrow(tabela), ncol(tabela))
  C[i] <- sqrt(qui2/(qui2+sum(tabela)))
  C_ajust.SEXO  [i] <- sqrt(qui2/(qui2+sum(tabela)))/sqrt((k-1)/k)
}



corr.covariaveis <- data.frame(C_ajust.RACACOR ,
                               C_ajust.ESTCIV ,
                               C_ajust.SEXO)
rownames(corr.covariaveis) <- c("Raça", 
                                "Estado Civil", 
                                "Sexo")
colnames(corr.covariaveis) <- c("Raça", 
                                "Estado Civil", 
                                "Sexo")

cor_melt <- melt(as.matrix(corr.covariaveis))

cor_melt$Var1 <- factor(cor_melt$Var1, levels = rev(rownames(corr.covariaveis)))


heatmap <- ggplot(cor_melt, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Covariáveis", y = "Covariáveis", fill = "Coeficiente")+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0),
    axis.title.y = element_blank()
  ) +
  scale_x_discrete(position = "top")
heatmap
