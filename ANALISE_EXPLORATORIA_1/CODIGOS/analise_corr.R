data.selec <- dados_es_psic %>% 
  select(CAUSABAS, SEXO, RACACOR) %>% 
  group_by(CIDs = substr(CAUSABAS, 1, 3), SEXO) %>% 
  select(SEXO, CIDs, RACACOR) %>%
  filter(CIDs == "F10" | CIDs == "F17")

data.selec



C_ajust.CIDs_es <- c() 

for (i in col_data.selec){
  
  tabela <- table(data.selec$CIDs , data.selec[[i]])
  qui2 <- stats::chisq.test(tabela)$statistic
  k <- min(nrow(tabela), ncol(tabela))
  C[i] <- sqrt(qui2/(qui2+sum(tabela)))
  C_ajust.CIDs_es[i] <- sqrt(qui2/(qui2+sum(tabela)))/sqrt((k-1)/k)
}

C_ajust.CIDs_es



C_ajust.CIDs_br <- c() 

for (i in col_data.selec){
  
  tabela <- table(data.selec$CIDs , data.selec[[i]])
  qui2 <- stats::chisq.test(tabela)$statistic
  k <- min(nrow(tabela), ncol(tabela))
  C[i] <- sqrt(qui2/(qui2+sum(tabela)))
  C_ajust.CIDs_br[i] <- sqrt(qui2/(qui2+sum(tabela)))/sqrt((k-1)/k)
}
















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





library(gtsummary)
# CRIANDO TABELAS DE CONTINGENCIA POR CIDS X SEXO --------------------------------

sexo_CIDs <- dados_es_psic %>% 
select(CAUSABAS, SEXO) %>% 
  mutate(CIDs = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                       substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  #select(SEXO, CIDs) %>%
  #table()
  tbl_cross(col = CIDs, row = SEXO) %>% 
  bold_labels()


# CRIANDO TABELA DE CONTINGENCIA POR CIDS X RACA --------------------------



raca_CIDs <- dados_es_psic %>% 
  select(CAUSABAS, RACACOR) %>% 
  mutate(CIDs = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                       substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  mutate(RaçaCor=case_when(RACACOR == "Branca"~"Branca",
                   RACACOR == "Parda"~"Outras",
                   RACACOR == "Indígena"~"Outras",
                   RACACOR == "Amarela"~"Outras",
                   RACACOR == "Preta"~"Outras")) %>% 
  #select(SEXO, CIDs) %>%
  #table() %>% as.data.frame() %>%
  tbl_cross(col = CIDs, row = RaçaCor, missing = "ifany",
            missing_text = "NA", ) %>% 
  bold_labels()

raca_CIDs


sexo_CIDs
Qp_sexo_CIDs <-  sexo_CIDs %>% chisq.test()
Qp_sexo_CIDs

Coef_contg_ajust <- function(Qp, tabela){
  k <- min(nrow(tabela), ncol(tabela))
  phi <- sqrt(Qp/sum(tabela))
  v_cramer<-sqrt(phi/k)
  return(v_cramer)
}

Coef_contg_ajust(Qp_sexo_CIDs$statistic, sexo_CIDs)


# CRIANDO TABELAS DE CONTINGENCIA POR CIDS X RACA/COR --------------------------------

dados_es_psic %>% 
  select(CAUSABAS, RACACOR) %>% 
  group_by(CIDs = substr(CAUSABAS, 1, 3), RACACOR) %>% 
  select(RACACOR, CIDs) %>%
  table() %>% 
  kable()
