#### GRÁFICO 1 = QTD CIDS ES
n=10:19


# criando vetor de CIDs por psicoativo ------------------------------------

vet.CIDpsic2 <- paste(c("F"), n, 
                      sep = "", collapse = ",")

vet.CIDpsic2 <- strsplit(vet.CIDpsic2, ",") %>% 
  unlist()


# criando contagens das CIDs ES e BR --------------------------------------

contagens.CID_es=c()
contagens.CID_br=c()

for (i in vet.CIDpsic2) {
  
  count_CID <- dados_es_psic$CAUSABAS %>% str_count(pattern = i) %>% 
    sum()
  contagens.CID_es[i] <- count_CID
}

for (i in vet.CIDpsic2) {
  
  count_CID <- dados_br_psic$CAUSABAS %>% str_count(pattern = i) %>% 
    sum()
  contagens.CID_br[i] <- count_CID
}


dados.count.CID <- data.frame(CIDs = vet.CIDpsic2,
                            Contagens = c(contagens.CID_es, contagens.CID_br),
                            Localidade = c(rep("Espírito Santo", 10), 
                                           rep("Brasil", 10)))

dados.count.CID <- full_join(
  
  dados.count.CID %>% 
  filter(Localidade == "Brasil") %>% 
  mutate(prop = Contagens/sum(Contagens)),

  dados.count.CID %>% 
  filter(Localidade == "Espírito Santo") %>% 
  mutate(prop = Contagens/sum(Contagens))

)






g_count_CIDs <- dados.count.CID %>%  
  ggplot(aes(x = fct_reorder(CIDs, prop), y = prop)) +
  geom_col(fill = "#105DEB") +
  facet_grid(rows = vars(Localidade))+
  labs(title = "Quantidade de Óbitos de 2013 a 2022 no ES por cada CID relacionada a psicoativos", y="Quantidade", x="CIDs")+ 
  theme_classic() + coord_flip() + theme(title = element_text(size = 15),
                                         axis.text = element_text(size = 13),
                                         strip.text = element_text(face = "bold", size = 15))

g_count_CIDs
