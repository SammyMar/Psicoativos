


# criando vetor de CIDs por psicoativo ------------------------------------

n=10:19


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



# construindo dados das proporções ----------------------------------------


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



# montando o plot ---------------------------------------------------------

g_count_CIDs <- dados.count.CID %>%  
  ggplot(aes(x = fct_reorder(CIDs, prop), y = prop)) +
  geom_col(fill = "#105DEB") +
  facet_grid(rows = vars(Localidade))+
  labs(title = "Proporção de Óbitos de 2013 a 2022 no Brasil e no ES por cada CID relacionada a psicoativos", y="Quantidade", x="CIDs")+ 
  theme_classic() + coord_flip() + theme(title = element_text(size = 15),
                                         axis.text = element_text(size = 13),
                                         strip.text = element_text(face = "bold", size = 15))

g_count_CIDs



# preparando dados para heatmap por cids vs anos ------------------------------------


dados_heatmap_CIDs <- dados_es_psic %>% select(CAUSABAS, ANOOBITO) %>% 
                      group_by(CIDs = substr(CAUSABAS, 1, 3), ANOOBITO) %>% 
                      select(ANOOBITO, CIDs) %>%
                      table() %>% 
                      prop.table(margin = 1)




# construindo heatmap por cids vs anos ------------------------------------

dados_heatmap_CIDs <- melt(dados_heatmap_CIDs)

heatmap_CIDS <- ggplot(dados_heatmap_CIDs, aes(ANOOBITO, CIDs, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = ("darkblue"))+
  theme_minimal() +
  labs(x = "Anos", y = "CIDs", fill = TeX("Proporção"), 
       title = "Proporção de óbitos por cada CID e cada ano no Espírito Santo") +
  theme(axis.text.x = element_text(size = 15),
       axis.text.y = element_text(size = 15),
       title = element_text(size = 15))+
  scale_x_discrete(limits = dados_heatmap_CIDs$ANOOBITO,position = "bottom")
  
heatmap_CIDS




