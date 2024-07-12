


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

contagens.CID_es_anos<-c()
for (i in vet.CIDpsic2) {
  
  count_CID <- dados_es_psic %>% group_by(CAUSABAS, ANOOBITO) %>% 
    select(CAUSABAS, ANOOBITO)%>% str_count(pattern = i) %>% 
    sum()
  contagens.CID_es_anos[i] <- count_CID
}


dados.count.CID_hm <- data.frame(full_join(

    dados_es_psic %>% 
    group_by(unique(CAUSABAS = substr(CAUSABAS, 1, 3)), ANOOBITO) %>% 
    reframe(Quantidade = n(), ANOOBITO),
    
    dados_br_psic %>% 
      group_by(unique(CAUSABAS = substr(CAUSABAS, 1, 3)), ANOOBITO) %>% 
      reframe(Quantidade = n(), ANOOBITO)
  
)#, 
#Localidade = c(rep("Espírito Santo", 10), rep("Brasil", 10))
)





dados.count.CID_hm <- melt(as.matrix(dados.count.CID %>% filter(Localidade == "Brasil")
                                     %>% select(prop, CIDs, )))

dados.count.CID_hm$Var1 <- factor(dados.count.CID_hm$Var1, levels = rev(rownames(dados.count.CID)))


# construindo heatmap por cids vs anos ------------------------------------


heatmap_CIDS <- ggplot(dados.count.CID_hm, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#105DEB") +
  theme_minimal() +
  labs(x = "Anos", y = "CIDs", fill = TeX("Proporção"))+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0, size = 15),
    axis.title.y = element_blank(), axis.title.x = element_blank()
  ) +
  scale_x_discrete(position = "top")+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        axis.text = element_text(size = 15), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size=14),
        strip.text = element_text(face = "bold", size = 11.5))

heatmap_CIDS




