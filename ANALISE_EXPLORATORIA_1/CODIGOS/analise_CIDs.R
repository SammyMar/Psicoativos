


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
  mutate(prop = Contagens*100/sum(Contagens)),

  dados.count.CID %>%
  filter(Localidade == "Espírito Santo") %>%
  mutate(prop = Contagens*100/sum(Contagens))

)



# montando o plot ---------------------------------------------------------

g_count_CIDs <- dados.count.CID %>%
  ggplot(aes(x = fct_reorder(CIDs, prop), y = prop)) +
  geom_col(aes(text = paste("CID: ", CIDs, "<br>Percentual: ", 
                            round(prop, 2), "%"))
           ,fill = paleta_hist(1)) +
  facet_grid(rows = vars(Localidade))+
  labs(title = "Porcentual de Óbitos por CID no Brasil e Espírito Santo", y="Percentual", x="CIDs")+
  theme_classic() + coord_flip() + theme(title = element_text(size = 15),
                                         axis.text = element_text(size = 13),
                                         strip.text = element_text(face = "bold", size = 15))

ggplotly(g_count_CIDs, tooltip = "text")


save(g_count_CIDs, file="GRAFICOS_RDA/g_count_CIDs.RData")


# preparando dados para heatmap por cids vs anos ------------------------------------


dados_heatmap_CIDs <- dados_es_psic %>% select(CAUSABAS, ANOOBITO) %>%
                      group_by(CIDs = substr(CAUSABAS, 1, 3), ANOOBITO) %>%
                      select(ANOOBITO, CIDs) %>%
                      table() %>%
                      prop.table(margin = 1)




# construindo heatmap por cids vs anos ------------------------------------

dados_heatmap_CIDs <- melt(dados_heatmap_CIDs)

heatmap_CIDS <- ggplot(dados_heatmap_CIDs,
                       aes(ANOOBITO, CIDs, fill = value*100,
                            text = paste("Ano: ", ANOOBITO, 
                                         "<br>CID: ", CIDs,
                                         "<br>Percentual: ", 
                                            round(value*100, 2), "%"))) +
  geom_tile(color="white") +
  scale_fill_gradient(low = "lightblue", high = "#010440")+
  theme_minimal() +
  labs(x = "Anos", y = "CIDs", fill = "%",
       title = "Percentual de Óbitos por CID - ES") +
  theme(axis.text.x = element_text(size = 13),
       axis.text.y = element_text(size = 15),
       title = element_text(size = 14))+
  scale_x_discrete(limits = dados_heatmap_CIDs$ANOOBITO,position = "bottom")

ggplotly(heatmap_CIDS, tooltip = "text")

save(heatmap_CIDS, file="GRAFICOS_RDA/heatmap_CIDS.RData")


