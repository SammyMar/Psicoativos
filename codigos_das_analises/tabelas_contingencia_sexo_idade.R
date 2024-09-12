
library(epitools)
library(gtsummary)

# CRIANDO TABELAS DE CONTINGENCIA POR CIDS X SEXO --------------------------------

sexo_CIDs <- dados_es_psic %>% 
  select(CAUSABAS, SEXO) %>% 
  mutate(factor(CIDs) = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                               substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  #select(SEXO, CIDs) %>%
  #table()
  tbl_cross(col = CIDs, row = SEXO) %>% 
  bold_labels()



raca_CIDs <- dados_es_psic %>% 
  select(CAUSABAS, RACACOR) %>% 
  mutate(CIDs = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                       substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  
  mutate(RacaCor=case_when(RACACOR == "Branca"~"Branca",
                           RACACOR == "Parda"~"Parda",
                           RACACOR == "Indígena"~"Outras",
                           RACACOR == "Amarela"~"Outras",
                           RACACOR == "Preta"~"Outras")) %>% 
  #select(SEXO, CIDs) %>%
  #table() %>% as.data.frame() %>%
  tbl_cross(col = CIDs, row = RacaCor, missing = "ifany",
            missing_text = "NA", ) %>% 
  bold_labels()