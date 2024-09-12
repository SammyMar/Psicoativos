

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

sexo_CIDs


dados_sexo_CIDs <- dados_es_psic %>% 
  select(CAUSABAS, SEXO) %>% 
  mutate(CIDs = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                       substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  select(CIDs, SEXO) %>%
  table() %>% as.matrix()

Qp_sexo <- chisq.test(dados_sexo_CIDs)
Qp_sexo

dados_sexo_CIDs

OR_sexo_f <- (237*305)/(1744*111)



OR_sexo_m <- 1/OR_sexo_f

OR_sexo_m




RP <- function(matriz)
# CRIANDO TABELA DE CONTINGENCIA POR CIDS X RACA --------------------------


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


raca_CIDs

dados_raca_CIDs <- dados_es_psic %>% 
  select(CAUSABAS, RACACOR) %>% 
  mutate(CIDs = ifelse(substr(CAUSABAS, 1, 3) %in% c("F10"),
                       substr(CAUSABAS, 1, 3), "Outras CIDs")) %>% 
  mutate(RacaCor=case_when(RACACOR == "Branca"~"Branca",
                           RACACOR == "Parda"~"Parda",
                           RACACOR == "Indígena"~"Outras",
                           RACACOR == "Amarela"~"Outras",
                           RACACOR == "Preta"~"Outras")) %>% 
  select(RacaCor, CIDs) %>%
  table()

dados_raca_CIDs

Qp_raca <- chisq.test(dados_raca_CIDs)
Qp_raca
