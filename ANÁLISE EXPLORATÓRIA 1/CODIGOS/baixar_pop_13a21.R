pacman::p_load("sidrar")



pop13a21 <- get_sidra(api = "/t/6579/n1/all/n3/all/v/all/p/last%209")

pop13a21 <- clean_names(pop13a21)
  
pop13a21 <- pop13a21 %>% dplyr::select(codigoUF = brasil_e_unidade_da_federacao_codigo, ano, valor)



