
# baixando info estados brasil (geobr) ------------------------------------

brasil = read_state(code_state = "all", year = 2018)

### MAPA DO BRASIL OBITOS POR ESTADO POR PSICO/POP ESTADO 2013 e 2022


### numeros de obitos por estado 13 e 22
dados_mapa_psic_pop13 <- data.frame(
  dados_br_psic %>%  
    filter(ANOOBITO == "2013") %>% 
    group_by(codigoUF, ANOOBITO) %>% 
    summarise(N.obitos = n()))

dados_mapa_psic_pop22 <- data.frame(
  dados_br_psic %>%  
    filter(ANOOBITO == "2022") %>% 
    group_by(codigoUF, ANOOBITO) %>% 
    summarise(N.obitos = n()))

### selecionando pop de 2013 dos estados
pop_UF_13 <- pop13a21 %>% 
  filter(ano == "2013" & codigoUF != 1) %>% 
  reframe(unique(valor), codigoUF, ano)

pop_UF_13 <- rename(pop_UF_13, pop13 = `unique(valor)`)

dados_mapa_psic_pop13<- left_join(dados_mapa_psic_pop13,pop_UF_13,
                                  by = "codigoUF")


#preparando dados com as siglas
dados_mapa_psic_pop13 <- left_join(dados_mapa_psic_pop13, var_extras_UF,
                                   by = c("codigoUF"))

dados_mapa_psic_pop22<- left_join(dados_mapa_psic_pop22, var_extras_UF,
                                  by = c("codigoUF"))


# joins com base brasil coordenadas
dados_mapa_psic_pop13 <- brasil %>% 
  left_join(dados_mapa_psic_pop13, by = c("abbrev_state"="Sigla"))

dados_mapa_psic_pop22 <- brasil %>% 
  left_join(dados_mapa_psic_pop22, by = c("abbrev_state"="Sigla"))


mapa_psic_pop13 <- ggplot() + 
  geom_sf(data = dados_mapa_psic_pop13, aes(fill = (N.obitos/pop13)*10000),
          color = "#788881")+
  scale_fill_gradient(low = "white", high = "red", limits = c(0,2),
                      name="Obitos (por 10000)") +
  labs(title="Obitos por Psicoativos em 2013 por 10000 habitantes") +
  theme_void() + theme(title = element_text(size = 15))

mapa_psic_pop13


mapa_psic_pop22 <- ggplot() + 
  geom_sf(data = dados_mapa_psic_pop22, aes(fill = (N.obitos/pop22)*10000),
          color = "#788881")+
  scale_fill_gradient(low = "white", high = "red", , limits = c(0,2),
                      name=TeX("Obitos (por 10000)")) +
  labs(title="Obitos por Psicoativos em 2022 por 10000 habitantes", size=15) +
  theme_void() + theme(title = element_text(size = 15))

mapa_psic_pop22

