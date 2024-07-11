
### MAPA DO BRASIL VALORES ABSOLUTOS DE OBITOS POR PSICOS

brasil = read_state(code_state = "all", year = 2018)

dados_mapa_psic_pop <- data.frame(
  dados_br_psic %>%  
    group_by(codigoUF) %>% 
    summarise(N.obitos = n()))

dat.g.map1 <- brasil %>% 
  left_join(dat.map1, by = c("abbrev_state"="Sigla"))


g.map1 <- ggplot() + 
  geom_sf(data = dat.g.map1, aes(fill= N.obitos), color = NA)+
  scale_fill_gradient(low = "white", high = "red",
                      name="Total de Óbitos 
por Psicoativos") +
  labs(title="   Mortalidade por Psicoativos", size=8) +
  theme_void()



### MAPA DO BRASIL OBITOS POR ESTADO POR PSICO/OBITOS POR ESTADO 2013


dados_mapa_psic_obitos_uf <- data.frame(
  dados_br_psic %>% 
    filter(ANOOBITO == "2013") %>% 
    group_by(codigoUF) %>% 
    summarise(N_obitos = n()))


dados_mapa_total_obitos_uf <- data.frame(
  dados_br_total %>%  
    filter(ANOOBITO == "2013") %>%
    group_by(codigoUF) %>% 
    summarise(N_obitos = n()))



dados_mapa_psic_obitos_uf <- left_join(dados_mapa_psic_obitos_uf,
                                 dados_mapa_total_obitos_uf, 
                                 by = "codigoUF")

dados_mapa_psic_obitos_uf <- left_join(dados_mapa_psic_obitos_uf, codigosUF)

dados_mapa_psic_obitos_uf <- brasil %>% 
  left_join(dados_mapa_psic_obitos_uf, by = c("abbrev_state"="Sigla"))


mapa_psic_obitos_uf <- ggplot() + 
  geom_sf(data = dados_mapa_psic_obitos_uf, aes(fill = (N_obitos.x/N_obitos.y)*100),
          color = NA)+
  scale_fill_gradient(low = "white", high = "red",
                      name=TeX("% de Óbitos")) +
  labs(title="Nº de óbitos por Psicoativos/Nºde Óbitos Totais", size=8) +
  theme_void()

mapa_psic_obitos_uf




### MAPA DO BRASIL OBITOS POR ESTADO POR PSICO/POP ESTADO 2013 e 2022


dados_mapa_psic_pop13_22 <- data.frame(
                dados_br_psic %>%  
                filter(ANOOBITO == "2013" | ANOOBITO == "2022") %>% 
                group_by(codigoUF, ANOOBITO) %>% 
                summarise(N.obitos = n()))
              

pop_UF_13 <- pop13a21 %>% 
  filter(ano == "2013") %>% 
  reframe(valor, brasil_e_unidade_da_federacao_codigo, ano)

pop_UF_22 <- dados_br_psic %>%
  filter(ANOOBITO == "2022") %>% 
  group_by(codigoUF) %>% 
  summarise(unique(pop22), ANOOBITO)

pop_UF_13_22 <- left_join(pop_UF_13, pop_UF_22,
                by = c("brasil_e_unidade_da_federacao_codigo" = "codigoUF"))

dados_mapa_psic_pop13_22 <- left_join(dados_mapa_psic_pop13_22,
                                 pop_UF_13_22, 
        by = c("codigoUF" = "brasil_e_unidade_da_federacao_codigo"))

dados_mapa_psic_pop <- left_join(dados_mapa_psic_pop, codigosUF)

dados_mapa_psic_pop <- brasil %>% 
  left_join(dados_mapa_psic_pop, by = c("abbrev_state"="Sigla"))


mapa_psic_pop <- ggplot() + 
  geom_sf(data = dados_mapa_psic_pop, aes(fill = (N.obitos/mediana)*10000),
          color = NA)+
  scale_fill_gradient(low = "white", high = "red",
                      name=TeX("Óbitos (por 10000)")) +
  labs(title="Mortalidade por Psicoativos/População Mediana", size=8) +
  theme_void()

mapa_psic_pop



