 #codigo pra salvar os graficos

 # Defina o caminho da pasta onde você quer salvar os gráficos
  pasta_destino <- "Psicoativos/Graficos carol"

  # Lista de gráficos e seus nomes de arquivos correspondentes
  graficos <- list(
    boxplot_idade_br = "boxplot_idade_br.png",
    boxplot_idade_br_psic = "boxplot_idade_br_psic.png",
    boxplot_idade_es = "boxplot_idade_es.png",
    boxplot_idade_es_psic = "boxplot_idade_es_psic.png",
    graf_serie_idade_es_t = "graf_serie_idade_es_t.png",
    graf_serie_idade_es_p = "graf_serie_idade_es_p.png",
    graf_serie_idade_br_t = "graf_serie_idade_br_t.png",
    graf_serie_idade_br_p = "graf_serie_idade_br_p.png",
    barplot_estciv_br = "barplot_estciv_br.png",
    barplot_estciv_br_psic = "barplot_estciv_br_psic.png",
    barplot_estciv_es = "barplot_estciv_es.png",
    barplot_estciv_ex_psic = "barplot_estciv_ex_psic.png",
    serie_obt_psic_br = "serie_obt_psic_br.png",
    serie_obt_psic_es = "serie_obt_psic_es.png"
  )

  # Loop para salvar todos os gráficos na pasta especificada
  for (grafico in names(graficos)) {
    caminho_completo <- file.path(pasta_destino, graficos[[grafico]])
    ggsave(caminho_completo, get(grafico), width = 10, height = 6, dpi = 300)
  }






