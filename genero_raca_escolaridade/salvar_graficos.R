# Defina o caminho da pasta onde você quer salvar os gráficos
pasta_destino <- "Psicoativos/graficos"

# Lista de gráficos e seus nomes de arquivos correspondentes
graficos <- list(
  hist_escolaridade_br_psic = "hist_escolaridade_br_psic.png",
  hist_escolaridade_br_total = "hist_escolaridade_br_total.png",
  hist_escolaridade_es_psic = "hist_escolaridade_es_psic.png",
  hist_escolaridade_es_total = "hist_escolaridade_es_total.png",
  hist_genero_br_psic = "hist_genero_br_psic.png",
  hist_genero_br_total = "hist_genero_br_total.png",
  hist_genero_es_psic = "hist_genero_es_psic.png",
  hist_genero_es_total = "hist_genero_es_total.png",
  hist_obito_br_psic = "hist_obito_br_psic.png",
  hist_obito_br_totais = "hist_obito_br_totais.png",
  hist_obito_es_psic = "hist_obito_es_psic.png",
  hist_obito_es_totais = "hist_obito_es_totais.png",
  hist_raca_br_psic = "hist_raca_br_psic.png",
  hist_raca_br_total = "hist_raca_br_total.png",
  hist_raca_es_psic = "hist_raca_es_psic.png",
  hist_raca_es_total = "hist_raca_es_total.png",
  series_escolaridade_br_psic = "series_escolaridade_br_psic.png",
  series_escolaridade_br_total = "series_escolaridade_br_total.png",
  series_escolaridade_es_psic = "series_escolaridade_es_psic.png",
  series_escolaridade_es_total = "series_escolaridade_es_total.png",
  series_genero_br_psic = "series_genero_br_psic.png",
  series_genero_br_total = "series_genero_br_total.png",
  series_genero_es_psic = "series_genero_es_psic.png",
  series_genero_es_total = "series_genero_es_total.png",
  series_mortes_br_total = "series_mortes_br_total.png",
  series_mortes_combined = "series_mortes_combined.png",
  series_raca_br_psic = "series_raca_br_psic.png",
  series_raca_br_total = "series_raca_br_total.png",
  series_raca_es_psic = "series_raca_es_psic.png",
  series_raca_es_total = "series_raca_es_total.png"
)

# Loop para salvar todos os gráficos na pasta especificada
for (grafico in names(graficos)) {
  caminho_completo <- file.path(pasta_destino, graficos[[grafico]])
  ggsave(caminho_completo, get(grafico), width = 10, height = 6, dpi = 300)
}
