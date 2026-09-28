#Teste efeito dominância 
#(delta>1, efeito de espécies oportunistas com grandes surtos temporais 
#ou espécies raras/ocasionais?)

library(dplyr)
library(tidyr)

raw_data <- read.csv("dados_comvar_fish_inv_semfiltar.csv")

#critério de Dajos(1973)

# Supondo que raw_data tenha colunas: SiteID, Species, (e talvez SampleID/Date)
# Ajuste os nomes conforme seu dataset

# 1) Número total de amostras por comunidade
#    (assumindo que cada linha é uma ocorrência; se houver réplicas, 
#     precisamos contar amostras únicas)
total_amostras <- raw_data %>%
  group_by(SiteID) %>%
  summarise(n_total = n_distinct(Year))  # troque SampleID pelo nome da sua coluna de amostra

# 2) Número de amostras em que cada espécie aparece, por comunidade
freq_esp <- raw_data %>%
  group_by(SiteID, Species) %>%
  summarise(n_ocorr = n_distinct(Year), .groups = "drop")

# 3) Juntar e calcular a frequência relativa (%)
dajoz <- freq_esp %>%
  left_join(total_amostras, by = "SiteID") %>%
  mutate(
    freq_pct = (n_ocorr / n_total) * 100,
    categoria = if_else(freq_pct >= 50, "Comum", "Rara")
  )

# 4) Lista de espécies raras por comunidade
raras <- dajoz %>%
  filter(categoria == "Rara") %>%
  select(SiteID, Species, n_ocorr, n_total, freq_pct) %>%
  arrange(SiteID, freq_pct)

print(raras)

###############################################################################

#Teste de especulação: 
#Alteração no rank abundância (Dominância) com a invasão

rank_abund <- raw_data %>%
  group_by(SiteID, Species,Status) %>%
  summarise(abund_total = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
  group_by(SiteID) %>%
  mutate(
    abund_rel = abund_total / sum(abund_total),   # abundância relativa (opcional)
    rank = row_number(desc(abund_total))          # 1 = espécie mais abundante
  ) %>%
  arrange(SiteID, rank) %>%
  ungroup()



