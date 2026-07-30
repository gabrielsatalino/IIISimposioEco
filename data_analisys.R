# Script: III Simposio Internacional de Ecologia PPG UFSCar
# Como o aumento da diversidade de espécies afeta a estabilidade de comunidades com presença 
# de espécies não nativas em ecossistemas de água doce? 
# "Subpergunta:" Essa relação se modifica com a abundância relativa de espécies não nativas 
# e com o tempo de invasão?


# Por via de regra, o aumento da diversidade tende a gerar maior estabilidade em comunidades.
# No entanto, a presença de espécies não nativas pode alterar essa relação, 
# uma vez que espécies não nativas podem ser mais competitivas e alterar a estrutura da comunidade.

library(tidyverse)
library(lmerTest)
library(AICcmodavg)
library(here)
library(treemap)
library(treemapify)



dados = read_csv(here("data_for_collaborators.csv"))
print(head(dados))

dados = dados |>
  select(c(HYBAS_ID, log_CVc, log_Delta, log_CVe, log_Psi, log_omega,
         s_nat_rich_covar, s_inv_rich, s_inv_rel_abund, yrs_with_intro)) |>
mutate(invasive_pa = ifelse(s_inv_rich == 0, 0, 1),
invasive_pa = factor(invasive_pa, levels = c(0, 1), labels = c("Nativa", "Não-nativa")))
# Classificando os sítios como com presença ou não de espécies não-nativa


# Primeira pergunta: Estabilidade total em comunidades nativas vs não-nativas

#Boxplot simples
dados |>
  ggplot(aes(x = invasive_pa, y = log_CVc, fill = invasive_pa)) +
  geom_boxplot() +
  labs(title = "Estabilidade total em comunidades nativas vs não-nativas")


#Violin Chart
dados %>%

  ggplot(aes(x = invasive_pa, y = log_CVc, fill = invasive_pa, shape = invasive_pa)) +
 

 geom_hline(yintercept = 0, linetype = "dashed", color = "#050404", 
 linewidth = 0.8) +

  ggdist::stat_halfeye(
    adjust = .5, width = .3, show.legend = FALSE,
    .width = 0, justification = -.3, alpha = .6,
    point_colour = NA
  ) + 

  ggbeeswarm::geom_quasirandom(  # Troquei o gghalves por ggbeeswarm pra resolver seu problema ;)
    width = .12,
    alpha = .5,
    size = 1.8,
    show.legend = FALSE
  ) +

  geom_boxplot(
    width = .1, outlier.shape = NA,
    alpha = .75, show.legend = FALSE
  ) +

  labs(
    x = "",
    y = "log(CVc)"
  ) +

  scale_fill_manual(
    '',
    values = rev(wesanderson::wes_palette(n = 2, name = "GrandBudapest1")),
    labels = c("Nativa", "Não-nativa")
  ) +

  scale_shape_manual(
    '',
    values = c(21, 23),
    labels = c("Nativa", "Não-nativa")
  ) +

  scale_color_manual(
    '',
    values = rev(wesanderson::wes_palette(n = 2, name = "GrandBudapest1")),
    labels = c("Nativa", "Não-nativa")
  ) +

  theme_classic() +
  theme(
    axis.text.y = element_text(color = "black", size = 26),
    axis.text.x = element_text(color = "black", size = 30),
    axis.title.y = element_text(size = 24, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    panel.border = element_rect(linewidth = 2, colour = "black", fill = NA)
  )

# Modelo: Estabilidade total em comunidades nativas vs não-nativas

mod_pergunta <- lmer(log_CVc ~ invasive_pa + (1 | HYBAS_ID), data = dados)
summary(mod_pergunta)

# GABRIEL: testar os diagnosticos do modelo e escrever uma explicação sobre o resultado do modelo, se a relação entre estabilidade
# e diversidade é diferente para comunidades nativas e não-nativas.

componentes_long = dados |>
  filter(!is.na(invasive_pa)) |>
  pivot_longer(
    cols = c(log_Delta, log_CVe, log_Psi, log_omega),
    names_to = "componente",
    values_to = "valor"
  ) |>
  mutate(
    componente = recode(
      componente,
      log_Delta = "Delta",
      log_CVe = "CVe",
      log_Psi = "Psi",
      log_omega = "Omega"
    )
  )


nativa <- componentes_long |>
  filter(invasive_pa == "Nativa") |>
  group_by(componente) |>
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
  mutate(magnitude = abs(valor))

nao_native <- componentes_long |>
  filter(invasive_pa == "Não-nativa") |>
  group_by(componente) |>
  summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
  mutate(magnitude = abs(valor))

# Nativa
treemap(
  nativa,
  index = "componente",
  vSize = "magnitude",
  vColor = "valor",
  type = "value",
  palette = "RdBu",
  title = "",
  title.legend = "",
  fontsize.labels = 18,
  fontcolor.labels = "white",
  fontface.labels = c("bold"),
  border.col = "white",
  border.lwds = c(2, 2),
  align.labels = list(
    c("center", "center"),
    c("center", "center")
  ),
  bg.labels = "transparent",
  position.legend = "none"
)

# treemapify()
p_nativa =
ggplot(
  nativa,
  aes(
    area  = magnitude,
    fill  = valor,
    label = componente
  )
) +
  geom_treemap(color = "white", size = 10) +
  geom_treemap_text(
    colour = "white",
    size   = 1.2,
    fontface = "bold",
    place  = "centre",
    grow   = TRUE
  ) +
  scale_fill_distiller(palette = "RdBu") +
  labs(
    title = "Comunidades nativas",
    fill  = "Value"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 30
    ),
    legend.position = "none"
  )


# Não-nativa
treemap(nao_native,
  index = "componente",
  vSize = "magnitude",
  vColor = "valor",
  type = "value",
  palette = "RdBu",
  title = "",
  title.legend = "",
  fontsize.labels = 18,
  fontcolor.labels = "white",
  fontface.labels = c("bold"),
  border.col = "white",
  border.lwds = c(2, 2),
  align.labels = list(
    c("center", "center"),
    c("center", "center")
  ),
  bg.labels = "transparent",
  position.legend = "none"
)

# treemapify()
p_naonativa =
ggplot(
  nao_native,
  aes(
    area  = magnitude,
    fill  = valor,
    label = componente
  )
) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(
    colour = "white",
    size   = 1.2,
    fontface = "bold",
    place  = "centre",
    grow   = TRUE
  ) +
  scale_fill_distiller(palette = "RdBu") +
  labs(
    title = "Comunidades não nativas",
    fill  = "Value"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 30
    ),
    legend.position = "none"
  )


p_nativa + p_naonativa

# Testar os modelos de cada componente - e avaliar os resultadfos

m_delta <- lmer(
  log_Delta ~ invasive_pa + (1 | HYBAS_ID),
  data = dados
)
summary(m_delta)

m_cve <- lmer(
  log_CVe ~ invasive_pa + (1 | HYBAS_ID),
  data = dados
)
summary(m_cve)

m_psi <- lmer(
  log_Psi ~ invasive_pa + (1 | HYBAS_ID),
  data = dados
)
summary(m_psi)

m_omega <- lmer(
  log_omega ~ invasive_pa + (1 | HYBAS_ID),
  data = dados
)
summary(m_omega)

p_vals <- c(
  Delta = coef(summary(m_delta))["invasive_paNão-nativa", "Pr(>|t|)"],
  CVe   = coef(summary(m_cve))["invasive_paNão-nativa", "Pr(>|t|)"],
  Psi   = coef(summary(m_psi))["invasive_paNão-nativa", "Pr(>|t|)"],
  Omega = coef(summary(m_omega))["invasive_paNão-nativa", "Pr(>|t|)"]
)

p.adjust(p_vals, method = "bonferroni") # pensar em outros métodos de ajuste
