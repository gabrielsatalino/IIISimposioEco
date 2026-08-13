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
         s_nat_rich_covar, s_inv_rich, s_inv_rel_abund, yrs_with_intro))|>
  mutate(
    invasive_pa = if_else(s_inv_rich == 0, "Nativa", "Não-nativa"),
    invasive_pa = factor(
      invasive_pa,
      levels = c("Nativa", "Não-nativa")
    ),

    CVc   = exp(log_CVc),
    Delta = exp(log_Delta),
    CVe   = exp(log_CVe),
    Psi   = exp(log_Psi),
    omega = exp(log_omega)
  )

# Primeira pergunta: Estabilidade total em comunidades nativas vs não-nativas
#Boxplot simples
dados |>
  ggplot(aes(x = invasive_pa, y = CVc, fill = invasive_pa)) +
  geom_boxplot() +
  labs(title = "Estabilidade total em comunidades nativas vs não-nativas")


#Violin Chart - CVc
dados %>%
  ggplot(aes(x = invasive_pa, y = CVc, fill = invasive_pa, shape = invasive_pa)) +


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
    y = "CVc"
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


#Violin Chart - CVe
dados %>%
  ggplot(aes(x = invasive_pa, y = CVe, fill = invasive_pa, shape = invasive_pa)) +


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
    y = "CVe"
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

# ============================================================
# PRÓXIMAS ETAPAS:

# Objetivo:
# Avaliar quais variaveis estão associadas as modificações da estabilidade total 
# e de seus componentes em comunidades nativas e não-nativas.

# Foque em decidir:
# (i) qual hipótese cada modelo vai representar
# (ii) quais variáveis devem (fazendo sentido de estar) em cada modelo;
# (iii) como comparar e interpretar os modelos.

# Antes de montar os seus novos modelos, cheque:
# - Qual é a variável-resposta principal?
# - Há medidas repetidas no tempo para um mesmo sítio?
# - Há agrupamento de sítios dentro de bacias?

# Essas decisões vão te ajudar a escolher a família de distribuição, a possível
# inclusão de efeitos aleatórios e a independência das observações.


# Os preditores não devem entrar todos juntos automaticamente.
# Primeiro, organizar variáveis que respondem a mecanismos ecológicos
# diferentes e propor modelos candidatos com base nesses mecanismos.
#
# Por exemplo:
#
# Intensidade da invasão:
# - ano de primeira introdução;
# - yrs_with_intro;
# - s_inv_rich;
# - s_inv_rel_abund.
#
# Contexto da comunidade:
# - s_nat_rich_covar.
#
# Estrutura e conectividade:
# - betweenness;
# - closeness;
# - degree;
# - b_spat_wc_mean;
# - modularidade.
#

# Minha sugestão é começar com modelos que respondem perguntas diferente por exemplo:
# Modelo de histórico de invasão.
# Modelo de composição da invasão.
# Modelo de conectividade
# Modelo que combine invasão + conectividade.


# Acho que a princípio da pra evitar inicialmente um modelo global com todos os preditores. 
# Você precisa entender a correlação entre essas variaveis - ou seja,
# avaliar se as variáveis de um mesmo grupo expressam processos diferentes
# ou se são medidas alternativas de um mesmo processo.

# Antes de ajustar modelos:
# Examinar correlações entre todas as variáveis contínuas.
# Verificar especialmente possíveis correlações entre yrs_with_intro, ano de primeira introdução, s_inv_rich e s_inv_rel_abund.
# Verificar também a relação entre as métricas de centralidade da rede.
#
# Se duas variáveis forem fortemente correlacionadas:
#  Escolha apenas uma com base na hipótese ecológica;
#  Monte modelos alternativos, cada um contendo uma das variáveis para avaliar;


# Você precisa pensar que interações só devem ser inseridas somente quando há o efeito de um preditor como dependente de outro.
# Além disso pensa se  o agrupamento dos sítios por bacia precisa ser incorporado
# ao modelo, como efeito aleatório ou efeito fixo.


#
# Também considerar se a dependência espacial entre sítios pode violar
# a independência dos resíduos.

#Modelos teste - CVc


#Mod1 - Histórico de invasão

mod1CVc <- lmer(
  log_CVc ~ yrs_with_intro + (1 | HYBAS_ID),
  data = dados
)

summary(mod1CVc)

#Mod2.1 - Composição da invasão

mod2.1CVc <- lmer(
  log_CVc ~ s_inv_rich + s_inv_rel_abund  + (1 | HYBAS_ID),
  data = dados
)

summary(mod2.1CVc)

#Mod3.1 - Invasão total 

mod3.1CVc <- lmer(
  log_CVc ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + (1 | HYBAS_ID),
  data = dados
)

summary(mod3.1CVc)


#Mod3.1 - Conectividade 

mod3.1CVc <- lmer(
  log_CVc ~ s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod3.1CVc)


#Mod4 - Invasão + Conectividade 

mod4CVc <- lmer(
  log_CVc ~ s_inv_rich + s_spat_btw + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod4CVc)

#Modelo Global

mod_globalCVc <- lmer(
  log_CVc ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod_globalCVc)

#Obs Mods teste CVc: Problemas com as escalas das variáveis preditoras de conectividade. 
#Efeito positivo da riqueza de não-nativas sobre a estabilidade total em todos os modelos (forte em alguns, sutil em outros).
#Efeito individual positivo forte do tempo de invasão, mas colinearidade detectada com riqueza de não nativas.

###########################################################################################################

#Modelos teste - CVe


#Mod1 - Histórico de invasão

mod1CVe <- lmer(
  log_CVe ~ yrs_with_intro + (1 | HYBAS_ID),
  data = dados
)

summary(mod1CVe)

#Mod2.1 - Composição da invasão 

mod2.1CVe <- lmer(
  log_CVe~ s_inv_rich + s_inv_rel_abund  + (1 | HYBAS_ID),
  data = dados
)

summary(mod2.1CVe)


#Mod3.1 - Invasão total 

mod3.1CVe <- lmer(
  log_CVe ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + (1 | HYBAS_ID),
  data = dados
)

summary(mod3.1CVe)


#Mod3.1 - Conectividade 

mod3.1CVe <- lmer(
  log_CVe ~ s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod3.1CVe)


#Mod4 - Invasão + Conectividade
mod4CVe <- lmer(
  log_CVe ~ s_inv_rich + s_spat_btw + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod4CVe)

#Modelo Global

mod_globalCVe <- lmer(
  log_CVe ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod_globalCVe)

#Obs Mods teste CVc: Problemas com as escalas das variáveis preditoras de conectividade. 
#Riqueza de não-nativas e anos de invasão tiveram efeitos positivos fortes em modelos diferentes. Riqueza teve efeito positivo forte em mais modelos.
############################################################################################################################

#Modelos teste - Delta


#Mod1 - Histórico de invasão

mod1Del <- lmer(
  log_Delta ~ yrs_with_intro + (1 | HYBAS_ID),
  data = dados
)

summary(mod1Del)

#Mod2.1 - Composição da invasão 

mod2.1Del <- lmer(
  log_Delta ~ s_inv_rich + s_inv_rel_abund  + (1 | HYBAS_ID),
  data = dados
)

summary(mod2.1Del)


#Mod3.1 - Invasão total 

mod3.1Del <- lmer(
  log_Delta ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + (1 | HYBAS_ID),
  data = dados
)

summary(mod3.1Del)


#Mod3.1 - Conectividade 

mod3.1Del <- lmer(
  log_Delta ~ s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod3.1Del)


#Mod4 - Invasão + Conectividade
mod4Del <- lmer(
  log_Delta ~ s_inv_rich + s_spat_btw + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod4Del)

#Modelo Global

mod_globalDel <- lmer(
  log_Delta ~ yrs_with_intro + s_inv_rich + s_inv_rel_abund + s_spat_btw + b_spat_wc_mean + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod_globalDel)

#Obs Mods teste Delta: Problemas com as escalas das variáveis preditoras de conectividade. 
#Modelo invasão total: Efeito negativo forte da riqueza e efeito negativo forte do tempo de invasão.
#Efeito positivo da riqueza de não-nativas em quase todos os modelos.


