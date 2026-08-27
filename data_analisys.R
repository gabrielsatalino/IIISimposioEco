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


dados = read_csv(here("data_for_collaborators.csv"))
print(head(dados))

dados = dados |>
  mutate(
    CVc   = exp(log_CVc),
    Delta = exp(log_Delta),
    CVe   = exp(log_CVe),
    Psi   = exp(log_Psi),
    omega = exp(log_omega)
  )

glimpse(dados)

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


# Também considerar se a dependência espacial entre sítios pode violar
# a independência dos resíduos.


###########################################################################################################
# Modelos teste - CVc

# Anotações importantes: REML é frequentemente usado para estimar os componentes de variância.
# Nesse caso, a variância entre bacias e a variância residual
# Quando comparar modelos com efeitos fixos diferentes usando AIC você precisa usar os modelos candidatos com REML = FALSE
# Depois, com o modelo final decidido, você volta a reajustar com REML = TRUE.

# Modelo 1 - Histórico de invasão

mod1CVc <- lmer(
  log_CVc ~ yrs_with_intro + (1 | HYBAS_ID),
  data = dados
)

summary(mod1CVc)

# Esse modelo parace funcionar, mas a gente precisa prestar atenção a variação das bacias, talvez se colocar alguma variavel espacial melhore.
# Vou tentar centralizar a variável pra ver se estabiliza um pouco melhor.

dados$yrs_with_intro_c <- scale(
  dados$yrs_with_intro,
  center = TRUE,
  scale = FALSE
)

mod_nulo <- lmer(
  log_CVc ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
) # Fazendo pra você entender a comparação de como funciona a inserção da variável temporal

mod1CVc_c <- lmer(
  log_CVc ~ yrs_with_intro_c + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)
summary(mod1CVc_c)

anova(mod_nulo, mod1CVc_c) # Função básica pra comparar modelos

# Modelo 2.1 - Composição da invasão
dados <- dados |>
  dplyr::mutate(
    s_inv_rich_z = as.numeric(scale(s_inv_rich)),
    s_inv_rel_abund_z = as.numeric(scale(s_inv_rel_abund)),
    HYBAS_ID = factor(HYBAS_ID)
  )

mod2.1CVc <- lmer(
  log_CVc ~ s_inv_rich_c + s_inv_rel_abund_c  + (1 | HYBAS_ID),
  data = dados
)

summary(mod2.1CVc)

mod2.1CVc_int = lmer(
  log_CVc ~ s_inv_rich_c * s_inv_rel_abund_c + (1 | HYBAS_ID),
  data = dados
)
summary(mod2.1CVc_int)
# Testar intenração de riqueza de não-nativas e abundância relativa
# Não tem interação, daí segue com esse modelo que você fez!


#Modelo 3.1 - Invasão total 

mod3.1CVc <- lmer(
  log_CVc ~ yrs_with_intro_c + s_inv_rich_c + s_inv_rel_abund_c + (1 | HYBAS_ID),
  data = dados
)

summary(mod3.1CVc)

mod3.1CVc_int = lmer(
  log_CVc ~ s_inv_rich_c * s_inv_rel_abund_c * yrs_with_intro_c+ (1 | HYBAS_ID),
  data = dados
)
summary(mod3.1CVc_int)
# Testar intenração de riqueza de não-nativas e abundância relativa
# Sem interação, daí segue com esse modelo que você fez!

#Modelo 4.1 - Conectividade
# Como as variaveis estao em diferentes escalas, precisamos de uma transformação
# para que o modelo seja estimado corretamente.

dados <- dados |>
  dplyr::mutate(
    s_spat_btw_z = as.numeric(scale(s_spat_btw_c)),
    b_spat_wc_mean_z = as.numeric(scale(b_spat_wc_mean_c)),
    HYBAS_ID = factor(HYBAS_ID)
  )

mod4.1CVc <- lmer(
  log_CVc ~ s_spat_btw_z + b_spat_wc_mean_z + (1 | HYBAS_ID),
  data = dados,
)
summary(mod4.1CVc)

#Modelo 5 - Invasão + Conectividade 

mod4CVc <- lmer(
  log_CVc ~ s_inv_rich_z * s_spat_btw_z + (1 | HYBAS_ID),
  data = dados,
) # Foca mais nesse modelo de interação

summary(mod4CVc)

# Modelo Global
dados <- dados |>
  dplyr::mutate(
    HYBAS_ID = factor(HYBAS_ID),
    yrs_with_intro_z = as.numeric(scale(yrs_with_intro)))


mod_globalCVc <- lmer(
  log_CVc ~ yrs_with_intro_z + s_inv_rich_z * s_spat_btw_z + (1 | HYBAS_ID),
  data = dados,
)

summary(mod_globalCVc)

#Efeito positivo da riqueza de não-nativas sobre a estabilidade total em todos os modelos (forte em alguns, sutil em outros).
#Efeito individual positivo forte do tempo de invasão, mas colinearidade detectada com riqueza de não nativas.

###########################################################################################################

#Modelos teste - CVe

#Mod1 - Histórico de invasão

mod1CVe <- lmer(
  log_CVe ~ yrs_with_intro_z + (1 | HYBAS_ID),
  data = dados
)

summary(mod1CVe)

#Mod2.1 - Composição da invasão 

mod2.1CVe <- lmer(
  log_CVe ~ s_inv_rich_z * s_inv_rel_abund_z  + (1 | HYBAS_ID),
  data = dados
) 
# Pensa nesse modelo de interação, o p é marginal, mas de qualquer forma é um modelo interessante.

summary(mod2.1CVe)


#Mod3.1 - Invasão total 

mod3.1CVe <- lmer(
  log_CVe ~ yrs_with_intro_z + s_inv_rich_z + (1 | HYBAS_ID),
  data = dados_CVe_comp,
  REML = FALSE
) # Acho que esse modelo é o mais interessante pra Cve até agora.

summary(mod3.1CVe)


#Mod3.1 - Conectividade 

mod3.1CVe <- lmer(
  log_CVe ~ s_spat_btw_z + b_spat_wc_mean_z + (1 | HYBAS_ID),
  data = dados,
  control = lmerControl(autoscale = TRUE)
)

summary(mod3.1CVe)


#Mod4 - Invasão + Conectividade
mod4CVe <- lmer(
  log_CVe ~ s_inv_rich_z + s_spat_btw_z + (1 | HYBAS_ID),
  data = dados
)

summary(mod4CVe)

#Modelo Global

mod_globalCVe <- lmer(
  log_CVe ~ yrs_with_intro_z + s_inv_rich_z + s_spat_btw_z + (1 | HYBAS_ID),
  data = dados_CVe_comp,
  REML = FALSE
) 


summary(mod_globalCVe)

anova(mod3.1CVe, mod_globalCVe) 
# Acho que discutimos essa comparação com o Tadeu na sexta

# Uma variavel tem NA, dai precisamos filtrar os dados completmos antes de fazer o modelo comparável

dados_CVe_comp <- dados |>
  dplyr::filter(
    complete.cases(
      log_CVe,
      yrs_with_intro_z,
      s_inv_rich_z,
      s_spat_btw_z,
      HYBAS_ID
    )
  )

#Riqueza de não-nativas e anos de invasão tiveram efeitos positivos fortes em modelos diferentes. Riqueza teve efeito positivo forte em mais modelos.
############################################################################################################################

#Modelos teste - Delta


#Mod1 - Histórico de invasão

mod1Del <- lmer(
  log_Delta ~ yrs_with_intro_z + (1 | HYBAS_ID),
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

########################################################################################################
#Modelos Finais

# Centralização das variáveis
glimpse(dados)

dados = dados |>
  dplyr::mutate(
    s_spat_btw_z = as.numeric(scale(s_spat_btw)),
    b_spat_wc_mean_z = as.numeric(scale(b_spat_wc_mean)),
    s_inv_rich_z = as.numeric(scale(s_inv_rich)),
    s_inv_rel_abund_z = as.numeric(scale(s_inv_rel_abund)),
   yrs_with_intro_z = as.numeric(scale(yrs_with_intro)),
    HYBAS_ID = factor(HYBAS_ID)
  )

glimpse(dados)

#Paleta de cores (gráficos)

paletawes <- wes_palette(n = 2, name = "GrandBudapest1")

#Modelos preditoras numéricas: 4 modelos para cada var, resposta: nulo, invasão, conectividade e global
#Variabilidade temporal total da comunidade (CVc)#############

#Nulo
mod_nullCVc <- lmer(
  CVc ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_nullCVc)

#Invasão
mod_invCVc <- lmer(
  CVc ~ s_inv_rich_z + s_inv_rel_abund_z + s_nat_rich_covar + yrs_with_intro_z + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_invCVc)

#Conectividade
mod_conectCVc <- lmer(
  CVc ~ b_spat_wc_mean_z + s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_conectCVc)

#Global
mod_globalCVc <- lmer(
  CVc ~ yrs_with_intro_z + s_inv_rich_z + s_inv_rel_abund_z  + b_spat_wc_mean_z +  s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_globalCVc)

#AICC
aictab(c(mod_nullCVc, mod_invCVc, mod_conectCVc, mod_globalCVc))

#O melhor modelo foi o mod_invCVc
library(performance)
library(see)

summary(mod_invCVc)
check_model(mod_invCVc)

r2(mod_invCVc)
check_collinearity(mod_invCVc)
check_singularity(mod_invCVc)
icc(mod_invCVc)

# Refazer o modelo sem REML=FALSE
mod_invCVc <- lmer(
  CVc ~ s_inv_rich_z + s_inv_rel_abund_z + s_nat_rich_covar + yrs_with_intro_z + (1 | HYBAS_ID),
  data = dados,
  REML = TRUE
)
summary(mod_invCVc)

#Gráfico preditora mais explicativa (Sem escala / s_inv_rich)

dados |>
  ggplot(aes(x = s_inv_rich, y = CVc)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Variabilidade temporal da comunidade vs. tempo de introdução") +
  theme_minimal()

#Variabilidade temporal populacional (CVe)##############
#Nulo
mod_nullCVe <- lmer(
  CVe ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_nullCVe)

#Invasão
mod_invCVe <- lmer(
  CVe ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
    data = dados,
    REML = FALSE
)

summary(mod_invCVe)

#Conectividade
mod_conectCVe <- lmer(
  CVe ~ b_spat_wc_mean_z + s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE)

summary(mod_conectCVe)

#Global
mod_globalCVe <- lmer(
  CVe ~ yrs_with_intro_z + s_inv_rich_z + s_inv_rel_abund_z  + b_spat_wc_mean_z +  s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE)

summary(mod_globalCVe)

#AICC
aictab(c(mod_nullCVe, mod_invCVe, mod_conectCVe, mod_globalCVe))

#O melhor modelo foi o mod_invCVe 
# Gabriela: talvez dê pra pensar no modelo global nesse caso.

summary(mod_invCVe)
check_model(mod_invCVe)

r2(mod_invCVe)
check_collinearity(mod_invCVe)
check_singularity(mod_invCVe)
icc(mod_invCVe)

mod_invCVe <- lmer(
  CVe ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
    data = dados,
    REML = TRUE
)

summary(mod_invCVe)

#Gráfico preditora mais explicativa (Sem escala / yrs_with_intro)

dados |>
  ggplot(aes(x = yrs_with_intro, y = CVe)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Variabilidade populacional vs. tempo de introdução") +
  theme_minimal()



#Efeito dominância (Delta)##############
#Nulo
mod_nullDelta <- lmer(
  Delta ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_nullDelta)

#Invasão
mod_invDelta <- lmer(
  Delta ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_invDelta)

#Conectividade
mod_conectDelta <- lmer(
  Delta ~ s_spat_btw_z + b_spat_wc_mean_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_conectDelta)

#Global
mod_globalDelta <- lmer(
Delta ~ yrs_with_intro_z + s_inv_rich_z + s_inv_rel_abund_z  + b_spat_wc_mean_z +  s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
data = dados,
REML = FALSE
)

summary(mod_globalDelta)

#AICC
aictab(c(mod_nullDelta, mod_invDelta, mod_conectDelta, mod_globalDelta))
#O melhor modelo foi o mod_invDelta

summary(mod_invDelta)
check_model(mod_invDelta)

r2(mod_invDelta)
check_collinearity(mod_invDelta)
check_singularity(mod_invDelta)
icc(mod_invDelta)

mod_invDelta <- lmer(
  Delta ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

#Gráfico preditora mais explicativa (1) (Sem escala / yrs_with_intro)

dados |>
  ggplot(aes(x = yrs_with_intro, y = Delta)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Efeito dominância vs. tempo de introdução") +
  theme_minimal()

#Gráfico preditora mais explicativa (2) (Sem escala / s_inv_rich)

dados |>
  ggplot(aes(x = s_inv_rich, y = Delta)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Efeito dominância vs. riqueza de não-nativas") +
  theme_minimal()

#Efeito assincronia (Psi)#################

#Nulo
mod_nullPsi <- lmer(
  Psi ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_nullPsi)

#Invasão
mod_invPsi <- lmer(Psi ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
data = dados,
REML = FALSE)

summary(mod_invPsi)

#Conectividade
mod_conectPsi <- lmer(
  Psi ~ s_spat_btw_z + b_spat_wc_mean_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_conectPsi)

#Global
mod_globalPsi <- lmer(Psi ~ yrs_with_intro_z + s_inv_rich_z + s_inv_rel_abund_z
 + b_spat_wc_mean_z +  s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
 data = dados,
REML = FALSE)

summary(mod_globalPsi)

#AICC
aictab(c(mod_nullPsi, mod_invPsi, mod_conectPsi, mod_globalPsi))

#O melhor modelo foi o mod_nullPsi, mas mod_invPsi teve valor de Delta_AICc = 1.65.
# Gabriela: dá pra usar o de invasão

summary(mod_nullPsi)
performance::r2(mod_nullPsi)

# Gabriela
summary(mod_invPsi)
check_model(mod_invPsi)

r2(mod_invPsi)
check_collinearity(mod_invPsi)
check_singularity(mod_invPsi)
icc(mod_invPsi)

mod_invPsi <- lmer(
  `Psi` ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = TRUE
)

summary(mod_invPsi)

#Problema gráficos Psi: 
#Apesar de mod_invPsi ser o melhor modelo, a variável com o menor valor de p deste modelo é yrs_with_intro, com p = 0.3820

#Gráfico preditora mais explicativa  (Sem escala / yrs_with_intro)

dados |>
  ggplot(aes(x = yrs_with_intro, y = Psi)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Efeito assincronia vs. tempo de introdução") +
  theme_minimal()

#Efeito diversidade (omega)##################
#Nulo
mod_nullomega <- lmer(
  omega ~ 1 + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
)

summary(mod_nullomega)

#Invasão
mod_invomega <- lmer(
  omega ~ s_inv_rich_z + yrs_with_intro_z + s_inv_rel_abund_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = FALSE
  )

summary(mod_invomega)

#Conectividade
mod_conectomega <- lmer(omega ~ s_spat_btw_z + b_spat_wc_mean_z + s_nat_rich_covar + (1 | HYBAS_ID),
data = dados,
REML = FALSE)

summary(mod_conectomega)

#Global
mod_globalomega <- lmer(omega ~ yrs_with_intro_z + s_inv_rich_z + s_inv_rel_abund_z  + b_spat_wc_mean_z +  s_spat_btw_z + s_nat_rich_covar + (1 | HYBAS_ID),
 data = dados,
 REML = FALSE)

summary(mod_globalomega)

#AICC
aictab(c(mod_nullomega, mod_invomega, mod_conectomega, mod_globalomega))

#O melhor modelo foi o mod_invomega (Valor de AICcWt = 1)

summary(mod_invomega)
check_model(mod_invomega)

r2(mod_invomega)
check_collinearity(mod_invomega)
check_singularity(mod_invomega)
icc(mod_invomega)

mod_invomega <- lmer(
  omega ~ s_inv_rel_abund_z + yrs_with_intro_z + s_inv_rich_z + s_nat_rich_covar + (1 | HYBAS_ID),
  data = dados,
  REML = TRUE
)

#Gráfico preditora mais explicativa  (Sem escala / yrs_with_intro)

dados |>
  ggplot(aes(x = yrs_with_intro, y = omega)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = paletawes[2]) +
  labs(title = "Efeito diversidade vs. tempo de introdução") +
  theme_minimal()


summary(mod_invPsi)
