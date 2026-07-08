# Script: III Simposio Internacional de Ecologia PPG UFSCar
# 

library(tidyverse)
library(lme4)
library(AICcmodavg)
library(here)

dados = read_csv(here("data_for_collaborators.csv"))
print(head(dados))

dados = dados |>
  select(c(HYBAS_ID, log_CVc, log_Delta, log_CVe, log_Psi, log_omega,
         s_nat_rich_covar, s_inv_rich, s_inv_rel_abund, yrs_with_intro))

# 1. Distribuição das respostas (Histograma das métricas de estabilidade)
dados |>
  pivot_longer(starts_with("log_"), names_to = "metrica", values_to = "valor") |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "navy") +
  facet_wrap(~ metrica, scales = "free") +
  labs(title = "Distribuição das métricas de estabilidade")


# 2. Análise - Modelo Linear Simples

#2.1. Estabilidade total vs. Preditores

#Estabilidade total vs. abundância relativa de não-nativas

dados |>
  filter(!is.na(s_inv_rel_abund)) |>
  ggplot(aes(x = s_inv_rel_abund, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. abundância relativa de não-nativas")

#Estabilidade total vs. tempo de invasão

dados |>

  filter(!is.na(yrs_with_intro)) |>
  ggplot(aes(x = yrs_with_intro, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. tempo de invasão")

#Estabilidade total vs riqueza de não-nativas

dados |>
  filter(!is.na(s_inv_rich)) |>
  ggplot(aes(x = s_inv_rich, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. riqueza de não-nativas")

#Estabilidade total vs riqueza de nativas

dados |>
  filter(!is.na(s_nat_rich_covar)) |>
  ggplot(aes(x = s_nat_rich_covar, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. riqueza de nativas")

#Estabilidade total vs. centralidade de betweeness do local

dados |>
  filter(!is.na(s_spat_btw)) |>
  ggplot(aes(x = s_spat_btw, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. centralidade de betweennes do local")

#Estabilidade total vs.Distância média entre locais na bacia

dados |>
  filter(!is.na(b_spat_wc_mean)) |>
  ggplot(aes(x = b_spat_wc_mean, y = log_CVc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Estabilidade total vs. Distância média entre locais na bacia")

#2.2 Efeito dominância vs Preditores

#Log(Delta) vs Abundância relativa de não-nativas

dados |>
  filter(!is.na(s_inv_rel_abund)) |>
  ggplot(aes(x = s_inv_rel_abund, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. abundância relativa de não-nativas")

#Log(Delta) vs. tempo de invasão

dados |>
  filter(!is.na(yrs_with_intro)) |>
  ggplot(aes(x = yrs_with_intro, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. tempo de invasão")

#Log(Delta) vs riqueza de não-nativas

dados |>
  filter(!is.na(s_inv_rich)) |>
  ggplot(aes(x = s_inv_rich, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. riqueza de não-nativas")

#Log(Delta) vs riqueza de nativas

dados |>
  filter(!is.na(s_nat_rich_covar)) |>
  ggplot(aes(x = s_nat_rich_covar, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. riqueza de nativas")

#Log(Delta) vs. centralidade de betweeness do local

dados |>
  filter(!is.na(s_spat_btw)) |>
  ggplot(aes(x = s_spat_btw, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. centralidade de betweennes do local")

#Log(delta) vs.Distância média entre locais na bacia

dados |>
  filter(!is.na(b_spat_wc_mean)) |>
  ggplot(aes(x = b_spat_wc_mean, y = log_Delta)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito dominância vs. Distância média entre locais na bacia")

#2.3 Variabilidade populacional vs Preditores

#Log(Cve) vs Abundância relativa de não-nativas

dados |>
  filter(!is.na(s_inv_rel_abund)) |>
  ggplot(aes(x = s_inv_rel_abund, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. abundância relativa de não-nativas")

#Log(CVe) vs. tempo de invasão

dados |>
  filter(!is.na(yrs_with_intro)) |>
  ggplot(aes(x = yrs_with_intro, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. tempo de invasão")

#Log(CVe) vs riqueza de não-nativas

dados |>
  filter(!is.na(s_inv_rich)) |>
  ggplot(aes(x = s_inv_rich, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. riqueza de não-nativas")

#Log(CVe) vs riqueza de nativas

dados |>
  filter(!is.na(s_nat_rich_covar)) |>
  ggplot(aes(x = s_nat_rich_covar, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. riqueza de nativas")

#Log(CVe) vs. centralidade de betweeness do local

dados |>
  filter(!is.na(s_spat_btw)) |>
  ggplot(aes(x = s_spat_btw, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. centralidade de betweennes do local")

#Log(CVe) vs.Distância média entre locais na bacia

dados |>
  filter(!is.na(b_spat_wc_mean)) |>
  ggplot(aes(x = b_spat_wc_mean, y = log_CVe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Variabilidade populacional vs. Distância média entre locais na bacia")

#2.4. Efeito sincronia vs preditores

#Log(Psi) vs abundância relativa de não-nativas

dados |>
  filter(!is.na(s_inv_rel_abund)) |>
  ggplot(aes(x = s_inv_rel_abund, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito sincronia vs. abundância relativa de não-nativas")

#Log(Psi) vs. tempo de invasão

dados |>
  filter(!is.na(yrs_with_intro)) |>
  ggplot(aes(x = yrs_with_intro, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito sincronia vs. tempo de invasão")

#Log(Psi) vs riqueza de não-nativas

dados |>
  filter(!is.na(s_inv_rich)) |>
  ggplot(aes(x = s_inv_rich, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito Sincronia vs. riqueza de não-nativas")

#Log(Psi) vs riqueza de nativas

dados |>
  filter(!is.na(s_nat_rich_covar)) |>
  ggplot(aes(x = s_nat_rich_covar, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito sicnronia vs. riqueza de nativas")

#Log(Psi) vs. centralidade de betweeness do local

dados |>
  filter(!is.na(s_spat_btw)) |>
  ggplot(aes(x = s_spat_btw, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito sincronia vs. centralidade de betweennes do local")

#Log(Psi) vs.Distância média entre locais na bacia

dados |>
  filter(!is.na(b_spat_wc_mean)) |>
  ggplot(aes(x = b_spat_wc_mean, y = log_Psi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito sincronia vs. Distância média entre locais na bacia")

#2.5 Efeito portifólio vs Preditores

#Log(omega) vs abundância relativa de não-nativas

dados |>
  filter(!is.na(s_inv_rel_abund)) |>
  ggplot(aes(x = s_inv_rel_abund, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. abundância relativa de não-nativas")

#Log(omega) vs. tempo de invasão

dados |>
  filter(!is.na(yrs_with_intro)) |>
  ggplot(aes(x = yrs_with_intro, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. tempo de invasão")

#Log(omega) vs riqueza de não-nativas

dados |>
  filter(!is.na(s_inv_rich)) |>
  ggplot(aes(x = s_inv_rich, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. riqueza de não-nativas")

#Log(omega) vs riqueza de nativas

dados |>
  filter(!is.na(s_nat_rich_covar)) |>
  ggplot(aes(x = s_nat_rich_covar, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. riqueza de nativas")

#Log(omega) vs. centralidade de betweeness do local

dados |>
  filter(!is.na(s_spat_btw)) |>
  ggplot(aes(x = s_spat_btw, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. centralidade de betweennes do local")

#Log(omega) vs.Distância média entre locais na bacia

dados |>
  filter(!is.na(b_spat_wc_mean)) |>
  ggplot(aes(x = b_spat_wc_mean, y = log_omega)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Efeito portifólio vs. Distância média entre locais na bacia")


# 3. Comparação entre locais invadidos e não-invadidos
dados |>
  mutate(invadido = s_inv_rel_abund > 0) |>
  ggplot(aes(x = invadido, y = log_CVc, fill = invadido)) +
  geom_boxplot() +
  labs(title = "Estabilidade total em locais invadidos e não-invadidos")



#Testando modelos

#Simple Linear model

mod0 <- lm(log_CVc ~ yrs_with_intro, data = dados)

coef(mod0)

summary(mod0)


#LMM:

# Modelo nulo (apenas controle e efeito aleatório)
m0 <- lmer(log_CVc ~ s_nat_rich_covar + (1 | HYBAS_ID),
           data = dados)

# Modelo com intensidade da invasão
m1 <- lmer(log_CVc ~ s_inv_rel_abund + s_nat_rich_covar + (1 | HYBAS_ID),
           data = dados)

# Comparação
aictab(list(nulo = m0, intensidade = m1))


