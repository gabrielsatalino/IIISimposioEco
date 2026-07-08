# Script: III Simposio Internacional de Ecologia PPG UFSCar
# Como o aumento da diversidade de espécies afeta a estabilidade de comunidades com presença 
# de espécies não nativas em ecossistemas de água doce? 
# "Subpergunta:" Essa relação se modifica com a abundância relativa de espécies não nativas 
# e com o tempo de invasão?


# Por via de regra, o aumento da diversidade tende a gerar maior estabilidade em comunidades.
# No entanto, a presença de espécies não nativas pode alterar essa relação, 
# uma vez que espécies não nativas podem ser mais competitivas e alterar a estrutura da comunidade.

library(tidyverse)
library(lme4)
library(AICcmodavg)
library(here)

dados = read_csv(here("data_for_collaborators.csv"))
print(head(dados))

dados = dados |>
  select(c(HYBAS_ID, log_CVc, log_Delta, log_CVe, log_Psi, log_omega,
         s_nat_rich_covar, s_inv_rich, s_inv_rel_abund, yrs_with_intro)) |>
mutate(invasive_pa = ifelse(s_inv_rich == 0, 0, 1),
invasive_pa = factor(invasive_pa, levels = c(0, 1), labels = c("Nativa", "Não-nativa")))

# Primeira pergunta: Estabilidade total em comunidades nativas vs não-nativas

dados |>
  ggplot(aes(x = invasive_pa, y = log_CVc, fill = invasive_pa)) +
  geom_boxplot() +
  labs(title = "Estabilidade total em comunidades nativas vs não-nativas")

# GABRIEL: Deixar esse grafico mais bonito, com cores diferentes (padrão de cores, mais limpo e com tema minimalista)

# Modelo: Estabilidade total vs. abundância relativa de não-nativas

mod_pergunta <- lm(log_CVc ~ invasive_pa * s_nat_rich_covar, data = dados)
summary(mod_pergunta)

# GABRIEL: testar os diagnosticos do modelo e escrever uma explicação sobre o resultado do modelo, se a relação entre estabilidade
# e diversidade é diferente para comunidades nativas e não-nativas.

# Pergunta 2: Essa relação se modifica com a abundância relativa de espécies não nativas  e com o tempo de invasão?

mod_pergunta2 <- lm(log_CVc ~ s_nat_rich_covar *s_inv_rel_abund * yrs_with_intro, data = dados)
summary(mod_pergunta2)

# GABRIEL: testar os diagnosticos do modelo e escrever uma explicação sobre o resultado do modelo, se a relação entre estabilidade
# e diversidade é diferente para comunidades nativas e não-nativas.


# Pense em como colocar isso em gráficos, talvez com facetas,
# para mostrar a relação entre estabilidade e diversidade em comunidades nativas e não-nativas, 
# considerando a abundância relativa de espécies não nativas e o tempo de invasão.