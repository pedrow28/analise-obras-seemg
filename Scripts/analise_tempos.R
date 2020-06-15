### Nesse script pretendo analisar delays de tempo, como entre a celebração do TC e 
### a homologação da licitação, a liberação financeira e a ordem de início


# Pacotes e dados ---------------------------------------------------------

library(tidyverse)

library(lubridate)

library(readxl)

obras <- readRDS("rdata/relacao_termos.rds")



# Celebração e homologação ------------------------------------------------

# Quantas obras temos a data de celebração do termo e homologação da obra?


obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>% nrow() ##4328

obras %>% filter((is.na(data_tc)) | (is.na(data_homolog))) %>% pull(planilha) %>% table()

# Existem 1304 termos pagos sem data do tc OU data da homologação


obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
 mutate(delay = data_homolog - data_tc) %>%
  filter(delay < 0) %>% group_by(sre) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% View()

# A existência de datas de homologação prévias à celebração é provavelmente um erro.
# MOC possui o maior número de erros.


# DELAY GERAL -------------------------------------------------------------

obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay))

# Celebração e homologação: TIPO DE OBRA ------------------------------------------------

obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% group_by(tipo_obra) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay))


# Celebração e homologação: SRE ------------------------------------------------

obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% group_by(sre) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay)) %>% View()


# Celebração e homologação: VALOR ------------------------------------------------

breaks = seq(0, 1, by = 0.2) ## Criar sequencia de 0 a 100% de 20 em 20.

faixas <- obras %>% pull(valor_planilha) %>% quantile(probs = breaks, na.rm = TRUE)

valor_fatorizado <- obras %>% 
  filter(!is.na(valor_planilha)) %>% 
  mutate(faixa = case_when(valor_planilha <= faixas[2] ~ "Entre R$ 536,42 e R$ 7.306,47",
                           valor_planilha > faixas[2] & valor_planilha <= faixas[3] ~ "Entre R$ 7.306,47 e R$ 50.000,00",
                           valor_planilha > faixas[3] & valor_planilha <= faixas[4] ~ "Entre R$ 50.000,00 e R$ 108.181,38",
                           valor_planilha > faixas[4] & valor_planilha <= faixas[5] ~ "Entre R$ 108.181,38 e R$ 245.206,66",
                           valor_planilha > faixas[5] & valor_planilha <= faixas[6] ~ "Entre R$ 245.206,66 e R$ 1.953.838,06",
                           valor_planilha > faixas[6] ~ "Maior que R$ 1.953.838,06"))

## Lembrando que tais categorias foram criadas visando agrupar aproximadamente 20% do número de obras em cada.

## Conferindo:

valor_fatorizado %>% group_by(faixa) %>% summarise(n = n()) %>% 
  mutate(prct = n / nrow(valor_fatorizado))

## Abaixo o cálculo

valor_fatorizado %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% group_by(faixa) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay))




