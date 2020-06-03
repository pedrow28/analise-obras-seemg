### Nesse script pretendo analisar delays de tempo, como entre a celebração do TC e 
### a homologação da licitação, a liberação financeira e a ordem de início


# Pacotes e dados ---------------------------------------------------------

library(tidyverse)

library(lubridate)

library(readxl)

obras <- readRDS("rdata/relacao_termos.rds")



# Celebração e homologação ------------------------------------------------

## Quantas obras temos a data de celebração do termo e homologação da obra?


obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>% nrow() ##4328

obras %>% filter((is.na(data_tc)) | (is.na(data_homolog))) %>% pull(planilha) %>% table()

##Existem 1304 termos pagos sem data do tc OU data da homologação


obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
 mutate(delay = data_homolog - data_tc) %>%
  filter(delay < 0) %>% group_by(sre) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% View()

## A existência de datas de homologação prévias à celebração é provavelmente um erro.
## MOC possui o maior número de erros.


## Qual o tempo médio entre a celebração e a homologação, por tipo de obra?

obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% group_by(tipo_obra) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay))


## QUal o tepo médio entre a celebração e homologação, por SRE?

obras %>% filter(!(is.na(data_tc)) & !(is.na(data_homolog))) %>%
  mutate(delay = data_homolog - data_tc) %>%
  filter(delay > 0) %>% group_by(sre) %>% 
  summarise(n_obras = n(),
            media_delay = mean(delay),
            min_delay = min(delay),
            max_delay = max(delay),
            sd_delay = sd(delay)) %>% 
  arrange(desc(media_delay)) %>% View()






