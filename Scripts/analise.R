## SCRIPT PARA ANÁLISE


# pacotes -----------------------------------------------------------------

library(tidyverse)

library(lubridate)

library(readxl)

library(runner)


# Pegando info ------------------------------------------------------------

## Aqui vou puxar a planilha já tratada, na pasta "rdata", para não ficar baixando do Google Drive.

obras <- readRDS("rdata/relacao_termos.rds")



summary(obras)




# Valor obras e tempo -----------------------------------------------------


obras %>% mutate(ano = year(data_tc)) %>%
  mutate(ano = as_factor(ano)) %>% 
  group_by(ano, data_tc) %>% 
  summarise(sum = sum(valor_planilha)) %>% 
  ggplot(aes(x = data_tc, y = sum)) +
  geom_line(aes(colour = ano))


### Aparentemente há um movimento sazonal nas obras, para ver vou plotar todas as obras com dia e mês
### e usar o ano para distinguir


##  Data termo x ano

obras %>% mutate(dia_do_ano = yday(data_tc),  ## transformar todas as datas no dia do ano
                 ano = year(data_tc)) %>%
  mutate(ano = as_factor(ano)) %>%  ##Fatorizando ano
  filter(!is.na(ano) & ano != 2010) %>% 
  group_by(ano, dia_do_ano) %>% 
  summarise(valor = sum(valor_planilha)) %>%  ## Esse valor será a soma das planilhas do termo celebrado naquele dia
  ggplot(aes(x = dia_do_ano, y = valor)) +
  geom_line(aes(colour = ano)) +
  facet_wrap(~ ano)

# Há picos interessantes no final do ano de 2016 e 2017, melhor analisar com valor cumulativo


## Data termo ano - fazer valor cumulativo

obras %>% mutate(dia_do_ano = yday(data_tc),  ## transformar todas as datas no dia do ano
                 ano = year(data_tc)) %>%
  mutate(ano = as_factor(ano)) %>%  ##Fatorizando ano
  filter(!is.na(data_tc) & (ano != 2010)) %>% 
  group_by(ano, dia_do_ano) %>% 
  summarise(valor = sum(valor_planilha, na.rm = TRUE)) %>% 
  arrange(ano, dia_do_ano) %>%
  mutate(cumulativo = cumsum(valor)) %>% 
  ggplot(aes(x = dia_do_ano, y = cumulativo)) +
  geom_line(aes(colour = ano), size = 1.3) +
  facet_wrap(~ ano) +
  labs(title = "Valor de termos celebrados, por ano",
       caption = "O eixo y representa o valor cumulativo dos termos celebrados, o eixo x é o dia do ano") +
  theme_bw() +
  ylab("Valor cumulativo") +
  xlab("Dia do ano") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

ggsave("figuras/Termos celebrados ao longo dos ano.png", last_plot())



# Liberação x tempo -------------------------------------------------------

## Primeiro há de se descobrir quanto de informação temos sobre as liberações.


summary(obras$liber_financ) ## 12005 NA's

liberacao_financeira <- obras %>% filter(!is.na(liber_financ)) ## Nos resta uma amostra então de 2937


