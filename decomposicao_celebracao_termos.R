## Esse script foi motivado por questões relacionadas à celebração de termos de compromisso, a partir do 
## "analise.R".

# pacotes -----------------------------------------------------------------

library(tidyverse)

library(lubridate)

library(readxl)



# Pegando info ------------------------------------------------------------

## Aqui vou puxar a planilha já tratada, na pasta "rdata", para não ficar baixando do Google Drive.

obras <- readRDS("rdata/relacao_termos.rds")



# Regressão linear  -------------------------------------------------------

## 1 - Quão significativo é o fator "final do ano" na celebração de termos? 
## Interessante rodar uma regressão linear entre o dia do ano e o valor celebrado.



dias_termos <- obras %>% mutate(dia_do_ano = yday(data_tc),
                                ano = year(data_tc)) %>%   ## transformar todas as datas no dia do ano
                                mutate(ano = as_factor(ano)) %>%  ##Fatorizando ano
                                filter(!is.na(data_tc) & (ano != 2010)) %>% 
  group_by(ano, dia_do_ano) %>% 
  summarise(valor = sum(valor_planilha, na.rm = TRUE))


lm_diasxtermos <- lm(valor ~ dia_do_ano, data = dias_termos)

## Valor p bastante baixo, mas R² muito pouco relevante.

## Não ta dando em grandes coisas isso aqui. Talvez seja necessário fazer um corte nos últimos dias do ano.

## Faremos, mas como definir o corte?

ggplot(dias_termos, aes(x = dia_do_ano, y = valor)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 366, 5)) + coord_flip()

## Colocarei como corte 338


diasfinais_termos <- dias_termos %>% filter(dia_do_ano >= 338)

diasrestante_termos <- dias_termos %>% filter(dia_do_ano < 338)

ggplot(diasfinais_termos, aes(x = dia_do_ano, y = valor)) +
  geom_line(aes(colour = ano)) +
  facet_wrap(~ ano)


##Nova regressão

lm_finaistermos <- lm(valor ~ dia_do_ano, data = diasfinais_termos)


# A REGRESSÃO EM DIAS NÃO TEM MOSTRADO GRANDES CONCLUSÕES, TALVEZ EM MESES SEJA MELHOR

meses_termos <- obras %>% mutate(mes_do_ano = month(data_tc),
                                 ano = year(data_tc)) %>%   ## transformar todas as datas no dia do ano
  mutate(ano = as_factor(ano)) %>%  ##Fatorizando ano
  filter(!is.na(data_tc) & !(ano %in% c(2010, 2011)))  %>% ## Limpando anos com pouca informação
  group_by(ano, mes_do_ano) %>% 
  summarise(valor = sum(valor_planilha, na.rm = TRUE)) %>% 
  arrange(ano, mes_do_ano) %>% 
  group_by(ano) %>%  ## Aqui quero descobrir a porcentagem do valor emitido no ano, por mês
  mutate(total_ano = sum(valor)) %>% 
  mutate(prct = valor/total_ano)

## Visualização:

ggplot(meses_termos, aes(x = mes_do_ano, y = prct)) +
  geom_col(aes(fill = ano)) +
  facet_wrap(~ ano)

## A partir da visualização, parece-me interessantes investigar os termos emitidos em dezembro,
## especialmente em 2012, 2015, 2016 e 2018.




# SRE's decomposição ------------------------------------------------------

# 2 - Há um acréscimo enorme em celebrações de termos em alguns anos, 
# em quais regionais esse acréscimo foi acumulado



# 2015 --------------------------------------------------------------------

termos_2015 <- obras %>% mutate(ano = year(data_tc)) %>% 
  filter(ano == 2015)

dez_termos_2015 <- termos_2015 %>% mutate(mes = month(data_tc)) %>% 
 filter(mes == 12)

outros_termos_2015 <- termos_2015 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes != 12)

## Análise das SRE's 


maiores_medias_outro <- outros_termos_2015 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(outros_termos_2015$valor_planilha),
         prct_n = n_termos / nrow(outros_termos_2015)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)



maiores_medias_dez <- dez_termos_2015 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(dez_termos_2015$valor_planilha),
         prct_n = n_termos / nrow(dez_termos_2015)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)

## Nesse ponto estou confrontando as maiores médias mensais
## de geração de tcs, com 

maiores_medias_outro[maiores_medias_outro %in% maiores_medias_dez]

## "SRE TEÓFILO OTONI"   "SRE METROPOLITANA B" apenas estão entre as maiores medias
## tanto em dezembro como no resto do ano.

beneficiadas_015 <- maiores_medias_dez[!(maiores_medias_dez %in% maiores_medias_outro)]

## [1] "SRE DIAMANTINA"    "SRE MONTES CLAROS" "SRE ARAÇUAÍ"       "SRE JANUÁRIA"      "SRE PARÁ DE MINAS"
## [6] "SRE POUSO ALEGRE"  "SRE JANAÚBA"       "SRE ALMENARA" 


# 2016 --------------------------------------------------------------------

termos_2016 <- obras %>% mutate(ano = year(data_tc)) %>% 
  filter(ano == 2016)

dez_termos_2016 <- termos_2016 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes == 12)

outros_termos_2016 <- termos_2016 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes != 12)

## Análise das SRE's 


maiores_medias_outro <- outros_termos_2016 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(outros_termos_2016$valor_planilha),
         prct_n = n_termos / nrow(outros_termos_2016)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)



maiores_medias_dez <- dez_termos_2016 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(dez_termos_2016$valor_planilha),
         prct_n = n_termos / nrow(dez_termos_2016)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)

## Nesse ponto estou confrontando as maiores médias mensais
## de geração de tcs, com 

maiores_medias_outro[maiores_medias_outro %in% maiores_medias_dez]

## "SRE TEÓFILO OTONI"   "SRE METROPOLITANA B" apenas estão entre as maiores medias
## tanto em dezembro como no resto do ano.

beneficiadas_016 <- maiores_medias_dez[!(maiores_medias_dez %in% maiores_medias_outro)]



  

# 2017 --------------------------------------------------------------------

termos_2017 <- obras %>% mutate(ano = year(data_tc)) %>% 
  filter(ano == 2017)

dez_termos_2017 <- termos_2017 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes == 12)

outros_termos_2017 <- termos_2017 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes != 12)

## Análise das SRE's 


maiores_medias_outro <- outros_termos_2017 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(outros_termos_2017$valor_planilha),
         prct_n = n_termos / nrow(outros_termos_2017)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)



maiores_medias_dez <- dez_termos_2017 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(dez_termos_2017$valor_planilha),
         prct_n = n_termos / nrow(dez_termos_2017)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)

## Nesse ponto estou confrontando as maiores médias mensais
## de geração de tcs, com 

maiores_medias_outro[maiores_medias_outro %in% maiores_medias_dez]

## "SRE TEÓFILO OTONI"   "SRE METROPOLITANA B" apenas estão entre as maiores medias
## tanto em dezembro como no resto do ano.

beneficiadas_017 <- maiores_medias_dez[!(maiores_medias_dez %in% maiores_medias_outro)]




# 2018 --------------------------------------------------------------------

termos_2018 <- obras %>% mutate(ano = year(data_tc)) %>% 
  filter(ano == 2018)

dez_termos_2018 <- termos_2018 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes == 12)

outros_termos_2018 <- termos_2018 %>% mutate(mes = month(data_tc)) %>% 
  filter(mes != 12)

## Análise das SRE's 


maiores_medias_outro <- outros_termos_2018 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(outros_termos_2018$valor_planilha),
         prct_n = n_termos / nrow(outros_termos_2018)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)



maiores_medias_dez <- dez_termos_2018 %>% group_by(ano, sre) %>% 
  summarise(soma = sum(valor_planilha), 
            media_mensal = mean(valor_planilha),
            n_termos = n()) %>% 
  mutate(prct = soma / sum(dez_termos_2018$valor_planilha),
         prct_n = n_termos / nrow(dez_termos_2018)) %>% 
  arrange(desc(prct)) %>% ## Percentual do valor mensal liberado
  head(10) %>% 
  pull(sre)

## Nesse ponto estou confrontando as maiores médias mensais
## de geração de tcs, com 

maiores_medias_outro[maiores_medias_outro %in% maiores_medias_dez]

## "SRE TEÓFILO OTONI"   "SRE METROPOLITANA B" apenas estão entre as maiores medias
## tanto em dezembro como no resto do ano.

beneficiadas_018 <- maiores_medias_dez[!(maiores_medias_dez %in% maiores_medias_outro)]




# Avaliando SRE's "beneficiadas" ------------------------------------------

### Tomo por beneficiadas aquelas sres que não constavam na media mensal do ano e
### foram as maiores contempladas em dezembro.


beneficiadas <- c(beneficiadas_015,
                  beneficiadas_016,
                  beneficiadas_017,
                  beneficiadas_018)

table(beneficiadas) %>% sort(decreasing = TRUE)






# SRE Diamantina ----------------------------------------------------------

#Observando que Diamantina esteve entre as mais beneficiadas cabe fazer uma análise melhor.

#Por exemplo: quão acima foi o valor de dezembro pra ela em relação aos outros meses?
## Houve mudança nos tipos de obra?
