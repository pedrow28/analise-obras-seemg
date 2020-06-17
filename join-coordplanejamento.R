
# Bibliotecas e leitura dos dados -----------------------------------------


library(tidyverse)
library(readxl)

#Leitura planilha coordenação planejamento  
info_planej <- read_excel("get_data/CONTROLE DE PROCESSOS 16 06 2020 .xlsx")


##Mudar nomes planilha

names <- c("n_sin", "data_email", "n_sei",
           "proc_fis_email", "codesc", "sre",
           "munic", "escola", "tipo_estabelec",
           "tombado", "forma_ocupacao", "predio",
           "anexo_info", "n_alunos", "tipo_obra",
           "cod_dise", "tipo_atendimento", "atend_especial",
           "detalhes", "aditivo", "valor_analise",
           "responsavel", "aditivo_valor", "seriacao",
           "data_status_validacao", "status_validacao", "ressalvas",
           "responsavel_analise", "atendimento_vinculado", "status_atendimento",
           "ranking", "pendencias", "observacoes",
           "lista", "mao", "prioridade_diag",
           "emti", "saldo_dae", "pt", "tc", "ano")

names(info_planej) <- names



#Objeto com as planilhas
obras <- readRDS("rdata/relacao_termos.rds")




# Verificar existência mútua ----------------------------------------------

obras_chc <- obras %>% mutate(n_sin = as.character(n_sin))


obras_chc$n_sin %in% info_planej$n_sin


##Não estão encontrando. Buscar problemas como espaços.

info_planej %>% mutate(var_null = parse_number(n_sin)) %>% 
  filter(is.na(var_null)) %>% View()
