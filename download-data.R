### Para download da planilha será utilizado o pacote 



library(tidyverse)

library(lubridate)

library(googledrive)

library(readxl)

library(RCurl)



# Dowload da planilha -----------------------------------------------------


drive_download(
  file = as_id("1lx9F6m2p9iJzsB91OfwtYoiBtRBKKGk5QieuouBDXDU",overwrite = T),
  path = "get_data/planilha_dgrf.xlsx"
)

termos <- read_excel("get_data/planilha_dgrf.xlsx", skip = 3)



# Tratamento dos nomes da planilha ----------------------------------------

new_names <- c("planilha", "n_sin", "n_tc", "valor_planilha",
               "valor_eng_fiscal", "codesc", "sre", "munic",
               "escola", "predio", "ocupacao", "tipo_obra",
               "desc", "data_tc", "vigencia", "status_tc",
               "status_processo", "status_obraprinc", "observacoes_status",
               "data_homolog", "apagar", "status_pagamento", "liber_financ",
               "ordem_inicio", "previsao_termino", "empresa", "cnpj",
               "valor_contratual_planilha", "n_medicoes", "ultima_medicao",
               "avanc_fis", "valor_planilha_medicoes", "valor_autorizado_pgto",
               "valor_contrato_aditivo1", "perc_medido_aditivo1", "valor_pago_aditivo1",
               "desc_aditivo1", "valor_aditivo1", "empresa_aditivo1",
               "valor_contrato_aditivo2", "perc_medido_aditivo1", 
               "valor_pago_aditivo2", "desc_aditivo2", "valor_aditivo2",
               "empresa_aditivo2", "valor_contrato_aditivo3", "perc_medido_aditivo3",
               "valor_pago_aditivo3", "desc_aditivo3", "valor_aditivo3",
               "empresa_aditivo3", "valor_planilha_saldo1", "tipo_obra_saldo1",
               "desc_obra_saldo1", "status_obra_saldo1", "inicio_obra_saldo1",
               "previsao_termino_saldo1", "empresa_saldo1", "valor_contrato_saldo1",
               "avanc_fis_saldo1", "valor_medicoes_saldo1", "valor_pago_saldo1",
               "valor_planilha_saldo2", "tipo_obra_saldo2", "desc_obra_saldo2",
               "status_obra_saldo2", "inicio_obra_saldo2", "previsao_termino_saldo2",
               "empresa_saldo2", "valor_planilha_saldo2", "avanc_fis_saldo2",
               "valor_medicoes_saldo2", "valor_pgto_saldo2", "observacoes", "data_atualizacao"
               ) ##Novos nomes


names(termos) <- new_names
