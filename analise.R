## SCRIPT PARA ANÁLISE


# pacotes -----------------------------------------------------------------

library(tidyverse)

library(lubridate)

library(readxl)


# Pegando info ------------------------------------------------------------

## Aqui vou puxar a planilha já tratada, na pasta "rdata", para não ficar baixando do Google Drive.

obras <- readRDS("rdata/relacao_termos.rds")
