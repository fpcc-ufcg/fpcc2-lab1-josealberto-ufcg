library(dplyr)
library(readr)
library(here)

# Código base -------------------------------------------------------------

# lendo arquivo de dados
turma_fpcc2 <- read_csv(here("data/dados-fpcc2-22.1.csv"))

# visão geral dos dados 
glimpse(turma_fpcc2)
head(turma_fpcc2)

# renomeando colunas
colnames <- c("tipo_curso", "matricula", "conf_sumarios", "conf_histo", 
  "ler_ingles", "prog_r", "linear", "logistica",
  "exp_pesq", "exp_dev", "exp_admin", 
  "exp_desc", "area", "outra",
  "idade", "irmaos", "altura", "uf")
colnames(turma_fpcc2) <- colnames

# visão geral dados renomeados
glimpse(turma_fpcc2)



