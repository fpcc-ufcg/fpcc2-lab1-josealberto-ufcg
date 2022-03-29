library(dplyr)
library(readr)
library(here)
library(stringr)
library(ggplot2)

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


# QUESTÃO 01: média e desvio padrão das idades e do nível de program. R
turma_fpcc2$idade = gsub("\\.*", "", turma_fpcc2$idade)
turma_fpcc2$idade = gsub("\\ anos*", "", turma_fpcc2$idade)
turma_fpcc2$idade = as.integer(turma_fpcc2$idade)

print(paste0("média das idades: ",mean(turma_fpcc2$idade)))
print(paste0("Desevio padrão para as idades: ",sd(turma_fpcc2$idade)))

turma_fpcc2 %>%
  count(prog_r)%>%
  mutate(media = n/sum(n))%>%
  mutate(des_padrao = sqrt( (n-media)^2) )

turma_fpcc2_processado = turma_fpcc2


#===============================================================

# QUESTÃO 02: média de idade por curso

curso_media_idade = turma_fpcc2 %>%
  group_by(tipo_curso) %>%
  summarise_at(vars(idade), list(Media_idade = mean))
print(curso_media_idade)
#===============================================================


# QUESTÃO 03: gráfico de média de idade por curso
grafico_media_idade_curso <- ggplot(curso_media_idade, aes(x=tipo_curso, y=Media_idade, color=tipo_curso)) + 
  geom_bar(stat = "identity", fill="white")+
  coord_flip()

ggsave("grafico_media_idade_curso.png", grafico_media_idade_curso)

#===============================================================
# QUESTÃO 04: gráfico de média de idade por curso
turma_fpcc2$uf

turma_fpcc2$uf = gsub("\\Acre", "AC", turma_fpcc2$uf)
turma_fpcc2$uf = gsub("\\Pe.*", "PE", turma_fpcc2$uf)
turma_fpcc2$uf = gsub("\\Par.*", "PB", turma_fpcc2$uf)
turma_fpcc2$uf = gsub("\\PB.*", "PB", turma_fpcc2$uf)
turma_fpcc2$uf = gsub("\\Ala.*", "AL", turma_fpcc2$uf)

percent_aluno_df = subset(turma_fpcc2, uf != "Opção 1")


percent_aluno = percent_aluno_df %>%
  count(uf) %>%
  mutate(percentual = n/sum(n) *100)

percent_aluno_uf <- ggplot(percent_aluno, aes(x=uf, y=percentual, color=uf)) + 
  geom_bar(stat = "identity", fill="white")+
  ylab("%") + xlab("UF - Unifade Federativa")

ggsave("percent_aluno_uf.png", percent_aluno_uf)
#===============================================================

# QUESTÃO 05

tab <- matrix(c('Med_TOP5', mean(turma_fpcc2_processado$idade %>% head()),
                'Med_BOTTOM5', mean(turma_fpcc2_processado$idade %>% tail()),
                'Med_Geral', mean(turma_fpcc2_processado$idade)),ncol=2,byrow=TRUE)
colnames(tab) <- c("MEDIA","VALOR")
tab


