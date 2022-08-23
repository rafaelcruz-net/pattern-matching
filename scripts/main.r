cat("\014")
rm(list = ls());

library(tidyverse);
library(ggplot2);
library(gridExtra);
library(finalfit);
library(kableExtra);

setwd("C:\\Users\\User\\Desktop\\Codigos\\pattern-matching\\")


#
# Configuracao
#
baseDirectory <- "data\\";


# ============================================================
#
# CARGA DOS DADOS
#
# ============================================================

#
# Carga das correcoes
#
correcaoColSpec <- cols(
  Participante = col_character(),
  ExpProg = col_character(),
  ExpNET = col_character(),
  Form = col_character(),
  Q1 = col_double(),
  Q2 = col_double(),
  Q3 = col_double(),
  Q4 = col_double(),
  Q5 = col_double(),
  Q6 = col_double(),
  Q7 = col_double(),
  Q8 = col_double()
)

correcao <- read_delim(paste0(baseDirectory, "correcao-questionarios.csv"), delim=";", col_types=correcaoColSpec);


#
# Carga dos tratamentos dos questionarios
#
tratamentoColSpec <- cols(
  Form = col_character(),
  Q1 = col_character(),
  Q2 = col_character(),
  Q3 = col_character(),
  Q4 = col_character(),
  Q5 = col_character(),
  Q6 = col_character(),
  Q7 = col_character(),
  Q8 = col_character()
)

tratamento <- read_delim(paste0(baseDirectory, "tratamento-questionarios.csv"), delim=";", col_types=tratamentoColSpec);


#
# Carga das questoes utilizadas nos questionarios
#
questaoColSpec <- cols(
  Form = col_character(),
  Q1 = col_character(),
  Q2 = col_character(),
  Q3 = col_character(),
  Q4 = col_character(),
  Q5 = col_character(),
  Q6 = col_character(),
  Q7 = col_character(),
  Q8 = col_character()
)

questao <- read_delim(paste0(baseDirectory, "questao-questionarios.csv"), delim=";", col_types=questaoColSpec);


#
# Carga do genero dos participantes
#
generoColSpec <- cols(
  Participante = col_character(),
  ExpProg = col_character(),
  ExpNET = col_character(),
  Form = col_character(),
  Genero = col_character()
)

genero_participantes <- read_delim(paste0(baseDirectory, "participantes-por-genero.csv"), delim=";", col_types=generoColSpec);


#
# Recodificacao da experiencia dos participantes
#
experiencia <- c("Menos que 1 ano" = "< 1 ano",
                 "Mais que 1 ano e menor que 3 anos" = "1-3 anos",
                 "Mais que 3 anos e menor que 5 anos" = "3-5 anos",
                 "Mais que 5 anos" = "5+ anos")

correcao <- correcao %>%
  mutate(ExpProg = recode(ExpProg, !!!experiencia)) %>%
  mutate(ExpNET = recode(ExpNET, !!!experiencia))


#
# Reune todos os dados em uma unica tabela
#
tratamento <- tratamento %>%
  gather(OrdemQuestao, Tipo, -Form);

questao <- questao %>%
  gather(OrdemQuestao, Questao, -Form)

correcao <- correcao %>%
  gather(OrdemQuestao, Acerto, -Participante, -ExpNET, -ExpProg, -Form) %>%
  inner_join(tratamento, by=c("Form"="Form", "OrdemQuestao"="OrdemQuestao")) %>%
  inner_join(questao, by=c("Form"="Form", "OrdemQuestao"="OrdemQuestao")) %>% 
  select(Participante, ExpProg, ExpNET, Form, Questao, Tipo, Acerto)



# ============================================================
#
# ANALISE DE CONSISTENCIA
#
# ============================================================

#
# - equilibrado, 672 respostas por tipo de resolução.
#
# - desequilibrado, 40 respostas para F1, 39 para F2, 57 para F3 e 32 para F4
#
# - equilibrado, 158 respostas por questão.
#
correcao %>%
  group_by(Tipo) %>%
  summarise(Respostas = n());

correcao %>%
  group_by(Form) %>%
  summarise(Respostas = n() / 8);

correcao %>%
  group_by(Questao) %>%
  summarise(Respostas = n());



# ============================================================
#
# DISTRIBUICAO DOS PARTICIPANTES
#
# ============================================================

#
# Número total de participantes - 168
#
correcao %>% 
  summarize(count = n() / 8)

  
#
# Participantes agrupados por experiência em programação: a maioria 
# dos participantes é experiente em programação
#
correcao %>% 
  group_by(ExpProg) %>% 
  summarize(count = n() / 8, .groups="keep") %>% 
  ungroup() %>% 
  mutate(perc = 100 * count / sum(count))


#
# Participantes agrupados por experiência em programação: boa 
# distribuição de experiência com C#
#
correcao %>% 
  group_by(ExpNET) %>% 
  summarize(count = n() / 8, .groups="keep") %>% 
  ungroup() %>% 
  mutate(perc = 100 * count / sum(count))


#
# Participantes por genero: baixa representatividade feminina
#
genero_participantes %>% 
  group_by(Genero) %>% 
  summarize(count = n(), .groups="keep") %>% 
  ungroup() %>% 
  mutate(perc = 100 * count / sum(count))





# ============================================================
#
# ANALISE DE NOTAS
#
# ============================================================

#
# Reune os grupos de menor experiencia em programacao no geral
#
correcao <- correcao %>%
  mutate(ExpProg = if_else(ExpProg == "1-3 anos", "Até 3 anos", ExpProg)) %>%
  mutate(ExpProg = if_else(ExpProg == "< 1 ano", "Até 3 anos", ExpProg));


#
# Nota por participante - maior parte entre 6 e 8
#
notaParticipante <- correcao %>%
  group_by(Participante, ExpProg, ExpNET, Form) %>%
  summarise(Nota = sum(Acerto), .groups="drop");


#
# Grafico de numero de participantes por nota
#
distribuicao_notas <- ggplot(notaParticipante, aes(Nota)) +
  geom_histogram(binwidth = 1, fill="blue", color="white", size=2) +
  ylab("Number of subjects") +
  xlab("Subject score") +
  theme_bw()

ggsave(filename="subject-score-distribution.pdf", plot=distribuicao_notas, width=20, height=9, units="cm", device="pdf");


#
# Média das notas de todos os participantes
#
notaParticipante %>% 
  summarize(mean = mean(Nota), .groups="drop")


#
# Nota média por formulário
#
notaFormulario <- notaParticipante %>% 
  group_by(Form) %>%
  summarise(NotaMedia = mean(Nota), NotaDesvio = sd(Nota), .groups = 'drop');

kruskal.test(notaParticipante$Nota, notaParticipante$Form)



#
# Nota média por nível de experiência de programação - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpProg <- notaParticipante %>%
  group_by(ExpProg) %>%
  summarise(NotaMedia = mean(Nota), NotaDesvio = sd(Nota));

kruskal.test(notaParticipante$Nota, notaParticipante$ExpProg)


#
# Nota média por nível de experiência em .NET - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpNET <- notaParticipante %>%
  group_by(ExpNET) %>%
  summarise(NotaMedia = mean(Nota), NotaDesvio = sd(Nota));

kruskal.test(notaParticipante$Nota, notaParticipante$ExpNET)

  

# ============================================================
#
# ANALISE DE ACERTOS POR TIPO
#
# ============================================================

#
# Acertos por tipo
#
acertosTipo <- correcao %>%
  group_by(Tipo) %>%
  summarise(Acertos = sum(Acerto), .groups="drop") %>% 
  mutate(Percentual = 100 * Acertos / 672);


#
# Acertos por tipo e questao
#
acertosQuestaoTipo <- correcao %>%
  group_by(Questao, Tipo) %>%
  summarise(Acertos = sum(Acerto), Total=n(), .groups="drop") %>% 
  mutate(Percentual = 100 * Acertos / Total) %>% 
  pivot_wider(names_from = Tipo, values_from=c(Acertos, Total, Percentual)) %>% 
  select(Questao, Acertos_I, Total_I, Percentual_I, Acertos_P, Total_P, Percentual_P)

mxAcertosQuestaoTipo <- acertosQuestaoTipo %>% 
  select(Percentual_I, Percentual_P) %>% 
  as.matrix() %>% 
  t()

chisq.test(mxAcertosQuestaoTipo[,1:8]);

maiores_diferencas_pm <- acertosQuestaoTipo %>% 
  mutate(diff = Percentual_P - Percentual_I) %>% 
  filter(!is.na(diff)) %>% 
  select(Questao, diff) %>% 
  arrange(-diff) %>% 
  top_n(3)

maiores_diferencas_cc <- acertosQuestaoTipo %>% 
  mutate(diff = Percentual_I - Percentual_P) %>% 
  filter(!is.na(diff)) %>% 
  select(Questao, diff) %>% 
  arrange(-diff) %>% 
  top_n(3)


#
# Acertos por formulario - pattern matching vence em 2 de 4 casos (diferença não significativa)
#
acertosFormTipo <- correcao %>%
  group_by(Form, Tipo) %>%
  summarise(Acertos = sum(Acerto), Percentual=100 * sum(Acerto) / n(), .groups = 'drop') %>%
  pivot_wider(names_from=Tipo, values_from=c(Acertos, Percentual));

mxAcertosFormTipo <- matrix(as.numeric(as.matrix(acertosFormTipo %>% select(Form, Acertos_I, Acertos_P))[,2:3]), ncol=4, byrow=TRUE);
chisq.test(mxAcertosFormTipo);


#
# Acertos por experiência em programação - pattern matching vence nos 2 de 3 casos com mais experiência (sem diferença significativa)
#
acertosExpProgTipo <- correcao %>%
  group_by(ExpProg, Tipo) %>%
  summarise(Acertos = sum(Acerto), Percentual=100 * sum(Acerto) / n(), .groups="drop") %>%
  pivot_wider(names_from=Tipo, values_from=c(Acertos, Percentual));

mxAcertosExpProgTipo <- matrix(as.numeric(as.matrix(acertosExpProgTipo %>% select(ExpProg, Acertos_I, Acertos_P))[,2:3]), ncol=3, byrow=TRUE);
chisq.test(mxAcertosExpProgTipo);


#
# Acertos por experiência em .NET - pattern matching vence em 1 de 4 casos (vence nos mais experientes, sem diferença significativa)
#
acertosExpNETTipo <- correcao %>%
  group_by(ExpNET, Tipo) %>%
  summarise(Acertos = sum(Acerto), Percentual=100 * sum(Acerto) / n(), .groups="drop") %>%
  pivot_wider(names_from=Tipo, values_from=c(Acertos, Percentual));

mxAcertosExpNETTipo <- matrix(as.numeric(as.matrix(acertosExpNETTipo %>% select(ExpNET, Acertos_I, Acertos_P))[,2:3]), ncol=4, byrow=TRUE);
chisq.test(mxAcertosExpNETTipo);

