cat("\014")
rm(list = ls());

library(tidyverse);
library(ggplot2);
library(gridExtra);
library(finalfit);
library(kableExtra);

#setwd("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\")
setwd("C:\\Users\\User\\Desktop\\Codigos\\pattern-matching\\")
#setwd("~/Mestrado/tese/pattern-matching")


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

correcao <- read_delim(paste0(baseDirectory, "final-correcao-questionarios.csv"), delim=";", col_types=correcaoColSpec);


#
# Carga das composicoes dos formularios
#
composicaoColSpec <- cols(
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

composicao <- read_delim(paste0(baseDirectory, "composicao-questionarios.csv"), delim=";", col_types=composicaoColSpec);


#
# Organizacao e recodificacao dos dados
#
experiencia <- c("Menos que 1 ano" = "< 1 ano",
                 "Mais que 1 ano e menor que 3 anos" = "1-3 anos",
                 "Mais que 3 anos e menor que 5 anos" = "3-5 anos",
                 "Mais que 5 anos" = "5+ anos")

composicao <- composicao %>%
  gather(Questao, Tipo, -Form);

correcao <- correcao %>%
  mutate(ExpProg = recode(ExpProg, !!!experiencia)) %>%
  mutate(ExpNET = recode(ExpNET, !!!experiencia)) %>%
  gather(Questao, Acerto, -Participante, -ExpNET, -ExpProg, -Form) %>%
  inner_join(composicao, by=c("Form"="Form", "Questao"="Questao"))



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
# Número total de participantes
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



# ============================================================
#
# ANALISE DE NOTAS
#
# ============================================================

#
# Nota por participante - maior parte entre 6 e 8
#
notaParticipante <- correcao %>%
  group_by(Participante, ExpProg, ExpNET) %>%
  summarise(Nota = sum(Acerto), .groups="drop");

ggplot(notaParticipante, aes(Nota)) +
  geom_histogram(binwidth = 1, fill="blue", color="white", size=2) +
  ylab("Número de participantes") +
  theme_bw();


#
# Média das notas de todos os participantes
#
media <- notaParticipante %>% 
  summarize(mean = mean(Nota), .groups="drop")


#
# Juntando o menor grupo (3 pessoas) e recalculando as notas
#
correcao <- correcao %>% 
  mutate(ExpProg = if_else(ExpProg == "1-3 anos", "Até 3 anos", ExpProg)) %>%
  mutate(ExpProg = if_else(ExpProg == "< 1 ano", "Até 3 anos", ExpProg));

notaParticipante <- correcao %>%
  group_by(Participante, ExpProg, ExpNET) %>%
  summarise(Nota = sum(Acerto), .groups = 'drop');


#
# Nota média por nível de experiência de programação - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpProg <- notaParticipante %>%
  group_by(ExpProg) %>%
  summarise(Nota = round(mean(Nota), 2));

kruskal.test(notaParticipante$Nota, notaParticipante$ExpProg)

# precisa de um recode de ExpProg para fator?
pairwise.wilcox.test(notaParticipante$Nota, notaParticipante$ExpProg, p.adj = "bonf");


#
# Nota média por nível de experiência em .NET - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpNET <- notaParticipante %>%
  group_by(ExpNET) %>%
  summarise(Nota = round(mean(Nota),2));

kruskal.test(notaParticipante$Nota, notaParticipante$ExpNET)

# precisa de um recode de ExpNET para fator?
pairwise.wilcox.test(notaParticipante$Nota, notaParticipante$ExpNET, p.adj = "bonf");

notaMediaExpNET %>%
  kbl(col.names = c("Experiência", "Nota"), booktabs = TRUE ,format = 'html' ,table.attr = "style='width:450px;height:450px'") %>%
  kable_styling(latex_options = "striped", full_width = T) %>%
  column_spec(1, width = "6cm") %>%
  kable_classic(full_width = F);
  

# ============================================================
#
# ANALISE DE ACERTOS POR TIPO
#
# ============================================================

#
# Acertos por tipo - 543 pattern matching, 543 condicionais
#
acertosTipo <- correcao %>%
  group_by(Tipo) %>%
  summarise(Acertos = sum(Acerto));


#
# Acertos por formulario - pattern matching vence em 2 de 4 casos (diferença não significativa)
#
acertosFormTipo <- correcao %>%
  group_by(Form, Tipo) %>%
  summarise(Acertos = sum(Acerto), .groups = 'drop') %>%
  spread(Tipo, Acertos);

mxAcertosFormTipo <- matrix(as.numeric(as.matrix(acertosFormTipo)[,2:3]), ncol=4, byrow=TRUE);
chisq.test(mxAcertosFormTipo);


#
# Acertos por experiência em programação - pattern matching vence nos 2 de 3 casos com mais experiência (sem diferença significativa)
#
acertosExpProgTipo <- correcao %>%
  group_by(ExpProg, Tipo) %>%
  summarise(Acertos = sum(Acerto), .groups="drop") %>%
  spread(Tipo, Acertos);

mxAcertosExpProgTipo <- matrix(as.numeric(as.matrix(acertosExpProgTipo)[,2:3]), ncol=3, byrow=TRUE);
chisq.test(mxAcertosExpProgTipo);


#
# Acertos por experiência em .NET - pattern matching vence em 1 de 4 casos (vence nos mais experientes, sem diferença significativa)
#
acertosExpNETTipo <- correcao %>%
  group_by(ExpNET, Tipo) %>%
  summarise(Acertos = sum(Acerto)) %>%
  spread(Tipo, Acertos);

mxAcertosExpNETTipo <- matrix(as.numeric(as.matrix(acertosExpNETTipo)[,2:3]), ncol=4, byrow=TRUE);
chisq.test(mxAcertosExpNETTipo);

