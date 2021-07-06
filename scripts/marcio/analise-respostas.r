rm(list = ls());
library(tidyverse);
library(ggplot2);
library(gridExtra);
library(finalfit);
library(kableExtra);
setwd("~/Mestrado/tese/pattern-matching")

#
# Configuracao
#
baseDirectory <- "data/";


#
# Carga dos dados
#
experiencia <- c("Menos que 1 ano" = "< 1 ano",
                 "Mais que 1 ano e menor que 3 anos" = "1-3 anos",
                 "Mais que 3 anos e menor que 5 anos" = "3-5 anos",
                 "Mais que 5 anos" = "5+ anos")

correcao <- read_delim(paste0(baseDirectory, "correcao-questionarios.csv"), delim=";") %>%
  mutate(ExpProg = recode(ExpProg, !!!experiencia)) %>%
  mutate(ExpNET = recode(ExpNET, !!!experiencia)) %>%
  gather(Questao, Acerto, -Participante, -ExpNET, -ExpProg, -Form);

composicao <- read_delim(paste0(baseDirectory, "composicao-questionarios.csv"), delim=";") %>%
  gather(Questao, Tipo, -Form);

correcao <- correcao %>%
  inner_join(composicao, by=c("Form"="Form", "Questao"="Questao"))



# ============================================================
#
# ANALISE DE CONSISTENCIA
#
# ============================================================

#
# Questoes por tipo - equilibrado, 216 por questão.
#
correcao %>%
  group_by(Tipo) %>%
  summarise(Acertos = n());



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
  summarise(Nota = sum(Acerto));

ggplot(notaParticipante, aes(Nota)) +
  geom_histogram();



#
# Caracterizacao: A maioria dos participantes é experiente em programação (39 dos 54 participantes têm 5+ anos de experiência)
#
notaParticipante %>% 
  group_by(ExpProg) %>% 
  summarise(n = n());


#
# Caracterizacao: Boa distribuição de experiência com .NET
#
notaParticipante %>% 
  group_by(ExpNET) %>% 
  summarise(n = n());


#
# Juntando o menor grupo (4 pessoas) e recalculando as notas
#
correcao <- correcao %>% 
  mutate(ExpNET = if_else(ExpNET == "1-3 anos", "1-5 anos", ExpNET)) %>%
  mutate(ExpNET = if_else(ExpNET == "3-5 anos", "1-5 anos", ExpNET));

notaParticipante <- correcao %>%
  group_by(Participante, ExpProg, ExpNET) %>%
  summarise(Nota = sum(Acerto));


#
# Dendograma - meio confuso ...
#
#nivel <- c("Menos que 1 ano" = 1,
#           "Mais que 1 ano e menor que 3 anos" = 2,
#           "Mais que 3 anos e menor que 5 anos" = 3,
#           "Mais que 5 anos" = 4);
# 
#porNivel <- read_delim(paste0(baseDirectory, "correcao-questionarios.csv"), delim=";") %>%
#      mutate(ExpProg = recode(ExpProg, !!!nivel)) %>%
#      mutate(ExpNET = recode(ExpNET, !!!nivel)) %>%
#      select(Participante, ExpNET, ExpProg);
# 
#dd <- dist(scale(porNivel[2:3]), method = "euclidean");
#hc <- hclust(dd);
#
#plot(hc);
#table(porNivel[2:3]);



#
# Nota média por nível de experiência de programação - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpProg <- notaParticipante %>%
  group_by(ExpProg) %>%
  summarise(Nota = round(mean(Nota),2));

pairwise.wilcox.test(notaParticipante$Nota, notaParticipante$ExpProg, p.adj = "bonf");


#
# Nota média por nível de experiência em .NET - mais junior, nota mais baixa (diferença não significativa)
#
notaMediaExpNET <- notaParticipante %>%
  group_by(ExpNET) %>%
  summarise(Nota = round(mean(Nota),2));

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
# Acertos por tipo - 192 pattern matching, 179 condicionais
#
acertosTipo <- correcao %>%
  group_by(Tipo) %>%
  summarise(Acertos = sum(Acerto));


#
# Acertos por formulario - pattern matching vence em 3 de 4 casos (diferença não significativa)
#
acertosFormTipo <- correcao %>%
  group_by(Form, Tipo) %>%
  summarise(Acertos = sum(Acerto)) %>%
  spread(Tipo, Acertos);

mxAcertosFormTipo <- matrix(as.numeric(as.matrix(acertosFormTipo)[,2:3]), ncol=4, byrow=TRUE);
chisq.test(mxAcertosFormTipo);



#
# Acertos por experiência em programação - pattern matching vence em 2 de 3 casos, com 1 empate (sem diferença significativa)
#
acertosExpProgTipo <- correcao %>%
  group_by(ExpProg, Tipo) %>%
  summarise(Acertos = sum(Acerto)) %>%
  spread(Tipo, Acertos);

mxAcertosExpProgTipo <- matrix(as.numeric(as.matrix(acertosExpProgTipo)[,2:3]), ncol=3, byrow=TRUE);
chisq.test(mxAcertosExpProgTipo);


#
# Acertos por experiência em .NET - pattern matching vence em 2 de 3 casos, com 1 empate (sem diferença significativa)
#
acertosExpNETTipo <- correcao %>%
  group_by(ExpNET, Tipo) %>%
  summarise(Acertos = sum(Acerto)) %>%
  spread(Tipo, Acertos);

mxAcertosExpNETTipo <- matrix(as.numeric(as.matrix(acertosExpNETTipo)[,2:3]), ncol=3, byrow=TRUE);
chisq.test(mxAcertosExpNETTipo);
