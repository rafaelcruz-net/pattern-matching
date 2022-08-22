library(dplyr)
library(RColorBrewer)

rm(list = ls())
questions <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\final-correcao-questionarios.csv", header = TRUE, sep = ";");
questions_type <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\composicao-questionarios.csv", header = TRUE, sep = ";");


# Verificando a experiencia em programacao
experiencia_programacao <- questions %>% 
  count(ExpProg, sort=TRUE, name="total") %>%
  mutate(percent = paste0(round(100*total/sum(total), 2)));


pie(as.numeric(x = experiencia_programacao$percent), labels = paste(experiencia_programacao$percent, "%", sep=""), border = "white", clockwise = TRUE, col=brewer.pal(7,"Set1"), cex=0.7);
legend(x =.8 , y=1.5,  legend=experiencia_programacao$ExpProg, fill=brewer.pal(7,"Set1"), bty = "n");

