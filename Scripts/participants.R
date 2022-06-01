library(dplyr)
library(RColorBrewer);

rm(list = ls())
genero <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\participantes-por-genero.csv", header = TRUE, sep = ";");

genero_recode <- c("M" = "Masculino",
            "F" = "Feminino");


# Verificando a participação por genero
genero_type <- genero %>%
  mutate(Genero = recode(Genero, !!!genero_recode)) %>%
  count(Genero, sort=TRUE, name="total") %>%
  mutate(percent = paste0(round(100*total/sum(total), 2)));

pie(as.numeric(x = genero_type$percent), labels = paste(genero_type$percent, "%", sep=""), border = "white", clockwise = TRUE, col=brewer.pal(7,"Set1"), cex=0.8, radius = 1.0);
legend(x =.9 , y=.9,  legend=genero_type$Genero, fill=brewer.pal(7,"Set1"), bty = "n", cex = 0.8);
