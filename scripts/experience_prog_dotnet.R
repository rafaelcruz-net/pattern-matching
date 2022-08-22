library(dplyr)
library(RColorBrewer)

rm(list = ls())
questions <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\final-correcao-questionarios.csv", header = TRUE, sep = ";");
questions_type <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\composicao-questionarios.csv", header = TRUE, sep = ";");


# Verificando a experiencia em .net
experiencia_dotnet <- questions %>% 
  count(ExpNET, sort=TRUE, name="total") %>%
  mutate(percent = paste0(round(100*total/sum(total), 2)));

legend(x = "bottom", legend=experiencia_dotnet$ExpNET, fill=brewer.pal(7,"Set1"), cex = 0.7, bty = "n",  xpd = TRUE, inset = c(-.95,0.32), horiz = FALSE);



