library(dplyr)
library(RColorBrewer)

rm(list = ls())
questions <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\correcao-questionarios.csv", header = TRUE, sep = ";");
questions_type <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\composicao-questionarios.csv", header = TRUE, sep = ";");


# Verificando a experiencia em .net
experiencia_dotnet <- questions %>% 
  count(ExpNET, sort=TRUE, name="total") %>%
  mutate(percent = paste0(round(100*total/sum(total), 2)));




png("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\Scripts\\Result\\experiencia_dotnet.png", width = 2048, height = 1024, res = 152);
pie(as.numeric(x = experiencia_dotnet$percent), labels = paste(experiencia_dotnet$percent, "%", sep=""), border = "white", radius = 0.9, main = "Experiência em .NET", clockwise = TRUE, col=brewer.pal(7,"Set1"), cex=0.9);
legend("left",legend=experiencia_dotnet$ExpNET, bty = "n", fill=brewer.pal(7,"Set1"), cex=0.9);

dev.off();


