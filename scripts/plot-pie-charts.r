cat("\014")
rm(list = ls());

library(dplyr)
library(RColorBrewer)

setwd("C:\\Users\\User\\Desktop\\Codigos\\pattern-matching\\")

#
# Carrega os dados
#
questions <- read.delim("data\\correcao-questionarios.csv", header = TRUE, sep = ";");
genero <- read.delim("data\\participantes-por-genero.csv", header = TRUE, sep = ";");


#
# Prepara o PDF
#
pdf(file="pie-charts.pdf", width=10, height=4)
par(mfrow=c(1, 3))


#
# Gráfico pela experiencia em programacao
#
experiencia_programacao <- questions %>% 
  count(ExpProg, sort=TRUE, name="total") %>%
  mutate(ExpProg = recode(ExpProg, 'Mais que 5 anos' = '> 5y', 'Mais que 3 anos e menor que 5 anos' = '3-5y', 'Mais que 1 ano e menor que 3 anos' = '1-3y', 'Menos que 1 ano' = '0-1y')) %>% 
  mutate(percent = paste0(round(100 * total / sum(total), 1)));

pie(as.numeric(x = experiencia_programacao$percent), 
    labels = paste0(experiencia_programacao$ExpProg, ": ", experiencia_programacao$percent, "%"), 
    border = "white", 
    clockwise = TRUE, 
    col = brewer.pal(7, "Set1"), 
    radius = 0.8);


#
# Gráfico pela experiencia em NET
#
experiencia_dotnet <- questions %>% 
  count(ExpNET, sort=TRUE, name="total") %>%
  mutate(ExpNET = recode(ExpNET, 'Mais que 5 anos' = '> 5y', 'Mais que 3 anos e menor que 5 anos' = '3-5y', 'Mais que 1 ano e menor que 3 anos' = '1-3y', 'Menos que 1 ano' = '0-1y')) %>% 
  mutate(percent = paste0(round(100 * total / sum(total), 1)));

pie(as.numeric(x = experiencia_dotnet$percent), 
    labels = paste0(experiencia_dotnet$ExpNET, ": ", experiencia_dotnet$percent, "%"), 
    border = "white", 
    clockwise = TRUE, 
    col = brewer.pal(7, "Set1"), 
    radius = 0.8);


#
# Gráfico pelo genero
#
genero_type <- genero %>%
  count(Genero, sort=TRUE, name="total") %>%
  mutate(percent = paste0(round(100 * total / sum(total), 1)));

pie(as.numeric(x = genero_type$percent), 
    labels = paste0(genero_type$Genero, ": ", genero_type$percent, "%"), 
    border = "white", 
    clockwise = TRUE, 
    col = brewer.pal(7, "Set1"), 
    radius = 0.8);


#
# Finalizacao
#
dev.off()
