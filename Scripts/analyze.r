library(dplyr)

rm(list = ls())
questions <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\correcao-questionarios.csv", header = TRUE, sep = ";");
questions_type <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\composicao-questionarios.csv", header = TRUE, sep = ";");


# total de acertos e percentual
question_temp <- questions %>%
                 mutate(acertos = rowSums(across(where(is.numeric)))) %>%
                 mutate(percent = round(100*(acertos/8),2));


# CRIA A COLUNA PARA ACERTOS DE PATTERN MATCHING E IF ENCADEADOS
question_temp$acerto_IF <- 0;
question_temp$percentual_acerto_if <- 0;
question_temp$acerto_PM <- 0;
question_temp$percentual_acerto_pm <- 0;
question_temp$erro_IF <- 0;
question_temp$percentual_erro_if <- 0;
question_temp$erro_PM <- 0;
question_temp$percentual_erro_pm <- 0;



# CALCULA a QUANTIDADE DE ERRO E ACERTOS POR IF E PM
for (row in 1:nrow(question_temp)) {
    
    #pega as colunas das questões
    qcols <- question_temp[row, 5:12];
    
    #pega qual o formulario o participante respondeu e quais tipo de questao (IF ou Pattern Matching)
    form <- questions_type %>% filter(Form == question_temp[row, "Form"]);
    
    
    #cria as variavies para acumular no contador
    acerto_if <- 0;
    acerto_pm <- 0;
    erro_pm <- 0;
    erro_if <- 0;
    
    # para cada coluna de questões, analisa o acerto e o erro
    for (i in colnames(qcols)){
      qtype <- form[i]; 
      qvalue <- question_temp[row, i];
      
       # caso a questão seja Pattern Matching
      if (qtype == "P") {
          # se acertou acumula o acerto
          if (qvalue == 1) {
            acerto_pm <- acerto_pm + 1;
          }
          # marca como errado
          else {
            erro_pm <- erro_pm + 1;
          }
      }
      #caso se IF
      else if (qtype == "I") {
        # se acertou acumula o acerto
        if (qvalue == 1) {
          acerto_if <- acerto_if + 1;
        }
        else  {
          erro_if <- erro_if + 1;
        }
          
      }
    }
    
    # coloca na linha os valores
    question_temp[row, "acerto_IF"] <- acerto_if;
    question_temp[row, "acerto_PM"] <- acerto_pm;
    question_temp[row, "erro_IF"] <- erro_if;
    question_temp[row, "erro_PM"] <- erro_pm;
    
    # calcula o percentual de erros e acertou
    question_temp[row, "percentual_acerto_if"] <- round(100 * (acerto_if / 8), 2);
    question_temp[row, "percentual_acerto_pm"] <- round(100 * (acerto_pm / 8), 2);
    question_temp[row, "percentual_erro_if"] <- round(100 * (erro_if / 8), 2);
    question_temp[row, "percentual_erro_pm"] <- round(100 * (erro_pm / 8), 2);
}


# CALCULA O TOTAL DE ACERTOS E ERROS GERAL
total_acerto_if <- c(total_acerto = sum(question_temp$acerto_IF));
total_acerto_pm <- c(total_acerto = sum(question_temp$acerto_PM));
sumarise_acertos <- rbind(total_acerto_if, total_acerto_pm);

png("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\Scripts\\Result\\acerto_if_pm.png", width = 1024, height = 768, res = 92);

# EXPORTA O GRAFICO
barplot(sumarise_acertos, 
        beside = TRUE, 
        ylab= "Total", 
        col=brewer.pal(7,"Set1"), 
        names.arg=c("IF/SWTICH","PATTERN MATCHING"), 
        space = 0.2,
        main = "Total de acertos entre IF & Pattern Matching");

dev.off();


# CALCULA OS ERROS
total_erro_if <- c(total_erro = sum(question_temp$erro_IF));
total_erro_pm <- c(total_erro = sum(question_temp$erro_PM));
sumarise_erros <- rbind(total_erro_if, total_erro_pm);


png("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\Scripts\\Result\\erros_if_pm.png", width = 1024, height = 768, res = 92);

# EXPORTA O GRAFICO
barplot(sumarise_erros, 
        beside = TRUE, 
        ylab= "Total", 
        col=brewer.pal(7,"Set1"), 
        names.arg=c("IF/SWTICH","PATTERN MATCHING"), 
        space = 0.2,
        main = "Total de Erros entre IF & Pattern Matching");

dev.off();

