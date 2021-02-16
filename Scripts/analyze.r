library(dplyr)

rm(list = ls())
questions <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\correcao-questionarios.csv", header = TRUE, sep = ";");
questions_type <- read.delim("C:\\Users\\rafae\\OneDrive\\Documentos\\Mestrado\\Tese\\pattern-matching\\data\\composicao-questionarios.csv", header = TRUE, sep = ";");

# Verificando a experiencia em programacao
experiencia_programacao <- questions %>% 
                           count(ExpProg, sort=TRUE, name="total") %>%
                           mutate(percent = paste0(round(100*total/sum(total), 2), '%'));


# Verificando a experiencia em .net
experiencia_dotnet <- questions %>% 
                      count(ExpNET, sort=TRUE, name="total") %>%
                      mutate(percent = paste0(round(100*total/sum(total), 2), '%'));


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

