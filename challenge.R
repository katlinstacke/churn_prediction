# Libraries and Packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tibble")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("MASS")
install.packages("caret")
install.packages("rpart")
install.packages("rminer")
install.packages("rpart.plot")
install.packages("nnet")
install.packages("Amelia")
install.packages("caTools")
install.packages("dummies")
install.packages("partykit")
install.packages("randomForest")
install.packages("rmarkdown")

# load packages 
library(tibble)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(MASS)
library(caret)
library(rpart)
library(rpart.plot)
library(rminer)
library(nnet)
library(Amelia)
library(caTools)
library(dummies)
library(dplyr)
library(partykit)
library(randomForest)
library(rmarkdown)

# Change directory
setwd("/home/stacke/Documentos/Desafio_Indicium")

# Import datasets to tibble

'Tibble é um data.frame mais enxuto na visualização, pois exibe até 10 linhas somente no console, e já detalha as classes das variáveis, tornando-se muito prático manipular os dados. '

data_training <- as_tibble(read.csv("Abandono_clientes.csv", header = TRUE))
data_testing <- as_tibble(read.csv("Abandono_teste.csv", header = TRUE, sep=";"))

print(data_training)
print(data_testing)


##### Estatística Descritiva #####

###### Separar a base em churn e não churn
data_training_churn <- filter(data_training, Exited == 1);
data_training_notchurn <- filter(data_training, Exited == 0);

# Estimated Salary

data_training_churn
is.factor(data_training_churn$EstimatedSalary)
is.numeric(data_training_churn$EstimatedSalary)
range(data_training_churn$EstimatedSalary) 'ver valores mínimos e máximos para definir o número de agrupamentos/classes'
nclass.Sturges(data_training_churn$EstimatedSalary)
EstimatedSalary_churn <- table(cut(data_training_churn$EstimatedSalary, seq(11.0, 199993.0, l = 16)))
prop.table(EstimatedSalary_churn)
salary_graphic <- hist(EstimatedSalary_churn)

data_training_notchurn
is.factor(data_training_notchurn$EstimatedSalary)
is.numeric(data_training_notchurn$EstimatedSalary)
range(data_training_notchurn$EstimatedSalary) 'ver valores mínimos e máximos para definir o número de agrupamentos/classes'
nclass.Sturges(data_training_notchurn$EstimatedSalary)
EstimatedSalary_notchurn <- table(cut(data_training_notchurn$EstimatedSalary, seq(11.0, 199993.0, l = 16)))
prop.table(EstimatedSalary_notchurn)
salary_graphic <- hist(EstimatedSalary_notchurn)

# CreditScore

data_training_churn
is.factor(data_training_churn$CreditScore)
is.numeric(data_training_churn$CreditScore)
range(data_training_churn$CreditScore) 'ver valores mínimos e máximos para definir o número de agrupamentos/classes'
nclass.Sturges(data_training_churn$CreditScore)
CreditScore_churn <- table(cut(data_training_churn$CreditScore, seq(20, 199993.0, l = 4)))
prop.table(CreditScore_churn)
salary_graphic <- hist(CreditScore_churn)

data_training_notchurn
is.factor(data_training_notchurn$CreditScore)
is.numeric(data_training_notchurn$CreditScore)
range(data_training_notchurn$CreditScore) 'ver valores mínimos e máximos para definir o número de agrupamentos/classes'
nclass.Sturges(data_training_notchurn$CreditScore)
CreditScore_notchurn <- table(cut(data_training_notchurn$CreditScore, seq(11.0, 199993.0, l = 16)))
prop.table(CreditScore_notchurn)
salary_graphic <- hist(CreditScore_notchurn)


## Análise Univariada

'Foram classificadas as variáveis quanto a seus tipos: qualitativas (nominal ou ordinal) ou quantitativa (discreta ou contínua). 
A fim de resumir o comportamento das variáveis veremos gráficos, tabelas e/ou outras medidas.'

# Nominal

'Exited porque essa é a variável resposta'

# Discreta 

# Contínua

is.factor(data_training$EstimatedSalary)
is.numeric(data_training$EstimatedSalary)
range(data_training$EstimatedSalary) 'ver valores mínimos e máximos para definir o número de agrupamentos/classes'
nclass.Sturges(data_training$EstimatedSalary)
EstimatedSalary <- table(cut(data_training$EstimatedSalary, seq(11.0, 199993.0, l = 16)))
prop.table(EstimatedSalary)
salary_graphic <- hist(EstimatedSalary)


p1 <- ggplot(data_training, aes(x=Gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p1

p2 <- ggplot(data_training, aes(x=HasCrCard)) + ggtitle("HasCrCard") + xlab("HasCrCard") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2

p3 <- ggplot(data_training, aes(x=IsActiveMember)) + ggtitle("IsActiveMember") + xlab("IsActiveMember") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3

data_training

summary(data_training)

# Customer Churn Overview
ggplot(data_training, aes(x = data_training$Exited)) + 
  geom_freqpoly(aes(color = Exited, linetype = Exited)) +
  theme_minimal()
labs(title = "Abandono de Clientes de Instituição Financeira", x = "Abandono", y = "Frequência")

# Age Overview
age_graphic <- ggplot(data_training, aes(x = data_training$Age)) +
  geom_histogram(color = "Gray", fill = "Blue", binwidth = 3) +
  labs(x = "Idade", y = "Quantidade", title = "Idade do Cliente")
mean(data_training$Age) # = 38.9218
age_graphic

# Tenure Overview
tenure_graphic <- ggplot(data_training, aes(x = data_training$Tenure)) +
  geom_histogram(color = "Gray", fill = "Blue", binwidth = 3) +
  labs(x = "Tempo", y = "Quantidade", title = "Tempo de Permanência")
mean(data_training$Tenure)  # = 5.0128
tenure_graphic

##### Exploration Analysis and ETL ###### 
'Os modelos preditivos baseados no aprendizado de máquina, ao contrário dos modelos de estatística tradicional, são gerados pelo algoritmo do computador, e não por interpretação de resultados.
Num primeiro momento, é importante analisar os dados de forma exploratória para entender quais variáveis temos e suas dimensões.' 

print(dim(data_training))
print(names(data_training))

'Assim, temos 14 variáveis (colunas) e 10000 casos de variáveis (linhas). Há variáveis como Gender (Gênero) e Tenure (Tempo de Permanência). O output que queremos prever é a variável Exited (abandono), o target.'

print(str(data_training)) 
print(summary(data_training)) 
'
* RowNumber (número da linha)       
* CustomerId (Id do cliente)      
* Surname (sobrenome)        
* CreditScore (pontuação de crédito)     
* Geography (geografia, origem do cliente)      
* Gender (gênero)          
* Age (idade)             
* Tenure (tempo de permanência)          
* Balance (balanço/saldo contábil)         
* NumOfProducts (número de produtos)   
* HasCrCard (possui cartão de crédito)       
* IsActiveMember (é membro ativo) 
* EstimatedSalary (estimativa salarial)
* Exited (abandono/saída do cliente)
'
'
Explorar cada ponto de dados e suas proporções - os modelos de aprendizado de máquina geralmente fazem isso significativamente melhor do que os humanos e encontram padrões inesperados.'

'Explicar o que é ETL. 
O processo de limpeza e tratamento de dados é tão importante quanto o de modelagem, já que pode evitar retrabalhos futuros e melhores modelagens.
O primeiro passo é alterar as variáveis de caracteres (categóricas) para fatores, pois estas variáveis são armazenadas em um fator. Geralmente, 
as variáveis categóricas são alocadas em 0, 1, 2, 3, com documentação adicional para explicar o que cada número significa. 
Claramente, essa não é a forma mais simples e eficiente de descrever estatisticamente as variáveis categóricas, justificando o armazenamento em um vetor de caracteres, ou seja, fator.
As classes dos objetos em R são críticas para o desempenho.'

'1. Transformar os casos de variáveis categóricas de 0 e 1 para Não e Sim. É importante informar que as variáveis HasCrCard, IsActiveMember e Exited são variáveis categóricas e não numéricas, por isso
o uso do comando factor(). Assim, redefinem-se as variáveis com os rótulos labels() associados aos níveis (levels).'

'2. Verificar valores ausentes. Verifiquei se há valores ausentes na colunas, mas como não havia, não foi preciso remover nenhum dado.'

sapply(data_training, function(x) sum(is.na(x)))

'3. Mínimos e Máximos de tempo de permanência'

min(data_training$Tenure); max(data_training$Tenure)

'4. Verificar a correlação entre as variáveis numéricas'

numeric.var <- sapply(data_training, is.numeric)
corr.matrix <- cor(data_training[,numeric.var])
corrplot(corr.matrix, main = "\n\nCorrelation Plot for Numerical Variables", method = "number")
corr.matrix
'Foi possível verificar que as variáveis NumOfProducts e Balance são negativamente correlacionadas, ou seja, movem-se em direções opostas. 
No entanto, como a correlação é muito baixa, não removerei as variáveis.'

'5. Cada linha representa um cliente e selecionei as variáveis (colunas) que representam atributos dos clientes. Do mesmo modo,
é importante remover as variáveis que não acrescentam informações úteis ao modelo, como RowNumber, CustomerId e Surname.'

data_training$RowNumber <- NULL
data_training$CustomerId <- NULL
data_training$Surname <- NULL
data_training



##### MODELAGEM #####

'Para simular um experimento com o objetivo de prever se os clientes vão dar “churn”, precisamos trabalhar com um banco de dados 
particionado, seguindo a metodologia de validação cruzada (cross validation). O dataset de treinamento será dividido em duas partes, uma será o conjunto 
de treinamento que será usado para criar o modelo, e a segunda parte será o conjunto de testes que será usado para validar o nosso 
modelo. Existem diversos modelos que podem ser utilizados para realizar previsões, dos quais selecionei o de Logistic Regression, Tree Decision e Random Forest. 
O modelo com maior acurácia será utilizado para prever o churn no dataset de teste. '

## LOGISTIC REGRESSION ##

'Logistic Regression - é um modelo de classificação linear e como estamos lidando com previsão de abandono de clientes ou não,
um modelo de classificação é o ideal. O modelo de regressão logística é o melhor que podemos interpretar porque podemos verificar
com mais facilidade a relação entre as features e os outputs, a escolha de um modelo mais simples é considerado uma das boas práticas de Machine Learning.
A desvantagem da regressão logística é que ela tem um viés no sentido de ajustes lineares. Se o limite de decisão não for linear, talvez não funcione tão bem quanto um modelo como o Random Forest.'
'Tanto problemas de classificação quanto de regressão são modelos preditivos, mas optou-se pelo modelo de classificação pois, 
neste caso, a variável resposta é de natureza qualitativa (churn sim ou não).'

# Dividir a base de dados 

library(caret)

intrain <- createDataPartition(y = data_training$Exited, p = 0.7, list = FALSE)
set.seed(2018)
training <- data_training[intrain, ]
testing <- data_training[ -intrain, ]

dim(training); dim(testing)

# fitting the model SEM FACTOR
'Para implementar um modelo de regressão logística utilizarei a função de modelos lineares generalizados (GLM). Existem diferente tipos
de regressão logística linear, mas será usada, aqui, o argumento family = binomial, pois fornece estimativas pontuais dos parâmetros, erros
padrão, p-valores de Wald entre outras informações.'

log_model <- glm(Exited~., data = training, family = binomial(link = "logit"))
print(summary(log_model))

'REVER: As principais variáveis que explicam o modelo são as que possuem o menor p-valor, sendo estatisticamente mais significantes: 
GeographyGermany, GenderMale, Age, Balance e IsActiveMemberSim.'

anova(log_model, test="Chisq")

"Podemos notar a queda no desvio ao adicionar uma variável de cada vez. Geography, Gender, Age e IsActiveMember reduziram significativamente
o desvio, enquanto Balance não parece ter melhorado o modelo com seu desvio de 36.02, mesmo tendo um p-valor baixo."

# Assessing the predictive ability of the Logistic Regression model
predict.result <- predict(log_model, newdata = training, type = 'response')
testing$Exited <- ifelse(predict.result > 0.5, 1, 0)
misClasificError <- mean(fitted.results != testing$Exited)
print(paste('Precisão da Regressão Logística',1-misClasificError))

fitted.results <- predict(log_model, newdata = testing, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Exited)
print(paste('Precisão da Regressão Logística',1-misClasificError))

print("Matrix Confusa de Regressão Logística")
table(testing$Exited, fitted.results > 0.5)

'no total foram 223 sim e 2777 não churn,
true positive (135): previsão de churn e que realmente aconteceu
true negative (101): previsão de churn e não aconteceu
false positive (506): previsão de não churn e aconteceu churn
true positive (2258): previsão de não churn e não aconteceu o churn'

# ODDS ratio - chances de um evento acontecer
exp(cbind(OR = coef(log_model), confint(log_model)))

## DECISION TREE + transformar factor(Exited) ##
'Decision Tree (Árvore da Decisão) é uma forma auxiliar a decisão por meio de gráficos ou modelos, incluindo, também, as consequências das escolhas. 
Uma árvore de decisão possui uma estrutura semelhante a um fluxograma, espelhando um algoritmo que contém apenas instruções condicionais, no qual cada nó representa um teste em um atributo, cada ramo
é um resultado do teste e as folhas, os rótulos das classes. O caminho da raiz para a folha, representa, assim, as regras de classificação. Os algoritmos de aprendizado baseados em árvore são considerados 
um dos melhores e mais usados métodos de aprendizado supervisionados. Os métodos baseados em árvores permitem modelos preditivos com alta precisão, estabilidade e facilidade de interpretação. '
'Para testar o modelo, optou-se por utilizar as variáveis com mais significância estatística nos testes anteriores para elaborar o fluxograma, sendo elas Geography (Geografia), Gender (Gênero) e Age (Idade).'


training$Exited <- factor(training$Exited, labels = c("Não", "Sim"), levels = 0:1)
testing$Exited <- factor(testing$Exited, labels = c("Não", "Sim"), levels = 0:1)

tree_model <- ctree(Exited ~ Geography+Gender+Age, training)
plot(tree_model)

prediction_matrix_tree <- predict(tree_model, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = prediction_matrix_tree, Actual = testing$Exited)

p1 <- predict(tree_model, training)
tab1 <- table(Predicted = p1, Actual = training$Exited)
tab2 <- table(Predicted = prediction_matrix_tree, Actual = testing$Exited)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


## RANDOM FOREST
random_forest_model <- randomForest(Exited ~., data = training)
print(random_forest_model)

prediction_rf <- predict(random_forest_model, testing)
#caret::confusionMatrix(pred_rf, testing$Churn)
table(Predicted = prediction_rf, Actual = testing$Exited)
plot(random_forest_model)

# modelo após ajuste
rfModel_new <- randomForest(Exited ~., data = training, ntree = 400,
                            mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Exited)
table(Predicted = pred_rf_new, Actual = testing$Exited)

varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

# BEST MODEL: LOGISTIC REGRESSION #

# Aplicação do modelo de regressão logística treinado, com melhor acurácia, no dataset de teste.
predict_results <- predict(log_model, newdata = data_testing, type = 'response')
data_testing$Exited <- ifelse(predict_results > 0.5, 1, 0)
predict_results_test <- data_testing[, c(0, 14)]
names(predict_results_test)[1] <- "predicValues"
print(predict_results_test)

# Converter tibble em csv

predict_exited <- write.csv(data,"abandono_teste_output.csv")

