# Libraries and Packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tibble")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("MASS")
install.packages("caret")
install.packages("rpart")
install.packages("rpartplot")
install.packages("rminer")
install.packages("nnet")
install.packages("Amelia")
install.packages("caTools")
install.packages("dummies")
install.packages("stats")

library(tibble)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(MASS)
library(caret)
library(rpart)
library(rpartplot)
library(rminer)
library(nnet)
library(Amelia)
library(caTools)
library(dummies)
library(dplyr)

# Criar diretório e salvar arquivos para análise
getwd()
setwd("/home/stacke/Documentos/Desafio_Indicium")
getwd()

# Importar e Carregar a base de dados de Churn
data_training <- read.csv("Abandono_clientes.csv", header = TRUE)

##### Análise Exploratória ######
'Num primeiro momento, é importante analisar os dados de forma exploratória para entender quais variáveis temos e suas dimensões.' 

dim(data_training)
names(data_training)

'Assim, temos 13 variáveis (colunas) e 10000 casos de variáveis (linhas). Há variáveis como Gender (Gênero) e Tenure (Tempo de Permanência). 
O output que queremos prever é a variável Churn (abandono).'

data_training <- tibble(data_training)
class(data_training)
head(data_training)
print(data_training)

str(data_training) # structure of the data
print(summary(data_training)) # undesrtanding of the data
'Tabelar todas as variaveis e características. A coluna Exited é o target, objetivo.'

## ETL - Estruturando os Dados ## 
'Explicar o que é ETL. 
O processo de limpeza e tratamento de dados é tão importante quanto o de modelagem, já que pode evitar retrabalhos futuros e melhores modelagens.
O primeiro passo é alterar as variáveis de caracteres (categóricas) para fatores, pois estas variáveis são armazenadas em um fator. Geralmente, 
as variáveis categóricas são alocadas em 0, 1, 2, 3, com documentação adicional para explicar o que cada número significa. 
Claramente, essa não é a forma mais simples e eficiente de descrever estatisticamente as variáveis categóricas, justificando o armazenamento em um vetor de caracteres, ou seja, fator.
As classes dos objetos em R são críticas para o desempenho.'

'1. Transformar os casos de variáveis categóricas de 0 e 1 para Não e Sim. É importante informar que as variáveis HasCrCard, IsActiveMember e Exited são variáveis categóricas e não numéricas, por isso
o uso do comando factor(). Assim, redefinem-se as variáveis com os rótulos labels() associados aos níveis (levels).'


data_training$HasCrCard <- factor(data_training$HasCrCard, label = c("Não", "Sim"), levels = 0:1)
data_training$IsActiveMember <- factor(data_training$IsActiveMember, labels = c("Não", "Sim"), levels = 0:1)
data_training$Exited <- factor(data_training$Exited, labels = c("Não", "Sim"), levels = 0:1)
print(data_training)

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

# Dividir a base de dados 

library(caret)

# selecting random seed to reproduce results
set.seed(5)

# sampling 75% of the rows
inTrain <- createDataPartition(y = data_training$Exited, p=0.75, list=FALSE)

# train/test split; 75%/25%
train <- data_training[inTrain,]
test <- data_training[-inTrain,]

##### Estatística Descritiva #####

## Análise Univariada
'Foram classificadas as variáveis quanto a seus tipos: qualitativas (nominal ou ordinal) ou quantitativa (discreta ou contínua). 
A fim de resumir a distribuição das variáveis veremos gráficos, tabelas e/ou outras medidas.'

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
salary_graphic



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

##### MODELAGEM #####

'Para simular um experimento com o objetivo de prever se os clientes vão dar “churn”, precisamos trabalhar com um banco de dados 
particionado, seguindo a metodologia de validação cruzada (cross validation). O banco de dados tem duas partes, uma será o conjunto 
de treinamento que será usado para criar o modelo, e a segunda parte será o conjunto de testes que será usado para avaliar o nosso 
modelo. Como recebi dois datasets já preparados para treinamento e teste, não precisarei dividir o banco de dados.'


'Logistic Regression - é um modelo de classificação linear e como estamos lidando com previsão de abandono de clientes ou não,
um modelo de classificação é o ideal. O modelo de regressão logística é o melhor que podemos interpretar porque podemos verificar
com mais facilidade a relação entre as features e os outputs, a escolha de um modelo é considerado uma das boas práticas de Machine Learning.
A desvantagem da regressão logística é que ela tem um viés no sentido de ajustes lineares. Se o limite de decisão não for linear, talvez não funcione tão bem quanto um modelo como o Random Forest.'
'Tanto problemas de classificação quanto de regressão são modelos preditivos, mas utilizou-se pelo modelo de classificação pois, 
neste caso, a variável resposta é de natureza qualitativa (churn sim ou não).'

# Dimensões dos DataSets: data_training e data_test

dim(data_training); dim(data_testing)

# fitting the model
'Para implementar um modelo de regressão logística utilizarei a função de modelos lineares generalizados (GLM). Existem diferente tipos
de regressão logística linear, mas será usada, aqui, o argumento family = binomial, pois fornece estimativas pontuais dos parâmetros, erros
padrão, p-valores de Wald entre outras informações.'

log_model <- glm(Exited~., data = churn, family = binomial(link="logit"))
print(summary(log_model))

'As principais variáveis que explicam o modelo são as que possuem o menor p-valor, sendo estatisticamente mais significantes: 
GeographyGermany, GenderMale, Age, Balance e IsActiveMemberSim.'

anova(log_model, test="Chisq")

"Podemos notar a queda no desvio ao adicionar uma variável de cada vez. Geography, Gender, Age e IsActiveMember reduziram significativamente
o desvio, enquanto Balance não parece ter melhorado o modelo com seu desvio de 36.02, mesmo tendo um p-valor baixo."

# Assessing the predictive ability of the Logistic Regression model

data_testing$Exited <- as.character(data_testing$Exited)
data_testing$Exited[data_testing$Exited == "No"] <- "0"
data_testing$Exited[data_testing$Exited == "Yes"] <- "1"
fitted.results <- predict(log_model, newdata = data_testing, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != data_testing$Exited)
print(paste('Precisão da Regressão Logística',1-misClasificError))


print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)

# making predictions
churn.probs <- predict(log_model, data_testing, type = "response")
head(churn.probs)

# Looking at the response encoding
contrasts(df$Churn)Copy

contrasts(df$Churn)





