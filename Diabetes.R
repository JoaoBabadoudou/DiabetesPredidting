library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(neuralnet)

# Importons nos donnees
df <- read_delim("diabetes.csv", delim = ";", 
                       escape_double = FALSE, col_types = cols(Outcome = col_factor(levels = c("0", 
                                                                                               "1"))), trim_ws = TRUE)
View(df)

#### Analyse descriptive
# Verifions s'il y a des valeurs manquantes
sum(is.na(df))
# Statistique sescriptive
summary(df)
str(df)

c2    <-    ggplot(df,    aes(Pregnancies))    +
  geom_bar(fill    =    "#104E8B")   +
  ggtitle(names(df[1]))

d2    <-    ggplot(df,    aes(Glucose))    +
  geom_histogram( fill    =    "#104E8B")   +
  ggtitle(names(df[,2]))

e2    <-    ggplot(df,    aes(BloodPressure) )   +
  geom_histogram( fill    =    "#104E8B" )      +    ggtitle(names(df[,3]))

f2    <-    ggplot(df,    aes(SkinThickness))    +
  geom_histogram( fill    =    "#104E8B")    + 
  ggtitle(names(df[,4]))

g2    <-    ggplot(df,    aes(Insulin))    +
  geom_histogram( fill    =    "#104E8B")    +    ggtitle(names(df[,5]))

h2    <-    ggplot(df,    aes(BMI))    +
  geom_histogram(fill    =    "#104E8B")     +
  ggtitle(names(df[,6]))

i2    <-    ggplot(df,    aes(DiabetesPedigreeFunction))    +
  geom_histogram(fill    =    "#104E8B")    +   ggtitle(names(df[,7]))

j2    <-    ggplot(df,    aes(Age))    +
  geom_histogram(fill    =    "#104E8B")    + 
  ggtitle(names(df[,8]))


gridExtra::grid.arrange(c2,d2,e2,f2,g2,h2,i2,j2)



ggplot(df,aes(Outcome,Insulin))+ geom_boxplot(fill = "mediumpurple",colour = "black")

ggpairs(df, aes(colour = Outcome, alpha = 0.4))
 # traiter les outliers




########### Normaliser les donnees
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}

df[,2:8] <- apply(df[,2:8],MARGIN = 2 , FUN = normalize)

########## Partitionner les donnees

# Let's split our data, 80% for the training and 20% for testing
split_size	=	0.8
sample_size	=	floor(split_size	*	nrow(df)) 
set.seed(123)
train_indices	<-	sample(seq_len(nrow(df)),	size	=	
                          sample_size)
train	<-	df[train_indices,	] 
test	<-	df[-train_indices,	]


X_train <- train[,-9]
Y_train <-  train[,9]


X_test <- test[,-9]
Y_test <-  test[,9]


# #####
# model<- neuralnet(Outcome~., data = train)
# plot(model)
# model_results <- compute(model, test[1:8])
# predicted_strength <- model_results$net.result
# cor(predicted_strength, as.numeric( test$Outcome))
