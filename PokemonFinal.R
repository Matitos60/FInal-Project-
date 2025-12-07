library(readr)
pokemon <- read_csv("Downloads/pokemon.csv")
View(pokemon)

num_pokemon <- pokemon[sapply(pokemon, is.numeric)]
cor(num_pokemon)

numeric_data = pokemon[, sapply(pokemon, is.numeric)]
correlation_matrix = cor(numeric_data)
library(corrplot)
corrplot(correlation_matrix, method = "circle", tl.col = "blue")




library(readr)
Pokemon_1_ <- read_csv("Downloads/Pokemon (1).csv")
View(Pokemon_1_)

numeric_data = Pokemon_1_[, sapply(Pokemon_1_, is.numeric)]
correlation_matrix = cor(numeric_data)
library(corrplot)
corrplot(correlation_matrix, method = "circle", tl.col = "blue")


library(readr)
PokemonData <- read_csv("Downloads/PokemonData.csv")
View(PokemonData)
jit_Pokemon <- PokemonData[sapply(PokemonData, is.numeric)]
cor(jit_Pokemon)

numeric_data = PokemonData[, sapply(PokemonData, is.numeric)]
correlation_matrix = cor(numeric_data)
library(corrplot)
corrplot(correlation_matrix, method = "circle", tl.col = "blue")
library(caret)
library(ggplot2)


inTrain <- createDataPartition(y = PokemonData$Legendary, p = 0.7, list = FALSE)

# Split into training and test sets
train <- PokemonData[inTrain, ]
test  <- PokemonData[-inTrain, ]

# Pull out X (predictors) and y (target) for training
X_train <- train[, c("Spa", "Spd")]
y_train <- train$Legendary

# Pull out X and y for test
X_test  <- test[, c("Spa","Spd")]
y_test  <- test$Legendary

# Load class library
library(class)

# Run KNN with k = 3
Evo_prediction <- knn(train = X_train, test = X_test, cl =y_train, k = 3)

# Confusion matrix
confusion_matrix <- table(Predicted = Evo_prediction, Actual = y_test)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy * 100, "%\n")

# Check correlations among numeric variables
numeric_data <- PokemonData[, sapply(PokemonData, is.numeric)]
cor(numeric_data)


ggplot(PokemonData) + geom_point(aes(BST,Att)) + 
  geom_smooth(aes(BST,Att), method="lm", se=F) + 
  labs(x="BST", y=" Att")







