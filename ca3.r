#---------------------------------- Libraries ----------------------------------
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)
#---------------------------- Load and Prepare Data ----------------------------
data <- read.csv("C:\\Users\\bavis\\Downloads\\heart_disease.csv")
data$target <- as.factor(data$target)  # Convert target to factor for classification tasks
#------------------------------- DECISION TREE ---------------------------------
# Splitting the dataset into training and testing sets
set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[pd == 1, ]
test <- data[pd == 2, ]
# Train decision tree model
tree_rpart <- rpart(target ~ age + trestbps + chol + thalach + oldpeak, data = train)
rpart.plot(tree_rpart)
#---------------------------- LINEAR REGRESSION --------------------------------
# Linear regression on continuous variables
x <- data$age
y <- data$trestbps
# Build the linear regression model
relation <- lm(y ~ x)
summary(relation)
# Predicting resting blood pressure based on age
new_age <- data.frame(x = c(40, 50, 60, 70))
predicted_bp <- predict(relation, new_age)
print(predicted_bp)
# Plot the regression results
ggplot(data, aes(x = age, y = trestbps)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression: Resting BP vs Age",
       x = "Age",
       y = "Resting BP")
#---------------------------------- SVM -----------------------------------------
# Train SVM model
svm_model <- svm(target ~ age + trestbps + chol + thalach + oldpeak, data = train, kernel = "linear", cost = 1, scale = TRUE)
summary(svm_model)
# Predict and evaluate SVM model
svm_predictions <- predict(svm_model, test)
confusion_matrix <- table(Predicted = svm_predictions, Actual = test$target)
print(confusion_matrix)
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy)
# Plot SVM decision boundaries (simplified example)
ggplot(train, aes(x = age, y = thalach, color = target)) +
  geom_point() +
  labs(title = "SVM Decision Boundary (Age vs Thalach)",
       x = "Age",
       y = "Max Heart Rate (Thalach)") +
  theme_minimal()
#---------------------------- K-MEANS CLUSTERING -------------------------------
# Selecting features and normalizing for clustering
data_selected <- data[, c("age", "trestbps", "chol", "thalach", "oldpeak")]
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
data_norm <- as.data.frame(lapply(data_selected, normalize))
# Determine optimal clusters using the Elbow Method
set.seed(123)
wss <- sapply(1:10, function(k) { kmeans(data_norm, centers = k, nstart = 10)$tot.withinss })
# Plot the Elbow Method
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method for Optimal K")

