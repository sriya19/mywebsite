set.seed(12)
model <- randomForest(Rings ~ ., data = abalone, importance = TRUE)
varImpPlot(model)

abalone_no_sex <- abalone[, !names(abalone) %in% c("Sex")]
index <- createDataPartition(abalone_no_sex$Rings, p = 0.8, list = TRUE)
train_data <- abalone_no_sex[index$Resample1,]
test_data <- abalone_no_sex[-index$Resample1,]
train_control <- trainControl(
  method = "cv", 
  number = 10   
)
models <- list()
models$rf <- train(Rings ~ ., data = abalone_no_sex, method = "rf", trControl = train_control)
train_control <- trainControl(method = "cv", number = 10, search = "grid")

tune_grid <- expand.grid(
  mtry = seq(2, ncol(abalone_no_sex) - 1, by = 1), 
  splitrule = c("variance"),
  min.node.size = c(5, 10)
)

set.seed(12)
tuned_model <- train(Rings ~ ., data = abalone_no_sex, method = "ranger",
                     trControl = train_control, tuneGrid = tune_grid,
                     metric = "RMSE")
library(caret)
library(FactoMineR)

#abalone_no_sex =abalone[, !names(abalone) %in% c("Sex")]
abalone_pca = abalone_no_sex[, !names(abalone) %in% c("Rings")]

data = abalone_pca  
data_scaled = scale(data)

pca_result = PCA(data_scaled, graph = FALSE)

num_components = which(cumsum(pca_result$eig[,2]) > 85)[1]
data_pca = pca_result$ind$coord[, 1:num_components]
num_components = which(cumsum(pca_result$eig[,2]) > 85)[1]
data_pca = pca_result$ind$coord[, 1:num_components]


final_data = data.frame(Rings = abalone_pca$Rings, data_pca)

set.seed(12)
index = createDataPartition(final_data$Rings, p = 0.8, list = TRUE)
train_data_pca = final_data[index$Resample1,]
test_data_pca = final_data[-index$Resample1,]

train_control_pca = trainControl(method = "cv", number = 10)
models_pca = list()
models_pca$rf = train(Rings ~ ., data = train_data_pca, method = "rf", trControl = train_control_pca)
models_pca$svm = train(Rings ~ ., data = train_data_pca, method = "svmRadial", trControl = train_control_pca)
models_pca$gbm = train(Rings ~ ., data = train_data_pca, method = "gbm", trControl = train_control_pca, verbose = FALSE)

# Linear Regression
models_pca$lm <- train(Rings ~ ., data = train_data_pca, method = "lm", trControl = train_control_pca)

predictions_rf_pca = predict(models_pca$rf, test_data_pca)
predictions_svm_pca = predict(models_pca$svm, test_data_pca)
predictions_gbm_pca = predict(models_pca$gbm, test_data_pca)
predictions_lm_pca = predict(models_pca$lm, test_data_pca)

rmse_rf_pca = RMSE(predictions_rf_pca, test_data_pca$Rings)
r2_rf_pca = R2(predictions_rf_pca, test_data_pca$Rings)

rmse_svm_pca = RMSE(predictions_svm_pca, test_data_pca$Rings)
r2_svm_pca = R2(predictions_svm_pca, test_data_pca$Rings)

rmse_gbm_pca = RMSE(predictions_gbm_pca, test_data_pca$Rings)
r2_gbm_pca = R2(predictions_gbm_pca, test_data_pca$Rings)

rmse_lm_pca = RMSE(predictions_lm_pca, test_data_pca$Rings)
r2_lm_pca = R2(predictions_lm_pca, test_data_pca$Rings)

cat("RF RMSE:", rmse_rf_pca, "R2:", r2_rf_pca, "\n")
cat("SVM RMSE:", rmse_svm_pca, "R2:", r2_svm_pca, "\n")
cat("GBM RMSE:", rmse_gbm_pca, "R2:", r2_gbm_pca, "\n")
cat("LM RMSE:", rmse_lm_pca, "R2:", r2_lm_pca, "\n")

library(shiny)

ui = fluidPage(
  titlePanel("Abalone Age Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("length", "Length", value = 0.5),
      numericInput("diameter", "Diameter", value = 0.4),
      numericInput("height", "Height", value = 0.1),
      numericInput("whole_weight", "Whole Weight", value = 0.5),
      numericInput("shucked_weight", "Shucked Weight", value = 0.2),
      numericInput("viscera_weight", "Viscera Weight", value = 0.1),
      numericInput("shell_weight", "Shell Weight", value = 0.15),
      actionButton("predict", "Predict Age")
    ),
    mainPanel(
      textOutput("age_prediction")
    )
  )
)

server = function(input, output) {
  observeEvent(input$predict, {
    new_data = data.frame(
      Length = input$length,
      Diameter = input$diameter,
      Height = input$height,
      Whole_weight = input$whole_weight,
      Shucked_weight = input$shucked_weight,
      Viscera_weight = input$viscera_weight,
      Shell_weight = input$shell_weight
    )
    
    predicted_age <- predict(models$rf, new_data)
    
    output$age_prediction <- renderText({
      paste("Predicted age:", round(predicted_age))
    })
  })
}

shinyApp(ui = ui, server = server)