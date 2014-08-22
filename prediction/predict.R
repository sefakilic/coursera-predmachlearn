# Load model and test data (model and test_data)
load("rf_model.RData", verbose=TRUE)
load("test_data.RData", verbose=TRUE)

# Function to write predictions to separate files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Prediction
library(randomForest)
library(caret)
answers <- predict(model, test_data)

# write answers to files
pml_write_files(answers)
