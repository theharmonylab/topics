
# Before sourcing, consider setting this option (can increase 5000); 
#  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")


library(ldatext)

exists("ldaDtm")

data <- read.csv("depression_anxiety_cleaned.csv")
colnames(data)
dtm <- ldaDtm(data = data, 
              id_col = "unique_id", 
              data_col = "dep_all_phrases", 
              stopwords = stopwords::stopwords("en", source = "snowball"))

# Checking the results from the dtm
length(Matrix::colSums(dtm$train_dtm))
Matrix::colSums(dtm$train_dtm)[1:20]
Matrix::colSums(dtm$train_dtm)[(length(Matrix::colSums(dtm$train_dtm)) - 100):length(Matrix::colSums(dtm$train_dtm))]


model <- ldaModel(dtm = dtm,
                  num_topics = 10,
                  num_iterations = 1000)
preds <- ldaPreds(model = model,
                  dtm = dtm)

test <- ldaTest(model = model,
                preds = preds,
                dtm = dtm,
                pred_var = "PHQtot",
                test_method = "logistic_regression")

ldaWordclouds(model = model, 
              test = test)
