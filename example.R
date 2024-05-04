
# Before sourcing, consider setting this option (can increase 5000); 
#  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")


library(ldatext)

exists("ldaDtm")

data <- read.csv("depression_anxiety_cleaned.csv")

dtm <- ldaDtm(data = data, 
              id_col = "unique_id", 
              data_col = "dep_all_phrases", 
              stopwords = stopwords::stopwords("en", source = "snowball"))

# Checking the results from the dtm
length(colSums(dtm$train_dtm))
colSums(dtm$train_dtm)[1:20]
colSums(dtm$train_dtm)[(length(colSums(dtm$train_dtm)) - 100):length(colSums(dtm$train_dtm))]


model <- ldaModel(dtm = dtm,
                  num_topics = 10,
                  num_iterations = 1000)
preds <- ldaPreds(model = model,
                  dtm = dtm)

test <- ldaTest(model = model,
                preds = preds,
                dtm = dtm,
                pred_var = "PHQtot",
                test_method = "linear_regression")

ldaWordclouds(model = model, 
              test = test)
