
# Before sourcing, consider setting this option (can increase 5000); 
#  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")
library(ldatext)

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


model1 <- ldaModel(dtm = dtm,
                  num_topics = 10,
                  num_iterations = 100)

model2 <- ldaModel(dtm = dtm,
                   num_topics = 10,
                   num_iterations = 100)
names(model2)
setequal(model1$instances, model2$instances)
setequal(model1$inferencer, model2$inferencer)
setequal(model1$top_terms_mallet, model2$top_terms_mallet) # TRUE
setequal(model1$top_terms, model2$top_terms) # TRUE
setequal(model1$phi, model2$phi) # TRUE
setequal(model1$topic_docs, model2$topic_docs) # TRUE
setequal(model1$frequencies, model2$frequencies) # TRUE
setequal(model1$vocabulary, model2$vocabulary) # TRUE
setequal(model1$labels, model2$labels) # TRUE
setequal(model1$theta, model2$theta) # TRUE
setequal(model1$prevalence, model2$prevalence) # TRUE
setequal(model1$coherence, model2$coherence) # TRUE
setequal(model1$pred_model, model2$pred_model) # TRUE
setequal(model1$summary, model2$summary) # TRUE


# change so model can be used on new data 
# tests
# "topics"

preds <- ldaPreds(model = model,
                  dtm = dtm)



test_line <- ldaTest(model = model,
                preds = preds,
                dtm = dtm,
                pred_var = "minidep_diagnose",
                test_method = "linear_regression")


ldaWordclouds(model = model, 
              test = test_log)





