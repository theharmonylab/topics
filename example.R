source("./src/main.R")
ls()
exists("ldaDtm")
getwd()
data <- read.csv("depression_anxiety_cleaned.csv")
dtm <- ldaDtm(data=data, 
              id_col="unique_id", 
              data_col="dep_all_phrases", 
              stopwords = stopwords::stopwords("en", source = "snowball"))


model <- ldaModel(dtm=dtm,
                  num_topics=10,
                  num_iterations=1000)
preds <- ldaPreds(model=model,
                           dtm=dtm)
test <- ldaTest(model=model,
                preds=preds,
                dtm=dtm,
                pred_var="PHQtot",
                test_method="ridge_regression")
ldaWordclouds(model=model, test=test)
