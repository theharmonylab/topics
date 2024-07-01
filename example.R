devtools::document()
rcmdcheck::rcmdcheck()
pkgdown::build_site(new_process = FALSE)
devtools::build()
# Before sourcing, consider setting this option (can increase 5000); 
#  without it the code may ran out of memory
options(java.parameters = "-Xmx5000m")
library(topics)
library(dplyr)
data <- Language_based_assessment_data_8 
View(data)
class(data["dep_all_phrases"])
dtm <- topicsDtm(data = data$dep_all_phrases, 
              #id_col = "unique_id", 
              #data_col = "harmonytexts", 
              stopwords = stopwords::stopwords("en", source = "snowball"))

dtm <- topicsDtm(load_dir = "./results")

# Checking the results from the dtm
length(Matrix::colSums(dtm$train_dtm))
Matrix::colSums(dtm$train_dtm)[1:20]
Matrix::colSums(dtm$train_dtm)[(length(Matrix::colSums(dtm$train_dtm)) - 100):length(Matrix::colSums(dtm$train_dtm))]


model1 <- topicsModel(dtm = dtm,
                  num_topics = 10,
                  num_iterations = 100)

model2 <- topicsModel(dtm = dtm,
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

length(data$harmonytexts)
temp1 <- data[["dep_all_phrases"]]
temp2 <- data["dep_all_phrases"]
temp2[,1] ==  temp1

preds <- topicsPreds(model = model1,
                  data=data$dep_all_phrases)
                  #id_col="unique_id",
                  #data_col="harmonytexts")


preds


test_line <- topicsTest(model = model1,
                data=data,
                preds = preds,
                pred_var = "age")

length(data)
View(test_line)


topicsPlot(model = model1, 
              test = test_line,
              p_threshold = 1,
              figure_format = "svg",
              color_negative_cor=ggplot2::scale_color_gradient(low = "darkgreen", high = "green"),
              color_positive_cor=ggplot2::scale_color_gradient(low = "darkred", high = "red"))
    




