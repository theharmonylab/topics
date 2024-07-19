# TODO: Put the dot plot function in the main to plot every category. Just some ggplot.

rm(list=ls())
gc()

library(topics)
dtmtest <- topicsDtm(
  data = topics::data$harmonywords
)

model = topicsModel(dtmtest,num_topics = 50)

preds = topicsPreds(
  model = model, 
  data = Language_based_assessment_data_3_100$harmonywords
)


tests <- topicsTest(
  model = model,
  preds = preds,
  data =  Language_based_assessment_data_3_100,
  pred_var_x = 'hilstotal',
  pred_var_y = 'swlstotal'
)

topicsPlot(model = model,
           test = tests,
           grid_plot = TRUE,
           p_threshold = 0.99,
           dim = 1
           ,seed = 42)

topicsPlot(model = model,
           test = tests,
           grid_plot = TRUE,
           p_threshold = 0.99,
           dim = 2
           ,seed = 42)

topicsPlot(model = model,
           test = tests,
           grid_plot = TRUE,
           dim = 3
           ,seed = 42)

tests <- topicsTest(
  model=model,
  preds = preds,
  data =  Language_based_assessment_data_3_100,
  pred_var_x = 'hilstotal',
  pred_var_y = NULL
)

topicsPlot(model = model,
           test = tests,
           grid_plot = FALSE,
           p_threshold = 0.99,
           seed = 42)
