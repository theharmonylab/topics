# remove.packages("topics")
# devtools::document()
# devtools::build()

rm(list=ls())
gc()

library(topics)

dtmtest <- topicsDtm(
  data = topics::data$harmonytexts
)

model <- topicsModel(dtmtest)

preds <- topicsPreds(
  model = model, 
  data = topics::data$harmonywords
)


tests <- topicsTest(
  model = model,
  preds = preds,
  data =  topics::data,
  pred_var_x = 'hilstotal',
  pred_var_y = 'swlstotal'
)

tests <- topicsTest(
  model = model,
  preds = preds,
  data =  topics::data,
  pred_var_x = 'hilstotal',
  pred_var_y = 'swlstotal',
  control_vars = c('age','gender')
)


topicsPlot(model = model,
           test = tests,
           grid_plot = FALSE,
           p_threshold = 0.99
           ,seed = 42)

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
           p_threshold = 0.99,
           dim = 2,
           way_popout_topics = 'max_y',
           seed = 42)

topicsPlot(model = model,
           test = tests,
           grid_plot = TRUE,
           p_threshold = 0.99,
           dim = 2,
           user_spec_topics = c('t_1', 't_20'),
           seed = 42)

topicsPlot(model = model,
           test = tests,
           grid_plot = TRUE,
           dim = 3
           ,seed = 42)

tests <- topicsTest(
  model=model,
  preds = preds,
  data =  topics::data,
  pred_var_x = 'hilstotal',
  pred_var_y = NULL
)

topicsPlot(model = model,
           test = tests,
           grid_plot = FALSE,
           p_threshold = 0.99,
           seed = 42)
