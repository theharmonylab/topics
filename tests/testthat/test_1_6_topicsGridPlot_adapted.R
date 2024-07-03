rm(list=ls())
gc()

library(text)
library(topics)
dtmtest <- topicsDtm(
  data = Language_based_assessment_data_3_100$harmonywords
)

model = topicsModel(dtmtest)

preds = topicsPreds(
  model = model, 
  data = Language_based_assessment_data_3_100$harmonywords
)


tests <- topicsTest(
  model = model,
  preds = preds,
  data =  Language_based_assessment_data_3_100,
  pred_var_x = 'hilstotal',
  pred_var_y = NULL#'swlstotal'
)

# tests <- topicsTest(
#   model=model,
#   preds = preds,
#   data =  Language_based_assessment_data_3_100,
#   pred_var_x = 'hilstotal',
#   pred_var_y = NULL
# )

# topicsPlot(model = model,
#            test = tests,
#            dim = 1
#            ,seed = 42)
# topicsPlot(model = model,
#                  test = tests,
#                  dim = 2
#                  ,seed = 42)
# topicsPlot(model = model,
#            test = tests,
#            dim = 3
#            ,seed = 42)

# topicsPlot(model = model,
#            test = tests,
#            grid = TRUE,
#            dim = 1
#            ,seed = 42)
topicsPlot(model = model,
           test = tests,
           grid = FALSE,
           p_threshold = 0.99,
           #dim = 1,
           seed = 42)

# topicsPlot(model = model,
#            test = tests,
#            dim = 3
#            ,seed = 42)

# topicsPlot(model = model,
#            test = tests,
#            grid = FALSE,
#            ,seed = 42)
