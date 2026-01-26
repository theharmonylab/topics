# Test topics or n-grams

Statistically test topics or n-grams in relation to one or two other
variables using regression or t-test.

## Usage

``` r
topicsTest(
  data,
  model = NULL,
  preds = NULL,
  ngrams = NULL,
  x_variable = NULL,
  y_variable = NULL,
  controls = c(),
  test_method = "default",
  p_adjust_method = "fdr",
  complete_cases = FALSE,
  seed = 42
)
```

## Arguments

- data:

  (tibble) The tibble containing the variables to be tested.

- model:

  (list) A trained model LDA-model from the topicsModel() function.

- preds:

  (tibble) The predictions from the topicsPred() function.

- ngrams:

  (list) Output of the n-gram function.

- x_variable:

  (string) The x variable name to be predicted, and to be plotted (only
  needed for regression or correlation).

- y_variable:

  (string) The y variable name to be predicted, and to be plotted (only
  needed for regression or correlation).

- controls:

  (vector) The control variables (not supported yet).

- test_method:

  (string) The test method to use. "default" checks if x_variable and
  y_variable only contain 0s and 1s, for which it applies logistic
  regression; otherwise it applies linear regression. Alternatively, if
  only specifying x_variable, the user may manually specify either
  "linear_regression" or "logistic_regression".

- p_adjust_method:

  (character) Method to adjust/correct p-values for multiple comparisons
  (default = "fdr"; see also "holm", "hochberg", "hommel", "bonferroni",
  "BH", "BY", "none").

- complete_cases:

  If TRUE, it will only use complete cases for x_variable, y_variable,
  controls, and preds.

- seed:

  (integer) The seed to set for reproducibility

## Value

A list of the test results, test method, and prediction variable.

## Examples

``` r
# \donttest{
# Test the topic document distribution in respect to a variable

dtm <- topicsDtm(
  data = dep_wor_data$Depphrase)

model <- topicsModel(
  dtm = dtm, # output of topicsDtm()
  num_topics = 20,
  num_top_words = 10,
  num_iterations = 1000,
  seed = 42)
                     
preds <- topicsPreds(
 model = model, # output of topicsModel()
 data = dep_wor_data$Depphrase)
                     
test <- topicsTest(
  model = model, # output of topicsModel()
  data=dep_wor_data,
  preds = preds, # output of topicsPreds()
  test_method = "linear_regression",
  x_variable = "Age")
#> 1: fitting model formula: z_Age ~ z_t_1 
#> 
#> 2: fitting model formula: z_Age ~ z_t_2 
#> 
#> 3: fitting model formula: z_Age ~ z_t_3 
#> 
#> 4: fitting model formula: z_Age ~ z_t_4 
#> 
#> 5: fitting model formula: z_Age ~ z_t_5 
#> 
#> 6: fitting model formula: z_Age ~ z_t_6 
#> 
#> 7: fitting model formula: z_Age ~ z_t_7 
#> 
#> 8: fitting model formula: z_Age ~ z_t_8 
#> 
#> 9: fitting model formula: z_Age ~ z_t_9 
#> 
#> 10: fitting model formula: z_Age ~ z_t_10 
#> 
#> 11: fitting model formula: z_Age ~ z_t_11 
#> 
#> 12: fitting model formula: z_Age ~ z_t_12 
#> 
#> 13: fitting model formula: z_Age ~ z_t_13 
#> 
#> 14: fitting model formula: z_Age ~ z_t_14 
#> 
#> 15: fitting model formula: z_Age ~ z_t_15 
#> 
#> 16: fitting model formula: z_Age ~ z_t_16 
#> 
#> 17: fitting model formula: z_Age ~ z_t_17 
#> 
#> 18: fitting model formula: z_Age ~ z_t_18 
#> 
#> 19: fitting model formula: z_Age ~ z_t_19 
#> 
#> 20: fitting model formula: z_Age ~ z_t_20 
#> 
#> The parameter y_variable is not set! Output 1 dimensional results.
# }                 
```
