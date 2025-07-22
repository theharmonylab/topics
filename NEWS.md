# text (development version)
# topics .60
- ready for CRAN and instllation harmonized with the text-package. 

# topics 0.54
- topicsGrams() now uses exact word boundary matching for n-grams (e.g., "lack" is matched 
as a standalone word, excluding partial matches like "black" or "lacking").
- added ability to handle NAs in topicsTest(). 

# topics 0.51
- adding function to plot circles in the scatter legend. 
- fixing where non-significant plots were the same.
- improving the structure of the creat_plot help function. 
- moving rJava to suggest to enable compatibility with the text-package.

# topics 0.40.6
- addting `scatter_legend_dots_alpha` and `scatter_legend_bg_dots_alpha` parameters for the `topicsPlot()` function. 
- adding setting for having the dot sizes according to their prevalence. 

# topics 0.40.5
- Fixing bug when plotting test based on `logistic_regression`. 

# topics 0.40.3 - 0.40.4 
- added `occurance_rate` to `topicsGrams()`
- added `removal_mode`, `removal_rate_most` and `removal_rate_least` to `topicsGrams()`
- `ngram_window = c(1)` now supported by `topicsDtm()`
- legend added to `topicsPlot()` with ngrams
- The `size` in the dot legend will be based on `prevalence` if scatter_legend_dot_size = "prevalence". And the popouts are not transparent.
- Fix the issues of tick and label of the x-axis in 1-dim dot legend.
- Able to save the pop-out grey topics in the target folder.
- Fix the bugs of rounding in `generate_scatter_plot`.
- The default value of `highlight_topic_words` is set to `NULL` in the `topicsPlot()` function.

# topics 0.40.2
- changed some behaviours in `topicsGrams()`, including removing `top_n` and treating 
n-grams type differently.
- added `stopwords` function to `topicsGrams()`. 
- fixed the `pmi` calculation. 
- fixed the `ngrams_max` parameter in `topicsPlot()```.

# topics 0.40.1
- adding `allowed_word_overlap` in `topicsPlot()` for plotting the most prevalence.
- improving help texts
- `highlight_topic_words` parameter to add different colours for a word list. 
- added `stopwords` removal for `topicsGram()`.
- added `ngrams_max` functionality to `topicsPlot()`.

<!-- README.md is generated from README.Rmd. Please edit that file -->
# topics 0.40.
- removing `save_dir` and `load_dir` from all function; only `topicsPlot()` now has the `save_dir` as an option. 
- size of the dots in distributions can be plotted according to `prevalence`.
- adding `p_adjust_method` to `topicsPlots()`.

# topics 0.30.5
- plots are not added as a list (and not only saved to the folder)
- added `scatter_show_axis_values` to the `topcisPlot()`.
- adding feature to plot the `n_most_prevalent_topics`. 

# topics 0.30.4
- scaling controls with scale instead of manually resulting in slightly different estimates. (but still same p-value and t-values)
- removed ridge regression, t-test and correlation codes since they did not work
- removed automatic removal of NAs in the topics predictions (this should be handled explicitly).
- topicsTest() `default` to linear_regression if not the variable only contains 0s and 1s; i.e., now different tests can be applied to different axes. 

# topics 0.30.3
- saving settings in `dtm` for downstream use in other functions.
- adding parameters in the `topicsPred()` function including `num_iteration`, `sampling_interval`, `burn_in`.
- implemented `create_new_dtm` for creating a new `dtm` for new data 
- adding test for using `topics` dimension for training using `textTrainRegression()`. 
- removing forcing user to set save_dir on most functions (only need to do it for topics functions).

# topics 0.30.2
- fixing coherence bug
- showing prevalence and coherence for in results
- restructuring the files

# topics 0.30.0
- Harmonizing parameters in `topicsTest()` incl. x_variable, y_variable and controls
- fixing error that variable names cannot be names with 1 underscore.

# topics 0.22.1
- added `pmi_threshold` (experimental) to `topicsDtm()`
- removed the saving of raw data and the `split` procedure in the `topicsDtm()`
- adding function that name emphasized topics so the file name starts with 0_.
- add a parameter to turn off the shuffling of the data in `topicsDtm()` 

# topics 0.22
- change `p_threshold` to `p_alpha`
- moved `p_alpha` from the `topicsTest()` function to the `topicsPlots()` function
- removed unnecessary list items from `topicsTest()`

# topics 0.21
- Changes related to compatability with the `text`-package

# topics 0.20

- Cleaning up code and ensuring improved compatibility across platforms. 
- Started the journey of improving documentation.

# topics 0.10.1

## Change
- Removing dim and grid_plot arguments in `topicsPlot()`.
- Fixing the color bugs.
- Adding possibility for the user to use gradient colors in all plots.
- Adding a stop warning when the variable name contains an underscore in `topicsTest().`





