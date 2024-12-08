# text (development version)


<!-- README.md is generated from README.Rmd. Please edit that file -->
# topics 0.30.4
- scaling controls with scale instead of manually resulting in sligthtly different estimates. (but still same p-value and t-values)



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





