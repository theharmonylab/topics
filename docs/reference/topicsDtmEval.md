# Summarize and Visualize your Document Term Matrix

This function creates a frequency table of your DTM and generates up to
four plots for visualization

## Usage

``` r
topicsDtmEval(dtm)
```

## Arguments

- dtm:

  (R_obj) The document term matrix -\> output of topicsDtm function

## Value

A named list containing:

- dtm_summary:

  A dataframe of terms and their frequencies.

- frequency_plot:

  A bar plot of all term frequencies with example terms.

- frequency_plot_30_least:

  A bar plot of the 30 least frequent terms (if numer of terms \> 30).

- frequency_plot_30_most:

  A bar plot of the 30 most frequent terms (if numer of terms \> 30).

- histogram_of_frequencies:

  A histogram of term frequencies (this is the same information as in
  the frequency_plot but presented differently).
