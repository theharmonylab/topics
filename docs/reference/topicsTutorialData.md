# Download and Prepare Tutorial Data

Downloads the Big5 Essays dataset from Hugging Face, filters by word
count, and returns a sample of a specified size.

## Usage

``` r
topicsTutorialData(
  sample_size = 1000,
  min_word_count = 250,
  max_word_count = 750,
  seed = 42,
  verbose = TRUE
)
```

## Arguments

- sample_size:

  (integer) The number of rows to return. Default is 1000.

- min_word_count:

  (integer) Minimum words per essay. Default is 250.

- max_word_count:

  (integer) Maximum words per essay. Default is 750.

- seed:

  (integer) Seed for reproducibility. Default is 42.

- verbose:

  (boolean) Whether to print status messages. Default is TRUE.

## Value

A processed tibble ready for topic modeling.
