# N-grams

The function computes ngrams from a text

## Usage

``` r
topicsGrams(
  data,
  ngram_window = c(1, 3),
  stopwords = stopwords::stopwords("en", source = "snowball"),
  occurance_rate = 0,
  removal_mode = "frequency",
  removal_rate_most = NULL,
  removal_rate_least = NULL,
  pmi_threshold = 0,
  top_frequent = 200
)
```

## Arguments

- data:

  (tibble) The data

- ngram_window:

  (list) the minimum and maximum n-gram length, e.g. c(1,3)

- stopwords:

  (stopwords) the stopwords to remove, e.g. stopwords::stopwords("en",
  source = "snowball")

- occurance_rate:

  (numerical) The occurance rate (0-1) removes words that occur less
  then in (occurance_rate)\*(number of documents). Example: If the
  training dataset has 1000 documents and the occurrence rate is set to
  0.05, the code will remove terms that appear in less than 50
  documents.

- removal_mode:

  (character) The mode of removal, either "term", frequency" or
  "percentage"

- removal_rate_most:

  (numeric) The rate of most frequent ngrams to remove

- removal_rate_least:

  (numeric) The rate of least frequent ngrams to remove

- pmi_threshold:

  (integer) The pmi threshold, if it shall not be used set to 0

- top_frequent:

  (integer) The number of most frequently occuring ngrams to included in
  the output.

## Value

A list containing tibble of the ngrams with the frequency and
probability and a tibble containing the relative frequency of the ngrams
for each user
