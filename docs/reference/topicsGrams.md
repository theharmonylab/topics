# N-grams

Compute n-grams and per-document relative frequencies from text.

## Usage

``` r
topicsGrams(
  data,
  ngram_window = c(1, 3),
  stopwords = stopwords::stopwords("en", source = "snowball"),
  removalword = "",
  pmi_threshold = NULL,
  occurance_rate = 0,
  removal_mode = "frequency",
  removal_rate_most = NULL,
  removal_rate_least = NULL,
  shuffle = TRUE,
  lower = TRUE,
  remove_punctuation = TRUE,
  remove_numbers = TRUE,
  stem_lemma_function = NULL,
  verbose = FALSE,
  seed = 42L,
  threads = 1,
  top_frequent = NULL,
  freq_per_user_format = c("auto", "wide", "long"),
  max_wide_cells = 5e+07
)
```

## Arguments

- data:

  A character vector of texts or a data.frame/tibble (first column used
  as text).

- ngram_window:

  Integer vector specifying n-gram sizes. If length 1, only that n is
  used (e.g., `2`). If length 2 and increasing, it is interpreted as an
  inclusive range (e.g., `c(1, 3)` -\> 1,2,3). Otherwise it is treated
  as an explicit set (e.g., `c(1, 3)` means only 1 and 3).

- stopwords:

  Character vector of stopwords to remove (e.g.,
  `stopwords::stopwords("en", source = "snowball")`).

- removalword:

  Character vector of words to remove from the raw text prior to
  tokenization.

- pmi_threshold:

  Numeric PMI threshold used to filter multi-word n-grams. If `NULL`,
  PMI filtering is skipped.

- occurance_rate:

  Numeric in \[0,1\]. Terms are removed if they occur in fewer than
  `round(N_docs * occurance_rate) - 1` documents, where `N_docs` is the
  number of texts. Example: with 1000 documents and
  `occurance_rate = 0.05`, terms occurring in fewer than ~50 documents
  are removed.

- removal_mode:

  Character. Removal mode passed to `filter_ngrams()`. Common options
  include `"frequency"` and `"percentage"` (depending on your helper
  implementation).

- removal_rate_most:

  Numeric. Rate/threshold for removing the most frequent n-grams
  (interpreted by `filter_ngrams()` according to `removal_mode`).

- removal_rate_least:

  Numeric. Rate/threshold for removing the least frequent n-grams
  (interpreted by `filter_ngrams()` according to `removal_mode`).

- shuffle:

  Logical. If `TRUE`, texts are shuffled (seed-controlled) before
  processing. The original ids are preserved in output.

- lower:

  Logical. If `TRUE`, text is lowercased prior to tokenization.

- remove_punctuation:

  Logical. Passed to `quanteda::tokens(remove_punct = ...)`.

- remove_numbers:

  Logical. Passed to `quanteda::tokens(remove_numbers = ...)`.

- stem_lemma_function:

  Optional function applied to tokens (via `quanteda::tokens_apply()`).
  Should accept and return a character vector of tokens.

- verbose:

  Logical. If `TRUE`, prints simple progress messages (if implemented).

- seed:

  Integer. Random seed used when `shuffle = TRUE`.

- threads:

  Integer. Number of threads used by quanteda via
  `quanteda_options(threads = ...)`.

- top_frequent:

  Integer or `NULL`. If set, keeps only the `top_frequent` most frequent
  n-grams after filtering (recommended for large corpora). If `NULL`,
  keeps all retained n-grams.

- freq_per_user_format:

  Output format for `freq_per_user`:

  `"wide"`

  :   Wide document-by-ngram table (may be large).

  `"long"`

  :   Long sparse table with columns `id`, `ngram`, `rel_freq`.

  `"auto"`

  :   Uses wide format only if `N_docs * N_ngrams <= max_wide_cells`;
      otherwise returns long.

- max_wide_cells:

  Numeric. Safety limit for `"auto"`: if `N_docs * N_ngrams` exceeds
  this, `freq_per_user` is returned in long format to avoid dense
  allocation and excessive memory use.

## Value

A named list with:

- settings:

  A named list of settings used to generate the results (for
  reproducibility).

- n_grams_pmi:

  PMI information if available from your PMI helper, otherwise a
  message.

- ngrams:

  A tibble of retained n-grams and statistics (e.g., `freq`,
  `prevalence`, `num_docs`).

- freq_per_user:

  Per-document relative frequencies, either wide or long depending on
  `freq_per_user_format`.

- stats:

  A tibble summarizing how many n-grams were removed by each filtering
  step, by n-gram size.

## Details

This function extracts n-grams using and returns: (1) a tibble of
n-grams with frequency/prevalence and document frequency, (2)
per-document relative frequencies for the retained n-grams (wide or long
format), (3) filtering statistics, (4) a named list of settings (for
reproducibility).
