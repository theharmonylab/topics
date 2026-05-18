# Combine topics and distribution legend into an overview figure (experimental)

Assembles a single composite figure from the individual topic word
clouds returned by \`topicsPlot()\`. The shape of the composition
depends on \`overview_plot_type\`:

- \`"topics_grid"\` / \`"textTopics"\`: a grid of the most prevalent
  topics (no test), controlled by \`overview_n_topics\`.

- \`"one_dimension_topics"\`: three columns (negative / non-significant
  / positive), controlled by \`scatter_legend_n\` (length 3).

- \`"two_dimension_topics"\`: a 3x3 quadrant layout with the
  distribution legend in the centre, controlled by \`scatter_legend_n\`
  (length 9).

## Usage

``` r
topicsPlotOverview(
  plot_list,
  overview_plot_type,
  overview_n_topics = 9,
  scatter_legend_n = NULL,
  title_font = "sans"
)
```

## Arguments

- plot_list:

  (list) Output from the \`topicsPlot()\` function.

- overview_plot_type:

  (character) One of \`"topics_grid"\`, \`"textTopics"\`,
  \`"one_dimension_topics"\`, or \`"two_dimension_topics"\`.

- overview_n_topics:

  (integer) For the \`"topics_grid"\` / \`"textTopics"\` overview only:
  the number of topic plots to include. Plots are arranged in 3 columns
  (the default of 9 produces a 3x3 grid). Ignored for the test-based 1D
  and 2D overviews, which instead use \`scatter_legend_n\`. Default: 9.

- scatter_legend_n:

  (numeric vector) For the test-based 1D and 2D overviews: the
  per-category counts that determine how many topic plots appear in the
  overview. Length 3 for 1D (negative, non-significant, positive) and
  length 9 for 2D (one value per quadrant). A value of 0 omits that
  category. Ignored for the prevalent-topics overview.

- title_font:

  Font family used for all non-word text elements in the plots (e.g.,
  titles, axis labels, tick labels, legend text, annotations). Default:
  \`"sans"\`.

## Value

A patchwork composition that can be printed, saved with \`ggsave()\`, or
further modified with patchwork operators.

## Details

This function is intended as a convenience for quickly inspecting
results. For publication figures, users typically save the individual
topic plots (returned separately by \`topicsPlot()\`) and assemble them
in external software such as PowerPoint, Google Drawings, or
Illustrator.
