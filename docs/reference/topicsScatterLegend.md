# Plot a distribution plot (available for the text-package)

Plot a distribution plot (available for the text-package)

## Usage

``` r
topicsScatterLegend(
  bivariate_color_codes,
  filtered_test,
  num_popout = 1,
  way_popout_topics = "mean",
  user_spec_topics = NULL,
  allow_topic_num_legend = FALSE,
  scatter_show_axis_values = TRUE,
  y_axes_1 = 2,
  cor_var = "",
  label_x_name = "x",
  label_y_name = "y",
  save_dir,
  figure_format = "svg",
  scatter_popout_dot_size = c(1, 5),
  scatter_bg_dot_size = c(1, 5),
  scatter_legend_dots_alpha = 0.8,
  scatter_legend_bg_dots_alpha = 0.2,
  scatter_legend_circles = FALSE,
  scatter_legend_circles_radius = 0,
  scatter_legend_circles_num = 4,
  width = 10,
  height = 8,
  seed = 42
)
```

## Arguments

- bivariate_color_codes:

  A vector of color codes specifying colors for different categories in
  the scatter plot. Default: c("#398CF9", "#60A1F7", "#5dc688",
  "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").

- filtered_test:

  A data frame containing the input data for the scatter plot. Must
  include columns like \`color_categories\` and other variables used in
  the function.

- num_popout:

  The number of topics to "pop out" in each category. Default: 1. Can be
  a single integer (applies to all categories) or a vector for specific
  categories.

- way_popout_topics:

  The method for selecting pop-out topics. Options: "mean", "max_y", or
  "max_x". Default: "mean".

- user_spec_topics:

  A vector of user-specified topics to highlight in the scatter plot.
  Default: NULL.

- allow_topic_num_legend:

  Logical; if TRUE, displays topic numbers in the legend. Default:
  FALSE.

- scatter_show_axis_values:

  Show values on the axises.

- y_axes_1:

  Specifies axis alignment for the scatter legend. Options: 1 (x-axis)
  or 2 (y-axis). Default: 2.

- cor_var:

  A string used for naming the correlation variable in labels or file
  names. Default: "".

- label_x_name:

  Label for the x-axis in the scatter plot. Default: "x".

- label_y_name:

  Label for the y-axis in the scatter plot. Default: "y".

- save_dir:

  Directory where the scatter legend plot will be saved. Default:
  "./results".

- figure_format:

  File format for the saved scatter plot. Examples: "svg", "png", "pdf".
  Default: "svg".

- scatter_popout_dot_size:

  Size of the dots for pop-out topics in the scatter legend. Set to
  "prevalence" for dot size changing based on topic prevalence. Default:
  15.

- scatter_bg_dot_size:

  Size of the dots for background topics in the scatter legend. Default:
  9.

- scatter_legend_dots_alpha:

  The transparency of the dots

- scatter_legend_bg_dots_alpha:

  The transparency of the dots

- scatter_legend_circles:

  Plot concentric circles for the scatter legend

- scatter_legend_circles_radius:

  Radius of first concentric circle

- scatter_legend_circles_num:

  Number of Concentric circles

- width:

  Width of the saved scatter plot in inches. Default: 10.

- height:

  Height of the saved scatter plot in inches. Default: 8.

- seed:

  Seed for reproducibility, ensuring consistent plot generation.
  Default: 42.
