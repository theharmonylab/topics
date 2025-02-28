

#'  Plot a distribution plot
#'  
#' @param bivariate_color_codes A vector of color codes specifying colors for 
#' different categories in the scatter plot. 
#' Default: c("#398CF9", "#60A1F7", "#5dc688", "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").
#' @param filtered_test A data frame containing the input data for the scatter plot. 
#' Must include columns like `color_categories` and other variables used in the function.
#' @param num_popout The number of topics to "pop out" in each category. Default: 1.
#'  Can be a single integer (applies to all categories) or a vector for specific categories.
#' @param way_popout_topics The method for selecting pop-out topics. Options: "mean", "max_y", or "max_x". Default: "mean".
#' @param user_spec_topics A vector of user-specified topics to highlight in the scatter plot. Default: NULL.
#' @param allow_topic_num_legend Logical; if TRUE, displays topic numbers in the legend. Default: FALSE.
#' @param scatter_show_axis_values Show values on the axises. 
#' @param y_axes_1 Specifies axis alignment for the scatter legend. Options: 1 (x-axis) or 2 (y-axis). Default: 2.
#' @param cor_var A string used for naming the correlation variable in labels or file names. Default: "".
#' @param label_x_name Label for the x-axis in the scatter plot. Default: "x".
#' @param label_y_name Label for the y-axis in the scatter plot. Default: "y".
#' @param save_dir Directory where the scatter legend plot will be saved. Default: "./results".
#' @param figure_format File format for the saved scatter plot. Examples: "svg", "png", "pdf". Default: "svg".
#' @param scatter_popout_dot_size Size of the dots for pop-out topics in the scatter legend. Set to "prevalence" for dot size changing based on topic prevalence. Default: 15.
#' @param scatter_bg_dot_size Size of the dots for background topics in the scatter legend. Default: 9.
#' @param width Width of the saved scatter plot in inches. Default: 10.
#' @param height Height of the saved scatter plot in inches. Default: 8.
#' @param seed Seed for reproducibility, ensuring consistent plot generation. Default: 42.
#' @importFrom ggplot2 ggplot geom_point scale_color_manual labs theme_minimal theme element_blank
#' @importFrom rlang sym !!
#' @importFrom dplyr pull select filter mutate anti_join summarise pull group_by group_modify ungroup
#' @export
topicsScatterLegend <- function(
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
    scatter_popout_dot_size = 15, 
    scatter_bg_dot_size = 9, 
    width = 10, 
    height = 8, 
    seed = 42
    ) {
  
  # Determine x, y, and color columns
  x_column <- names(filtered_test)[5]
  y_column <- if (y_axes_1 == 2) names(filtered_test)[9] else NULL
  color_column <- names(filtered_test)[ncol(filtered_test)]
  
  # Check for only significant or non-significant topics
  contains_category <- function(cat) {
    filtered_test %>%
      dplyr::summarise(contains_only = all(color_categories %in% cat)) %>%
      dplyr::pull(contains_only)
  }
  
  only_two <- contains_category(2)  # Non-significant topics
  only_five <- contains_category(5) # Significant topics
  
  # Logic for handling topics to be emphasised in scatter plot
  # User-specified topics for popout.
  if (!is.null(user_spec_topics)) {
    popout <- filtered_test %>% filter(topic %in% user_spec_topics)
    backgr_dots <- filtered_test %>% dplyr::anti_join(popout, by = colnames(filtered_test))
    
    # Only non-significant topics. Generating scatter legend.
  } else if (only_two && y_axes_1 == 1) {
    popout <- filtered_test %>% dplyr::filter(color_categories %in% 1:3)
    backgr_dots <- tibble::tibble() # No background dots
    
    # Only significant topics. Generating scatter plot.\n
  } else if (only_five && y_axes_1 == 2) {
    popout <- filtered_test
    backgr_dots <- tibble::tibble() # No background dots
    
    # Generating scatter plot based on specified popout criteria.\n
  } else {
    popout <- determine_popout_topics(
      filtered_test, num_popout, way_popout_topics, y_col =  y_column, x_col = x_column)
    
    # Convert `color_categories` in `popout` back to integer
    popout <- popout %>%
      dplyr::mutate(color_categories = as.integer(color_categories))
    filtered_test <- filtered_test %>%
      dplyr::mutate(color_categories = as.integer(color_categories))
    
    # Perform anti_join
    backgr_dots <- filtered_test %>% dplyr::anti_join(popout, by = colnames(filtered_test))
  }
 
  if (scatter_popout_dot_size == "prevalence"){
      popout <- popout %>%
        dplyr::mutate(dot_size = 3 + (prevalence - min(prevalence)) / (max(prevalence) - min(prevalence)) * (8 - 3))
      scatter_popout_dot_size <- popout$`dot_size`
      
      backgr_dots <- backgr_dots %>%
        dplyr::mutate(bg_dot_size = 2 + (prevalence - min(prevalence)) / (max(prevalence) - min(prevalence)) * (6 - 2))
      scatter_bg_dot_size <- backgr_dots$`bg_dot_size`
      
  }else{scatter_popout_dot_size <- scatter_popout_dot_size}
    
  # Generate scatter plot
  plot <- generate_scatter_plot(
    popout = popout,
    background = backgr_dots,
    bivariate_color_codes = bivariate_color_codes,
    x_col = x_column, 
    y_col = y_column, 
    label_x_name = label_x_name, 
    label_y_name = label_y_name, 
    color_col = color_column, 
    popout_size = scatter_popout_dot_size, 
    bg_size = scatter_bg_dot_size, 
    allow_topic_num_legend = allow_topic_num_legend, 
    scatter_show_axis_values = scatter_show_axis_values
  )
  
  # Save the plot
  if (!is.null(save_dir)){
    ggplot2::ggsave(paste0(save_dir, "/seed_", seed, 
                           "/wordclouds/",
                           "dot_legend_",
                           "corvar_", cor_var, ".", 
                           figure_format),
                    plot = plot, 
                    width = width, 
                    height = height, 
                    units = "in", 
                    device = figure_format, 
                    create.dir = TRUE)
  }
  
  
  #if (!only_two && !only_five){return (popout)}else{ return (NULL) }
  
  output <- list()
  output[[1]] <- popout
  output[[2]] <- plot
  names(output) <- c("popout", "legend")
  
  return(output)
}


#' @param filtered_test A data frame containing the input data, which must include a `color_categories` column. 
#' This column specifies the categories used for determining pop-out topics.
#' @param num_popout A vector of exactly 9 integers, specifying the number of topics to "pop out" for each 
#' category in a 3x3 grid. Each value corresponds to a category in `color_categories`.
#' @param way_popout_topics A string specifying the criterion for selecting pop-out topics. Options:
#'.  - "max_y": Selects topics with the maximum absolute values in the `y_col` column.
#'.  - "max_x": Selects topics with the maximum absolute values in the `x_col` column.
#'.  - "mean": Selects topics based on the highest mean of the absolute values of `x_col` and `y_col`.
#' @param y_col A string specifying the name of the column to be used for `y` values in the selection process.
#' This column must exist in `filtered_test`.
#' @param x_col A string specifying the name of the column to be used for `x` values in the selection process.
#' This column must exist in `filtered_test`.
#' @noRd
determine_popout_topics <- function(
    filtered_test, 
    num_popout, 
    way_popout_topics, 
    y_col = NULL, 
    x_col
) {
  # Ensure `color_categories` exists
  if (!"color_categories" %in% colnames(filtered_test)) {
    stop("The `filtered_test` dataset must include a `color_categories` column.")
  }
  
  # Convert `color_categories` to character for consistent comparison
  filtered_test <- filtered_test %>%
    dplyr::mutate(color_categories = as.character(color_categories))
  
  # Check for NA or unexpected values
  if (any(is.na(filtered_test$color_categories))) {
    stop("The `color_categories` column contains missing (NA) values.")
  }
  
  # Ensure `num_popout` has the correct number of values (either 3 or 9)
  if (!(length(num_popout) %in% c(3, 9))) {
    stop("`num_popout` must have exactly 3 or 9 values.")
  }
  
  # Map `num_popout` to corresponding categories (names will be "1", "2", etc.)
  legend_map_num_pop <- if (length(num_popout) == 9) {
    setNames(as.integer(num_popout), as.character(1:9))
  } else {
    setNames(as.integer(num_popout), as.character(1:3))
  }
  
  # Filter for categories present in `filtered_test`
  existing_categories <- unique(filtered_test$color_categories)
  valid_map <- legend_map_num_pop[names(legend_map_num_pop) %in% existing_categories]
  
  if (length(valid_map) == 0) {
    stop("No valid `color_categories` in `filtered_test` match `num_popout` mapping.")
  }
  
  # Define helper function for max-based selection (existing behavior)
  select_rows <- function(data, n_pop) {
    if (way_popout_topics == "max_y" && !is.null(y_col)) {
      return(dplyr::slice_max(data, order_by = abs(!!ggplot2::sym(y_col)), n = n_pop, with_ties = FALSE))
    }
    if (way_popout_topics == "max_x") {
      return(dplyr::slice_max(data, order_by = abs(!!ggplot2::sym(x_col)), n = n_pop, with_ties = FALSE))
    }
    if (way_popout_topics == "mean") {
      if (!is.null(y_col)) {
        data <- data %>%
          dplyr::mutate(mean_value = rowMeans(cbind(abs(!!ggplot2::sym(x_col)), abs(!!ggplot2::sym(y_col)))))
      } else {
        data <- data %>%
          dplyr::mutate(mean_value = abs(!!ggplot2::sym(x_col)))
      }
      return(dplyr::slice_max(data, order_by = mean_value, n = n_pop, with_ties = FALSE))
    }
    stop("Invalid `way_popout_topics`. Supported values are 'max_y', 'max_x', or 'mean'.")
  }
  
  # Define helper function for min-based selection (rows closest to 0)
  select_rows_min <- function(data, n_pop) {
    if (way_popout_topics == "max_y" && !is.null(y_col)) {
      return(dplyr::slice_min(data, order_by = abs(!!ggplot2::sym(y_col)), n = n_pop, with_ties = FALSE))
    }
    if (way_popout_topics == "max_x") {
      return(dplyr::slice_min(data, order_by = abs(!!ggplot2::sym(x_col)), n = n_pop, with_ties = FALSE))
    }
    if (way_popout_topics == "mean") {
      if (!is.null(y_col)) {
        data <- data %>%
          dplyr::mutate(mean_value = rowMeans(cbind(abs(!!ggplot2::sym(x_col)), abs(!!ggplot2::sym(y_col)))))
      } else {
        data <- data %>%
          dplyr::mutate(mean_value = abs(!!ggplot2::sym(x_col)))
      }
      return(dplyr::slice_min(data, order_by = mean_value, n = n_pop, with_ties = FALSE))
    }
    stop("Invalid `way_popout_topics`. Supported values are 'max_y', 'max_x', or 'mean'.")
  }
  
  # Determine which color category should use the min-based selection:
  # - For a 9-element vector, the popout category is "5".
  # - For a 3-element vector, the popout category is "2".
  popout_category <- if (length(num_popout) == 9) "5" else "2"
  
  # Process each category using group_modify:
  filtered_test %>%
    dplyr::filter(color_categories %in% names(valid_map)) %>%
    dplyr::group_by(color_categories) %>%
    dplyr::group_modify(~ {
      category <- .y$color_categories
      n_pop <- valid_map[[category]]
      if (n_pop > 0) {
        if (category == popout_category) {
          # For the designated popout group (only category "5" or "2"), use min-based selection.
          select_rows_min(.x, n_pop)
        } else {
          # For all other groups, use the existing max-based selection.
          select_rows(.x, n_pop)
        }
      } else {
        .x[0, ]  # Return an empty tibble for groups with 0 in the mapping.
      }
    }) %>%
    dplyr::ungroup()
}

#' @param popout A data frame containing the data points to be highlighted ("pop-out") in the scatter plot.
#' @param background A data frame containing the background data points for the scatter plot.
#'                   Can be empty if no background points are needed.
#' @param bivariate_color_codes A vector of color codes used to map `color_col` categories to colors in the scatter plot.
#' @param x_col A string specifying the name of the column to be used for the x-axis in the scatter plot.
#'              Must exist in both `popout` and `background` data frames.
#' @param y_col A string specifying the name of the column to be used for the y-axis in the scatter plot. Default: NULL.
#'              If NULL, a constant value of 1 is used for all points.
#' @param label_x_name Label for the x-axis in the scatter plot.
#' @param label_y_name Label for the y-axis in the scatter plot.
#' @param color_col A string specifying the name of the column in `popout` and `background` used to map categories to colors.
#' @param popout_size The size of the dots for pop-out points in the scatter plot.
#' @param bg_size The size of the dots for background points in the scatter plot.
#' @param allow_topic_num_legend Logical; if TRUE, adds topic numbers as text labels to the pop-out points. Default: FALSE.
#' @param scatter_show_axis_values Show the values on the axises. 
#' @noRd
generate_scatter_plot <- function(
    popout,
    background,
    bivariate_color_codes,
    x_col, 
    y_col = NULL,
    label_x_name, 
    label_y_name,
    color_col, 
    popout_size, 
    bg_size, 
    allow_topic_num_legend, 
    scatter_show_axis_values
) {
  
  # Define aesthetics for popout and background points
  # Ensure y_col is valid and resolve y_aesthetic
  y_aesthetic <- if (!is.null(y_col) && y_col != "") ggplot2::sym(y_col) else 1
  
  # Create aes with defined y aesthetic
  popout_aes <- ggplot2::aes(
    x = !!ggplot2::sym(x_col),
    y = y_aesthetic,
    color = as.factor(.data[[color_col]])
  )
  
  # Resolve y aesthetic value
  y_value <- if (is.null(y_col)) 1 else ggplot2::sym(y_col)
  
  # Aesthetics for background points
  bg_aes <- if (is.null(y_col)) {
    ggplot2::aes(
      x = !!ggplot2::sym(x_col),
      y = 1,
      color = as.factor(.data[[color_col]])
    )
  } else {
    ggplot2::aes(
      x = !!ggplot2::sym(x_col),
      y = !!ggplot2::sym(y_col),
      color = as.factor(.data[[color_col]])
    )
  }
  
  popout_aes <- if (is.null(y_col)) {
    ggplot2::aes(
      x = !!ggplot2::sym(x_col),
      y = 1,
      color = as.factor(.data[[color_col]])
    )
  } else {
    ggplot2::aes(
      x = !!ggplot2::sym(x_col),
      y = !!ggplot2::sym(y_col),
      color = as.factor(.data[[color_col]])
    )
  }
  
  # Build the plot
  plot <- ggplot2::ggplot()
  
  # Add background points only if background is not empty
  if (nrow(background) > 0) {
    plot <- plot +
      ggplot2::geom_point(data = background, bg_aes, size = bg_size, alpha = 0.2)
  }
  
  # Add popout points
  plot <- plot +
    ggplot2::geom_point(data = popout, 
                        popout_aes, 
                        size = popout_size, 
                        alpha = 0.8) +
    ggplot2::scale_color_manual(values = bivariate_color_codes) +
    ggplot2::labs(x = label_x_name, y = label_y_name, color = '') +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = if (scatter_show_axis_values) ggplot2::element_text(size = 12) else ggplot2::element_blank(),
      axis.ticks = if (scatter_show_axis_values) ggplot2::element_line() else ggplot2::element_blank(),
      legend.position = "none"
    )
  
  
  # Add topic numbers if enabled
  if (allow_topic_num_legend) {
    plot <- plot + geom_text(
      data = popout, 
      ggplot2::aes(x = !!ggplot2::sym(x_col), 
                   y = if (is.null(y_col)) 1 else !!ggplot2::sym(y_col), 
                   label = topic_number),
      size = popout_size - 3, 
      color = "black", 
      hjust = 0.5, 
      vjust = 0.5
    )
  }
  
  # Determine maximum absolute x-value
  x_values <- c(popout[[x_col]], background[[x_col]])
  max_abs_x <- max(abs(x_values))
  if (!is.null(y_col)){
    y_values <- c(popout[[y_col]], background[[y_col]])
    max_abs_y <- max(abs(y_values))
  }
  
  # Create symmetrical breaks
  # Find a suitable interval for the breaks. We'll try to get around 5 breaks.
  n_breaks <- 5
  interval <- max_abs_x/(n_breaks/2)
  breaks_x <- seq(-ceiling(max_abs_x/interval)*interval, ceiling(max_abs_x/interval)*interval, by = interval)
  if (!is.null(y_col)){
    interval <- max_abs_y/(n_breaks/2)
    breaks_y <- seq(-ceiling(max_abs_y/interval)*interval, ceiling(max_abs_y/interval)*interval, by = interval)
  }
  
  # 3. Set symmetrical x/y-axis limits AND explicit breaks
  plot <- plot + ggplot2::scale_x_continuous(limits = c(-max_abs_x, max_abs_x), breaks = breaks_x,
                                             labels = function(x) sprintf("%.2f", x))
  if (!is.null(y_col)){
    plot <- plot + ggplot2::scale_y_continuous(limits = c(-max_abs_y, max_abs_y), breaks = breaks_y,
                                               labels = function(x) sprintf("%.2f", x))
  }
  
  if (is.null(y_col)){
    plot <- plot + 
      ggplot2::theme(
        # Then apply this hjust_value and move x axis downward
        axis.title.x = ggplot2::element_text(hjust = 0.5,
                                             margin = ggplot2::margin(t = 21.3, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 21, unit = "pt"), size = 12),
        legend.position = "none",
        # Remove all y-axis elements
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.length.y = ggplot2::unit(0, "pt"), # Remove tick marks
        panel.spacing.y= ggplot2::unit(0, "lines"),
        panel.border = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        aspect.ratio = 1/20,
        # Other settings
        plot.margin = ggplot2::margin(0.5, 0.5, 1, 0.5, "cm") 
      ) 
  }else{
    plot <- plot +
      ggplot2::theme(
        # Then apply this hjust_value and move x axis downward
        axis.title.x = ggplot2::element_text(hjust = 0.5,
                                             margin = ggplot2::margin(t = 10.6, unit = "pt")
        ),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10.3, unit = "pt"), size = 12),
        legend.position = "none",
        # Other settings
        axis.ticks.x = ggplot2::element_line(),
        axis.text.y = ggplot2::element_text(size = 12),
        plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "cm")
      )
  }
  
  plot <- plot + ggplot2::coord_cartesian(clip = "off") # Prevent clipping
  
  return(plot)
}

#### topicsGridLegend ####
#' @param bivariate_color_codes A vector of color codes specifying the colors for the 3x3 grid legend.
#'                              Default: c("#398CF9", "#60A1F7", "#5dc688", "#e07f6a", "#EAEAEA", "#40DD52", "#FF0000", "#EA7467", "#85DB8E").
#' @param filtered_test A data frame containing the filtered topic data. Must include a `color_categories` column.
#' @param cor_var A string used to name the correlation variable, included in the file name of the saved plot. Default: "".
#' @param save_dir Directory where the grid legend plot will be saved. Default: "./results".
#' @param figure_format File format for the saved grid legend plot. Examples: "svg", "png", "pdf". Default: "svg".
#' @param seed Seed for reproducibility, ensuring consistent plot generation. Default: 42.
#' @param width Width of the saved grid legend in inches. Default: 10.
#' @param height Height of the saved grid legend in inches. Default: 8.
#' @param y_axes_1 Specifies axis alignment for the grid legend. Options: 2 (2D grid with x and y axes) or 1 (1D legend for x-axis only). Default: 2.
#' @param legend_title Title text for the grid legend. Must be provided by the user.
#' @param legend_title_size Font size of the legend title text. Must be provided by the user.
#' @param titles_color Color of the title and axis labels in the legend. Must be provided by the user.
#' @param legend_x_axes_label Label for the x-axis of the grid legend. Must be provided by the user.
#' @param legend_y_axes_label Label for the y-axis of the grid legend. Must be provided by the user.
#' @param topic_data_all A data frame containing all topic data, including the `color_categories` column used for counting topics.
#' @param legend_number_color Color of the numeric annotations in the grid legend. Must be provided by the user.
#' @param legend_number_size Font size of the numeric annotations in the grid legend. Must be provided by the user.' @return A legend plot saved that can be combined with the plot object.
#' @importFrom tidyr gather separate
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_tile ggtitle scale_fill_identity labs theme_void annotate theme element_text coord_fixed ggsave
#' @noRd
topicsGridLegend <- function(
    bivariate_color_codes = c(
      "#398CF9", "#60A1F7", "#5dc688",
      "#e07f6a", "#EAEAEA", "#40DD52",
      "#FF0000", "#EA7467", "#85DB8E"),
    filtered_test,
    cor_var = "",
    save_dir,
    figure_format = 'svg',
    seed = 42,
    width = 10, 
    height = 8, 
    y_axes_1 = 2,
    legend_title,
    legend_title_size,
    titles_color,
    legend_x_axes_label,
    legend_y_axes_label,
    topic_data_all,
    legend_number_color,
    legend_number_size
) {
  if (y_axes_1 == 2){y_axes_1 <- ""}else{y_axes_1 <- "only_x_dimension"}
  legCor <- bivariate_color_codes
  if(y_axes_1 == "only_x_dimension"){
    bivariate_color_data <- tibble::tibble(
      "1 - 3" = '', "2 - 3" = '', "3 - 3" = '',
      "1 - 2" = legCor[1], "2 - 2" = legCor[2], "3 - 2" = legCor[3],
      "1 - 1" = '', "2 - 1" = '', "3 - 1" = '')
  }else{bivariate_color_data <- tibble::tibble(
    "1 - 3" = legCor[1], "2 - 3" = legCor[2], "3 - 3" = legCor[3],
    "1 - 2" = legCor[4], "2 - 2" = legCor[5], "3 - 2" = legCor[6],
    "1 - 1" = legCor[7], "2 - 1" = legCor[8], "3 - 1" = legCor[9]
  )}
  bivariate_color_data <- rbind(bivariate_color_data, bivariate_color_codes)
  bivariate_color_data <- bivariate_color_data[-1, ]
  
  if (y_axes_1 == "only_x_dimension") {
    # Only select 3 colors
    bivariate_color_data <- bivariate_color_data[, c(4, 5, 6)]
    colnames(bivariate_color_data) <- c("1 - 2", "2 - 2", "3 - 2")
    #bivariate_color_data
    # Remove the y axes title on the legend
    legend_y_axes_label <- " "}
  # To output the number of categories for dim 1 and dim 2 (plot 1dim or 2dim)
  if (y_axes_1 == "only_x_dimension") {
    # for future only x dim grid in topicsTest
    categoryTotal_x_axes = c(
      sum(topic_data_all$color_categories == 1,
          na.rm = TRUE),
      sum(topic_data_all$color_categories == 2,
          na.rm = TRUE),
      sum(topic_data_all$color_categories == 3,
          na.rm = TRUE))
  }else{ categoryTotal_x_axes = c(
    sum(topic_data_all$color_categories == 4,
        na.rm = TRUE),
    sum(topic_data_all$color_categories == 5,
        na.rm = TRUE),
    sum(topic_data_all$color_categories == 6,
        na.rm = TRUE))}
  
  legend <- bivariate_color_data %>%
    tidyr::gather("group", "fill") %>%
    tidyr::separate(group, into = c("x", "y"), sep = " - ") %>%
    dplyr::mutate(
      x = as.integer(x),
      y = as.integer(y)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = fill)) +
    ggplot2::ggtitle(paste0(legend_title)) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      x = legend_x_axes_label,
      y = legend_y_axes_label
    ) +
    ggplot2::theme_void() +
    #    ggplot2::annotate(geom="text", x=2, y=2, label="ns",
    #               color = titles_color, size=legend_number_size)+
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 3, label = sum(topic_data_all$color_categories == 1,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size#bivariate_color_codes[1]
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 3, label = sum(topic_data_all$color_categories == 2,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 3, label = sum(topic_data_all$color_categories == 3,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    ggplot2::annotate(
      geom = "text", x = 1, y = 2, label = categoryTotal_x_axes[1],
      color = legend_number_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 2, y = 2, label = categoryTotal_x_axes[2],
      color = legend_number_color, size = legend_number_size
    ) +
    ggplot2::annotate(
      geom = "text", x = 3, y = 2, label = categoryTotal_x_axes[3],
      color = legend_number_color, size = legend_number_size
    ) +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 1, y = 1, label = sum(topic_data_all$color_categories == 7,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 2, y = 1, label = sum(topic_data_all$color_categories == 8,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    {
      if (y_axes_1 != "only_x_dimension") {
        ggplot2::annotate(
          geom = "text", x = 3, y = 1, label = sum(topic_data_all$color_categories == 9,
                                                   na.rm = TRUE
          ),
          color = legend_number_color, size = legend_number_size
        )
      }
    } +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = legend_title_size + 1),
      title = ggplot2::element_text(color = titles_color),
      axis.title.x = ggplot2::element_text(color = titles_color),
      axis.title = ggplot2::element_text(size = legend_title_size),
      axis.title.y = ggplot2::element_text(angle = 90, color = titles_color)
    ) +
    ggplot2::coord_fixed()
  
  if (!is.null(save_dir)){
    ggplot2::ggsave(paste0(save_dir,"/seed_", seed, 
                           "/wordclouds/",
                           "grid_legend_",
                           "corvar_", cor_var,
                           ".",
                           figure_format),
                    plot = legend, 
                    width = width, 
                    height = height, 
                    units = "in", 
                    create.dir = TRUE)
  }
  
  return(legend)
}

#' The function to create lda wordclouds
#' @param model (list) The trained model
#' @param test (list) The test results
#' @param popout (tibble) The tibble containing the topic idx to popout
#' @param color_negative_cor (R_obj) The color gradient for negative correlations
#' @param color_positive_cor (R_obj) The color gradient for positive correlations
#' @param grid_pos (numeric) The position for grid topics
#' @param scale_size (logical) Whether to scale the size of the words
#' @param plot_topics_idx (vector) The topics to plot determined by index
#' @param p_alpha (integer) The p-value threshold to use for significance
#' @param highlight_topic_words (named vector) The dictionary to popout negative words to an individual plot for easier reading. 
#'  Default words are "not", "never". Words are as vector names. 
#'  The values of the vector determine the color code to popout. The color values can be different for different words.
#' @param save_dir (string) The directory to save the wordclouds
#' @param figure_format (string) Set the figure format, e.g., svg, or png.
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The max size of the words.
#' @param seed (integer) The seed to set for reproducibility
#' @return nothing is returned, the wordclouds are saved in the save_dir
#' @noRd
topicsPlot1 <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    popout = NULL,
    color_negative_cor = NULL,
    color_positive_cor = NULL,
    grid_pos = "",
    scale_size = FALSE,
    plot_topics_idx = NULL,
    p_alpha = 0.05,
    highlight_topic_words = c(not = "#2d00ff", never = "#2d00ff"),    
    save_dir,
    figure_format = "svg",
    width = 10, 
    height = 8,
    max_size = 10, 
    seed = 42){
  
  df_list = NULL

  if (!is.null(model)){
    
    # if model$model_type == "bert_topic" (i.e., a maller model return null on model$model_type)
    if (!is.null(model$model_type)) {
      
      if(!is.null(test)){
        num_topics <- nrow(test$test)
      } else {
        num_topics <- length(model$model$summary$topic)
        model$summary <- model$model$summary
      }
      
      if(num_topics == 0){
        stop("There are no significant topics to plot.")
      }
      
      df_list <- create_df_list_bert_topics(
        save_dir, 
        seed, 
        num_topics)
      
    } else {
      # if from mallet: 
      model <- name_cols_with_vocab(model, "phi", model$vocabulary)
      df_list <- create_topic_words_dfs(model$summary)
      df_list <- assign_phi_to_words(df_list, model$phi, "mallet")
    }
  }
  
  if (!is.null(test) && !is.null(model)){
    summary = model$summary
    cor_var = test$x_y_axis
    test_type = test$test_method
    test = test$test
  }
  
  if (!is.null(model) && is.null(test)){
    summary = model$summary
  }
  
  if (is.null(model) && !is.null(ngrams) && !is.null(test)){
    test = test$test
  }
  
  plot <- create_plots(
    df_list = df_list, 
    summary = summary,
    ngrams = ngrams$ngrams,
    test = test, 
    test_type = test_type,
    cor_var = cor_var,
    popout = popout,
    color_negative_cor = color_negative_cor,
    color_positive_cor = color_positive_cor,
    grid_pos = grid_pos,
    scale_size = scale_size,
    plot_topics_idx = plot_topics_idx,
    p_alpha = p_alpha,
    highlight_topic_words = highlight_topic_words,  
    save_dir = save_dir,
    figure_format = figure_format,
    width = width, 
    height = height,
    max_size = max_size,
    seed = seed)
  
  return(plot)
  
}


#' The function sets default colors or arranges user specified colors
#' @param color_scheme (string or vector of strings) 
#' @return default colors or specified user colours in the right order and structure.
#' @noRd
colour_settings <- function(
    color_scheme, 
    test, 
    ngrams, 
    model, 
    dim){
  
  bivariate_color_codes <- NULL
  bivariate_color_codes_b <- NULL
  bivariate_color_codes_f <- NULL  
  #### Checking and arranging colors ####
  if(!color_scheme[[1]] == "default"){
    
    # Dim 0 (i.e., no test)
    if(is.null(test)){
      
      if(length(color_scheme) == 2) {
        bivariate_color_codes <- rep(color_scheme, 2)
      } else {
        stop("Please provide 2 colours in the color_scheme parameter or set it to 'default'.")
      }
    }
    
    # Dim 1: N-gram
    if (dim == 1 && !is.null(ngrams)){
      
      if(length(color_scheme) == 4) {
        bivariate_color_codes <- color_scheme
      } else {
        stop("Please provide 4 colours  in the color_scheme parameter or set it to 'default'.")
      }
    }
    
    # Dim 1: topics
    if (dim == 1 && !is.null(model)){
      
      if(length(color_scheme) == 6) {
        # Select every second color for "back" colour in the gradient 
        bivariate_color_codes_b <- color_scheme[seq(1, length(color_scheme), by = 2)]
        # Select every second color for "front" colour in the gradient 
        bivariate_color_codes_f <- color_scheme[seq(2, length(color_scheme), by = 2)]
        bivariate_color_codes_f <- setNames(bivariate_color_codes_f, seq_along(bivariate_color_codes_f))
        
      } else {
        stop("Please provide 6 colours for the gradient.")
      }
    }
    
    # Dim 2: topics
    if (dim == 2 && !is.null(model)){
      
      if(length(color_scheme) == 18) {
        # Select every second color for "back" colour in the gradient 
        bivariate_color_codes_b <- color_scheme[seq(1, length(color_scheme), by = 2)]
        # Select every second color for "front" colour in the gradient 
        bivariate_color_codes_f <- color_scheme[seq(2, length(color_scheme), by = 2)]
        bivariate_color_codes_f <- setNames(bivariate_color_codes_f, seq_along(bivariate_color_codes_f))
        
      } else {
        stop("Please provide 18 colours or use color_scheme = 'default'.")
      }
    }
  }
  
  #### Setting the (default) colours ####
  if (color_scheme[[1]] == "default"){
    
    if (is.null(test)){ # && is.null(ngrams)
      bivariate_color_codes <- c(
        # gradient colours 1 and 2
        "#EAEAEA", "darkblue", 
        "#EAEAEA", "darkblue")
    }
    
    if (dim == 1 && !is.null(ngrams)){
      # gradient pairs
      bivariate_color_codes <- c(
        "#EAEAEA", "darkred", # negative ngrams colours
        "#EAEAEA", "darkgreen" # positve ngrams colours
      )
    }
    
    # Below colours are used in for loop iterations; so easiest to have back and front colors separate
    
    if (dim == 1 && is.null(ngrams)){
      # gradient pairs
      
      # Colors for the "background" words
      bivariate_color_codes_b <- c(
        "#e07f6a",  "lightgray","#5dc688"
      )
      # Colors for the "front" words
      bivariate_color_codes_f <- c(
        "darkred", "darkgray", "darkgreen"
      ) 
    }
    
    if (dim == 2){
      # Colors for the "background" words
      bivariate_color_codes_b <- rep("lightgray", 9)
      
      # Colors for the "front" words 
      bivariate_color_codes_f <-  c(
        "#398CF9",  # quadrant 1 (upper left corner)
        "#60A1F7",  # quadrant 2 
        "#5dc688",  # quadrant 3 (upper right corner)
        "#e07f6a",  # quadrant 4
        "darkgray", # quadrant 5 (middle square)
        "#40DD52",  # quadrant 6 
        "#FF0000",  # quadrant 7 (bottom left corner)
        "#EA7467",  # quadrant 8 
        "#85DB8E")  # quadrant 9 (bottom right corner)
    }
  }
  
  codes <- list(
    bivariate_color_codes,
    bivariate_color_codes_b,
    bivariate_color_codes_f)
  
  return(codes)
}


#' General function to clean characters in a specified column
#'
#' @param data  
#' @param column  
#' @return default colors or specified user colours in the right order and structure.
#' @noRd
clean_characters <- function(
    data, 
    column) {
  
  # Replace "<" and ">" with "_"
  data[[column]] <- gsub("[<>]", "_", data[[column]])
  
  # Replace "-" with "_-_"
  data[[column]] <- gsub("-", "_-_", data[[column]])
  
  # Replace digits 0-9 with "_digit_"
  data[[column]] <- gsub("([0-9])", "_\\1_", data[[column]])
  
  # Special case: Replace "0" with "_10_"
  data[[column]] <- gsub("_0_", "_10_", data[[column]])
  
  return(data)
}

# Wrapper function for cleaning 'ngrams$ngrams$ngrams'
clean_characters_for_plotting_grams <- function(ngrams) {
  
  ngrams$ngrams <- clean_characters(ngrams$ngrams, "ngrams")
  
  return(ngrams)
}

# Wrapper function for cleaning 'test$test$top_terms'
clean_characters_for_plotting_test <- function(test) {
  
  test$test <- clean_characters(test$test, "top_terms")
  
  return(test)
}


#' Plot word clouds
#' 
#' This function create word clouds and topic figures
#' @param model (list) A trained topics model, e.g., from topicsModel(). Should be NULL if plotting ngrams.
#' @param ngrams (list) The output from the the topicsGram() function. Should be NULL if plotting topics.
#' Note 1: it is not possible to plot tags like <place>; so the < are replaced with underscore. 
#' Note 2: it is not possible to plot dash - alone, it is replaced with `_-_`.
#' @param test (list) The test results; if plotting according to dimension(s) include the object from topicsTest() function. 
#' @param p_alpha (integer) The p-value threshold to use for significance testing.
#' @param p_adjust_method (character) Method to adjust/correct p-values for multiple comparisons (default = "none"; 
#' see also "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr").
#' @param ngrams_max (integer) The maximum number of n-grams to plot.
#' @param ngram_select (character) Method to select ngrams_max, when using both ngram and test use "prevalence" or "estimate"; 
#' if you only use ngrams use "pmi", "frequency", or "proportion". 
#' @param color_scheme (string 'default' or vector) The color scheme.
#'  
#' For plots not including a test, the color_scheme should in clude 2 colours (1 gradient pair), such as:
#'
#' c("lightgray", "darkblue)
#' 
#' For 1 dimensional plots of n-grams it should contain 4 colours (2 gradient pairs), such as: 
#'
#' c(
#' "#EAEAEA", "darkred", # negative ngrams colors
#' 
#' "#EAEAEA", "darkgreen" # positve ngrams colors)
#' 
#' 
#' 
#' For 1-dimension plots of topics, it should contain 6 colours (3 gradient pairs), such as 
#'
#'  c(
#' "#EAEAEA", "darkred",     # negative topics colors
#' 
#' "#EAEAEA", "darkgray",     # colours of topics not significantly associated
#' 
#' "#EAEAEA", "darkgreen"     # positve topics colors)
#' 
#' 
#'
#'  For 2-dimensional plots of topics, the color scheme should contain 18 colours (9 gradient pairs), such as:
#'  
#'  c(
#'   "lightgray", "#398CF9",     # quadrant 1 (upper left corner)
#'   
#'   "lightgray", "#60A1F7",     # quadrant 2 
#'   
#'   "lightgray", "#5dc688",     # quadrant 3 (upper right corner)
#'   
#'   "lightgray", "#e07f6a",     # quadrant 4
#'   
#'   "lightgray", "darkgray",     # quadrant 5 (middle square)
#'   
#'   "lightgray", "#40DD52",     # quadrant 6 
#'   
#'   "lightgray", "#FF0000",     # quadrant 7 (bottom left corner)
#'   
#'   "lightgray", "#EA7467",     # quadrant 8 
#'   
#'   "lightgray", "#85DB8E")     # quadrant 9 (bottom right corner).
#'
#' 
#' @param highlight_topic_words (named vector) Words to highlight in topics (e.g., negative words). 
#'  The values of the vector determine the color: highlight_topic_words = c(not = "#2d00ff", never = "#2d00ff"); note that it needs
#'  to be hexa codes, so color naming such as "blue" does not work. The default value is NULL.
#' @param allowed_word_overlap (numeric) A filter function determining the maximum number of identical words in the topics to be plotted. 
#' This filter removes topics within each "color group" and also include removing topics from the distribution and grid legends; 
#' (Note that the adjustment for multiple comparison is taking place before these are removed; i.e., the adjusted p-values are not affected by this filter).   
#' @param scale_size (logical) Whether to scale the size of the words.
#' @param plot_topics_idx (vector)  The index or indices of the topics to plot 
#' (look in the model-object for the indices). They can, for example, be c(1, 3:5) to plot topic t_1, t_3, t_4 and t_5) (optional).
#' @param plot_n_most_prevalent_topics (numeric) Plots the n most prevalent topics in a given model. 
#' @param save_dir (string) The directory to save the plots.
#' @param figure_format (string) Set the figure format, e.g., ".svg", or ".png".
#' @param width (integer) The width of the topic (units = "in"). 
#' @param height (integer) The width of the topic (units = "in").
#' @param max_size (integer) The maximum size of the words.
#' @param seed (integer) The seed to set for reproducibility.
#' @param scatter_legend_dot_size (integer) The size of dots in the scatter legend. If set to "prevalence", the size will change accordingly.
#' @param scatter_legend_bg_dot_size (integer) The size of background dots in the scatter legend.
#' @param scatter_legend_n (numeric or vector) A vector determining the number of dots to emphasize in each quadrant of the scatter legend.
#' For example: c(1,1,1,1,0,1,1,1,1) result in one dot in each quadrant except for the middle quadrant. 
#' @param scatter_legend_method (string) The method to filter topics to be emphasized in the scatter legend; either "mean", "max_x", or "max_y".
#' @param scatter_legend_specified_topics (vector) Specify which topic(s) to emphasize in the scatter legend. 
#' For example, c("t_1", "t_2"). If set, scatter_legend_method will have no effect.
#' @param scatter_legend_topic_n (boolean) If TRUE, the topic numbers are shown in the scatter legend.
#' @param scatter_show_axis_values (boolean) If TRUE, the estimate values are shown on the distribution plot axes.
#' @param grid_legend_title Title of the grid topic plot.
#' @param grid_legend_title_size Title size of the grid topic plot.
#' @param grid_legend_title_color Legend title color of the grid topic plot.
#' @param grid_legend_x_axes_label x-axis label of the grid topic plot.
#' @param grid_legend_y_axes_label y-axis label of the grid topic plot.
#' @param grid_legend_number_color Text color in the legend boxes of the grid topic plot.
#' @param grid_legend_number_size Text size in the legend boxes.
#' @return The function provides a list of topic plots (if there are any significant topics), a legend plot, and a plot showing the topic distribution.
#' If save_dir is specified, it saves all plots in this directory. 
#' If you want to show all plots irrespective of the topics' significance, set p_alpha = 1. 
#' @importFrom dplyr filter arrange desc top_n select
#' @importFrom ggplot2 scale_color_gradient
#' @importFrom tibble as_tibble
#' @importFrom stats p.adjust
#' @export
topicsPlot <- function(
    model = NULL,
    ngrams = NULL,
    test = NULL,
    p_alpha = 0.05,
    p_adjust_method = "none",
    ngrams_max = 30,
    ngram_select = "prevalence",
    color_scheme = "default",
    highlight_topic_words = NULL,
    scale_size = FALSE,
    plot_topics_idx = NULL,
    allowed_word_overlap = NULL,
    plot_n_most_prevalent_topics = NULL,
    save_dir = NULL,
    figure_format = "svg",
    width = 6,
    height = 5,
    max_size = 10,
    seed = 42,
    scatter_legend_dot_size = 15,
    scatter_legend_bg_dot_size = 9,
    scatter_legend_n = c(1,1,1,1,0,1,1,1,1),
    scatter_legend_method = c("mean"),
    scatter_legend_specified_topics = NULL,
    scatter_legend_topic_n = FALSE,
    scatter_show_axis_values = TRUE,
    grid_legend_title = "legend_title",
    grid_legend_title_size = 5,
    grid_legend_title_color = 'black',
    grid_legend_x_axes_label = "legend_x_axes_label",
    grid_legend_y_axes_label = "legend_y_axes_label",
    grid_legend_number_color = 'white',
    grid_legend_number_size = 15){
  
  
  set.seed(seed)
  
  #### Setting the number of dimensions to plot ####
  # If no test is provide set dim to 0 
  if(is.null(test)){
    dim = 0
  }
  
  # If a test is given
  if(!is.null(test)){
    
    # set default to 1 since that works for both n-grams and topics
    dim = 1
    
    # Only set dim to 2 if the test include enough tests
    if(ncol(test$test) == 12) {
      dim = 2
    }
  }
  
  #### Setting colors ####
  codes <- colour_settings(
    color_scheme = color_scheme, 
    model = model,
    test = test, 
    ngrams = ngrams, 
    dim = dim)
  
  bivariate_color_codes   <- codes[[1]]
  bivariate_color_codes_b <- codes[[2]]
  bivariate_color_codes_f <- codes[[3]]

  if (!is.null(bivariate_color_codes_f)){
      names(bivariate_color_codes_f) <- as.character(seq(1:length(bivariate_color_codes_f))) # The names of the color vector prevent the wrong ordering of colors in scatter plot.
  }
  
  #### Controlling parameter settings and giving instructions #####
  if (!is.vector(scatter_legend_n) || !is.numeric(scatter_legend_n)){
    msg <- "The parameter 'scatter_legend_n' should be either a numeric vector or a number.\n"
    message(colourise(msg, "brown"))
    return (NULL)
  }
  
  #### Add adjustment of p-values for multiple comparisons ####
  if (p_adjust_method != "none"){
    
    # reset the adjusted p-value with potentially new correction method
    test$test[[8]]<- stats::p.adjust(p = test$test[[7]],
                              method = p_adjust_method)
    
    if(dim == 2) test$test[[12]]<- stats::p.adjust(p = test$test[[11]],
                                            method = p_adjust_method)
    
  } 
  if(p_adjust_method != "none"){
    
    # set the original p-value as the adjusted for plotting
    test$test[[8]] <- test$test[[7]]
    if(dim == 2) test$test[[12]] <- test$test[[11]]
  }
  
  #### Setting colour-categories: Selecting elements to plot according to the p_alpha ####
  if (dim == 1) {
    
    # Getting column names
    bak1 <- colnames(test$test)[c(5,8)]
    colnames(test$test)[c(5,8)] <- c('x_plotted', 'adjusted_p_values.x')
    
    # Getting colour-categories
    test$test <- topicsNumAssign_dim2(test$test, p_alpha, 1)
    # Setting the original clumns
    colnames(test$test)[c(5,8)] <- bak1
    
  }
  if (dim == 2){
    
    bak1 <- colnames(test$test)[c(5,8,9,12)]
    colnames(test$test)[c(5,8,9,12)] <- c('x_plotted', 'adjusted_p_values.x',
                                          'y_plotted', 'adjusted_p_values.y')
    
    test$test <- topicsNumAssign_dim2(test$test, p_alpha, 2)
    colnames(test$test)[c(5,8,9,12)] <- bak1
  }
  
  
  #### Filtering duplicate topics #### 
  if (!is.null(allowed_word_overlap) & is.null(plot_n_most_prevalent_topics)){
    
    # Remove duplicates within group categories
    arranged_topics <- test$test 
    max_n_texts <- nrow(test$test )

    # Apply the function to each color group based on scatter_legend_n
    #df = arranged_topics
    
    # Store the original column names
    original_col_order <- names(arranged_topics)
    
    test$test <- arranged_topics %>%
      dplyr::mutate(color_categories1 = color_categories) %>%
      dplyr::group_by(color_categories) %>%
      dplyr::group_modify(~ select_non_overlapping_texts(
        df = .x, 
        text_column = "top_terms", 
        n_texts = max_n_texts, 
        allowed_word_overlap)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-color_categories1) %>%            # Remove the temporary column
      dplyr::select(dplyr::all_of(original_col_order)) # Reorder columns to the original order
  }
  
  #### Selecting the most prevalence topics ####
  if(!is.null(plot_n_most_prevalent_topics) & !is.null(plot_topics_idx)){
    stop("Please note that you cannot set both the plot_n_most_prevalent_topics and the plot_topics_idx parameters.")
  }
  
  if (!is.null(plot_n_most_prevalent_topics)) {
    
    arranged_topics <- model$summary %>% 
      dplyr::arrange(dplyr::desc(prevalence))
    
    #if(!is.null(allowed_word_overlap)){
    arranged_topics <- select_non_overlapping_texts(
        arranged_topics, 
        "top_terms", 
        n_texts = plot_n_most_prevalent_topics, 
        allowed_word_overlap = allowed_word_overlap 
      )
   # }
    
    plot_topics_idx <- arranged_topics$topic
    
  }
  
  
  #### NGRAM filtering and fixing tags (e.g., <place>) in ngrams because of error when plotting #### 
  
  if(!is.null(ngrams) & !is.null(ngrams_max)){
    
    if(is.null(test)){
      if (!ngram_select %in% c("pmi", "frequency", "proportion")){
        stop("ngram_select incorrect -- can only select pmi, frequency, or proportion when not including a test.")
      }
      
      ngrams$ngrams <- ngrams$ngrams %>% 
        dplyr::arrange(
          if (ngram_select == "pmi") {
            dplyr::desc(pmi)
          } else if (ngram_select == "frequency") {
            dplyr::desc(freq)
          } else if (ngram_select == "proportion") {
            dplyr::desc(prop)
          } else {
            stop("Invalid value for ngram_select")
          }
        ) %>%
        dplyr::slice_head(n = ngrams_max)
      
      ngrams <- clean_characters_for_plotting_grams(ngrams)
    }
    if(!is.null(test)){
      
      if (!ngram_select %in% c("prevalence", "estimate")){
        stop("ngram_select incorrect -- when using ngram and test use one of prevalence or estimate")
      }
      
      # merge frequency
      # get the name of the column to arrange after colnames(test$test)
      beta_column <- names(test$test)[grepl("_beta$", names(test$test))][1]
    
      # Filter and arrange by positive beta scores
      positive_ngrams <- test$test %>%
        dplyr::filter(.data[[beta_column]] > 0) %>%
        {
          if (ngram_select == "estimate") {
            dplyr::arrange(., dplyr::desc(.data[[beta_column]]))
          } else if (ngram_select == "prevalence") {
            dplyr::arrange(., dplyr::desc(prevalence))
          } else {
            stop("Invalid value for ngram_select")
          }
        } %>%
        dplyr::slice_head(n = ngrams_max)
      
      negative_ngrams <- test$test %>%
        dplyr::filter(.data[[beta_column]] < 0) %>%
        {
          if (ngram_select == "estimate") {
            dplyr::arrange(., dplyr::desc(.data[[beta_column]]))
          } else if (ngram_select == "prevalence") {
            dplyr::arrange(., dplyr::desc(prevalence))
          } else {
            stop("Invalid value for ngram_select")
          }
        } %>%
        dplyr::slice_head(n = ngrams_max)
      negative_ngrams[34,]
      # Combine the positive and negative n-grams
      test$test <- dplyr::bind_rows(positive_ngrams, negative_ngrams)

      test <- clean_characters_for_plotting_test(test)
    }
  }
  
  #### Making the plots ####
  #### Plotting topics from model without at test | ####
  #### Plotting n-grams WIHT test | ### 
  #### Plotting n-grams WIHTOUT test | ####
  if(!is.null(model) & is.null(test) | 
     !is.null(ngrams) && !is.null(test)|
     !is.null(ngrams) && is.null(test)){
    
    
    plot_list <- topicsPlot1(
      model = model,
      ngrams = ngrams,
      test = test,
      p_alpha = p_alpha,
      highlight_topic_words = NULL,
      scale_size = scale_size,
      plot_topics_idx = plot_topics_idx,
      popout = NULL,
      color_negative_cor = ggplot2::scale_color_gradient(
        low = bivariate_color_codes[1], high = bivariate_color_codes[2]), # grey in hex code
      color_positive_cor = ggplot2::scale_color_gradient(
        low = bivariate_color_codes[3], high = bivariate_color_codes[4]),
      save_dir = save_dir,
      figure_format = figure_format,
      width = width, 
      height = height,
      max_size = max_size, 
      seed = seed
    )
  }
  
  
  #### 1- or 2 dimensional topic-plots ####

  
  if(is.null(ngrams) & !is.null(test$test)){
    
    popout1 <- topicsScatterLegend(
      bivariate_color_codes = bivariate_color_codes_f,
      filtered_test = test$test,
      num_popout = scatter_legend_n,
      y_axes_1 = dim,
      cor_var = test$x_y_axis,
      label_x_name = grid_legend_x_axes_label,
      label_y_name = grid_legend_y_axes_label,
      way_popout_topics = scatter_legend_method,
      user_spec_topics = scatter_legend_specified_topics,
      allow_topic_num_legend = scatter_legend_topic_n,
      scatter_popout_dot_size = scatter_legend_dot_size,
      scatter_bg_dot_size = scatter_legend_bg_dot_size,
      scatter_show_axis_values = scatter_show_axis_values,
      save_dir = save_dir,
      figure_format = figure_format,
      width = 15,
      height = 15*9/16,
      seed = seed
    )
    popout <- popout1$popout
  }
  
  
  
  if (!is.null(model) & !is.null(test)){
    
    if (dim == 1){
      #i=1
      plot_list <- list()
      plot_list <- vector("list", length = 3)
      names(plot_list) <- paste0("square", 1:3)
      
      for (i in 1:3){
        if (! (i %in% test$test$color_categories)){next}
        
        filtered_test <- test
        filtered_test$test <- dplyr::filter(
          tibble::as_tibble(filtered_test$test,.name_repair="minimal"),
          color_categories == i)
        color_b <- bivariate_color_codes_b[i]
        color_f <- bivariate_color_codes_f[i]
        
        plot_topics_idx <- as.numeric(sub(".*_", "", filtered_test[["test"]]$topic))
        
        plot <- topicsPlot1(
          model = model,
          test = filtered_test,
          popout = popout,
          color_negative_cor = ggplot2::scale_color_gradient(
            low = color_b, high = color_f),
          color_positive_cor = ggplot2::scale_color_gradient(
            low = color_b, high = color_f),
          grid_pos = i,
          scale_size = scale_size,
          plot_topics_idx = plot_topics_idx,
          p_alpha = p_alpha,
          highlight_topic_words = highlight_topic_words,
          save_dir = save_dir,
          figure_format = figure_format,
          width = width, 
          height = height,
          max_size = max_size, 
          seed = seed
        )
        plot_list[[i]] <- plot
      }
    }
    
    if (dim == 2){
      plot_list <- list()
      plot_list <- vector("list", length = 9)
      names(plot_list) <- paste0("square", 1:9)
      for (k in 1:9){
        if (! (k %in% test$test$color_categories)){next}
        filtered_test <- test
        filtered_test$test <- dplyr::filter(
          tibble::as_tibble(filtered_test$test,.name_repair="minimal"),
          color_categories == k)
        color_b <- bivariate_color_codes_b[k]
        color_f <- bivariate_color_codes_f[k]
        
        plot_topics_idx <- as.numeric(sub(".*_", "", filtered_test[["test"]]$topic))
        
        plot <- topicsPlot1(
          model = model,
          test = filtered_test,
          popout = popout,
          color_negative_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
          color_positive_cor = ggplot2::scale_color_gradient(low = color_b, high = color_f),
          grid_pos = k,
          scale_size = scale_size,
          plot_topics_idx = plot_topics_idx,
          p_alpha = p_alpha,
          highlight_topic_words = highlight_topic_words,
          save_dir = save_dir,
          figure_format = figure_format,
          width = width, 
          height = height,
          max_size = max_size, 
          seed = seed
        )
        plot_list[[k]] <- plot
      }
    }
    
    legend <- topicsGridLegend(
      bivariate_color_codes = bivariate_color_codes_f,
      filtered_test = test$test,
      cor_var = test$x_y_axis,
      save_dir = save_dir,
      figure_format = figure_format,
      seed = seed,
      y_axes_1 = dim,
      legend_title = grid_legend_title,
      legend_title_size = grid_legend_title_size,
      titles_color = grid_legend_title_color,
      legend_x_axes_label = grid_legend_x_axes_label,
      legend_y_axes_label = grid_legend_y_axes_label,
      topic_data_all = test[["test"]],
      legend_number_color = grid_legend_number_color,
      legend_number_size = grid_legend_number_size
    )
    
    plot_list[["legend"]] <- legend
    plot_list[["distribution"]] <- popout1$legend
    msg <- "The grid plot legends are saved in the save_dir."
    message(colourise(msg, "green"))
  }
  return(list(plot_list, popout1))
}
