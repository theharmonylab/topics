# Global settings (these avoids the note (warning) "no visible binding for global variable ‘XXX’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  "Word", "phi", "scale_color_gradient", "p.value", "adjust.p_value", "adjusted_p.value", 
  "top_terms", "prevalence", "coherence", "topic_name", "label_1", 
  "mean_group_1", "mean_group_2", "seed", "cohen_d", "value"

))
