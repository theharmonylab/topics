# Global settings (these avoids the note (warning) "no visible binding for global variable ‘XXX’
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c(
  "Word", "phi", "scale_color_gradient", 
  
  "p.value", "adjust.p_value", "adjusted_p.value", 
  
  "top_terms", "prevalence", "coherence", "topic_name", "label_1", 
  "mean_group_1", "mean_group_2", "seed", "cohen_d", "value",
  "phi_max_size", "test_path", 
  
  ".", ".data", "color_categories", "contains_only_five", "fill", "group", "left_join",
  
  "map_num", "mean_value", "prevalence", "rename", "topic", "topic_number", "x", "y",
  
  # generate_scatter_plot
  "geom_text", 
  
  # topicsTest
  "binomial", 
  
  # topicsScatterLegend
  "contains_only", "label_content",
  
  # General
  "msg2",
  
  # create_plots
  "color",
  
  "color_categories1",
  
  # topicsDtmEval
  "nr", "term", "freq",
  
  # topicsGrams
  "n", "component_prob", "joint_prob", "n_gram_type", "ngrams", "pmi_value", ":=",
  
  # topicsPlot
  "pmi", "color_negative_cor", "color_positive_cor", "estimate", "highlight_topic_words",
  "p_adjusted"
  
))
