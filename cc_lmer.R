cc_lmer <- function(target, effects, data, test = "Chisq") {
  # Überprüfen Sie, ob die Effekte eine Liste sind
  if (!is.list(effects)) {
    stop("effects must be a list of effects, see help_fct('create_compare_lmer'")
  }
  # Erstellen Sie eine Liste von Modellen mit lapply
  model_list <- lapply(effects, function(x) lme4::lmer(as.formula(paste(target, "~", x)), data = data, REML = TRUE))
  # Vergleichen Sie die Modelle mit do.call und anova
  anova_table <- do.call(anova, c(model_list, test = test))
  # Erstellen Sie eine Liste von Summaries mit lapply
  summary_list <- lapply(model_list, summary)
  # Geben Sie die ANOVA-Tabelle, die model_list und die summary_list zurück
  results <- return(list(anova_table = anova_table, model_list = model_list, summary_list = summary_list))
}
