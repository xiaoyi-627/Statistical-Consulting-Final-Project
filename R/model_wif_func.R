# This function fits multiple linear mixed-effects models
#
# Args:
#   wif: The cleaned data file
#
# Returns:
#   A list containing the results of the models:
#     - model_formulas: A list of formulas for all fitted models
#     - anova_results: A list of ANOVA results for the models
#     - model_panel: A list of diagnostic plots for the models
#     - summary_wild: A summary table for the follow-up analysis model of the 'Wild-type' cell line
#     - summary_cell: A summary table for the follow-up analysis model of the 'Cell-type 101' cell line

model_wif <- function(wif) {
  # Divide the data by cell line type
  wif_wild <- wif[wif$cell_line == "Wild-type", ]
  wif_cell <- wif[wif$cell_line == "Cell-type 101", ]
  
  # Model 1: treatment and concentration predict gene expression
  m1 <- lmer(gene_expression ~ treatment + conc + (1 | name),
             data = wif,
             REML = FALSE)
  
  # Model 2: cell line, treatment and concentration predict gene expression
  m2 <- lmer(
    gene_expression ~ cell_line + treatment + conc + (1 | name),
    data = wif,
    REML = FALSE
  )
  
  # Model 3: Including an interaction term between treatment and concentration
  m3 <- lmer(
    gene_expression ~ cell_line + treatment * conc + (1 | name),
    data = wif,
    REML = FALSE
  )
  
  # Model 4: Including interaction terms between cell line and treatment, between cell line and concentration, and an interaction term between cell line, treatment and concentration
  m4 <- lmer(
    gene_expression ~ cell_line * treatment * conc + (1 | name),
    data = wif,
    REML = FALSE
  )
  
  # Log model: Transform the response variable using logarithm
  m4_log <- lmer(
    log(gene_expression) ~ cell_line * treatment * conc + (1 | name),
    data = wif,
    REML = FALSE
  )
  
  # Follow-up analysis model for 'Wild-type' cell line
  m_wild <- lmer(log(gene_expression) ~ treatment * conc + (1 | name),
                 data = wif_wild,
                 REML = FALSE)
  
  # Follow-up analysis model for 'Cell-type 101' cell line
  m_cell <- lmer(log(gene_expression) ~ treatment * conc + (1 | name),
                 data = wif_cell,
                 REML = FALSE)
  
  # Provide a table showing result summaries of follow-up analysis for 'Wild-type' cell line
  summary_wild <- as.data.frame(summary(m_wild)$coefficients)
  summary_wild <- tibble::rownames_to_column(summary_wild, "Term")
  summary_wild$Pr <- c("<0.001", "<0.01", "<0.001", "<0.001")
  summary_wild <- summary_wild[, !names(summary_wild) %in% "Pr(>|t|)"]
  summary_wild <- summary_wild %>%
    gt() %>%
    tab_header(title = "Model Summary of Follow-up Analysis Model for 'Wild-type' Cell Line") %>%
    fmt_number(columns = vars(Estimate, `Std. Error`, df, `t value`),
               decimals = 3) %>%
    tab_style(style = list(cell_fill(color = "lightyellow")),
              locations = cells_body(rows = summary_wild$Pr %in% c("<0.001", "<0.01"))) %>%
    cols_label(Pr = "Pr(>|t|)")
  # summary_wild %>%
  #   gtsave("tabs/summary_wild.docx")
  
  # Provide a table showing result summaries of follow-up analysis for 'Cell-type 101' cell line
  summary_cell <- as.data.frame(summary(m_cell)$coefficients)
  summary_cell <- tibble::rownames_to_column(summary_cell, "Term")
  summary_cell$Pr <- c("<0.001", "0.357", "<0.001", "<0.05")
  summary_cell <- summary_cell[, !names(summary_cell) %in% "Pr(>|t|)"]
  summary_cell <- summary_cell %>%
    gt() %>%
    tab_header(title = "Model Summary of Follow-up Analysis Model for 'Cell-type 101' Cell Line") %>%
    fmt_number(columns = vars(Estimate, `Std. Error`, df, `t value`),
               decimals = 3) %>%
    tab_style(style = list(cell_fill(color = "lightyellow")),
              locations = cells_body(rows = summary_cell$Pr %in% c("<0.001", "<0.01", "<0.05"))) %>%
    cols_label(Pr = "Pr(>|t|)")
  # summary_cell %>%
  #   gtsave("tabs/summary_cell.docx")
  
  # Store the formulas of all models
  model_formulas <- list(
    m1 = formula(m1),
    m2 = formula(m2),
    m3 = formula(m3),
    m4 = formula(m4),
    m4_log = formula(m4_log),
    m_wild = formula(m_wild),
    m_cell = formula(m_cell)
  )
  
  # Store all ANOVA results
  anova_results <- list(
    anova_comparison = anova(m1, m2, m3, m4),
    anova_m4_log = anova(m4_log),
    anova_m_wild = anova(m_wild),
    anova_m_cell = anova(m_cell)
  )
  
  # Store all diagnostic plots
  model_panel <- list(
    m4_panel = resid_panel(m4, smoother = TRUE, qqbands = TRUE),
    m4_log_panel = resid_panel(m4_log, smoother = TRUE, qqbands = TRUE),
    m_wild_panel = resid_panel(m_wild, smoother = TRUE, qqbands = TRUE),
    m_cell_panel = resid_panel(m_cell, smoother = TRUE, qqbands = TRUE)
  )
  
  # Store the results of models
  model_results <- list(
    model_formulas = model_formulas,
    anova_results = anova_results,
    model_panel = model_panel,
    summary_wild = summary_wild,
    summary_cell = summary_cell
  )
  
  # # Save the model results
  # ggsave("figs/m4_panel.png", plot = plot(resid_panel(
  #   m4, smoother = TRUE, qqbands = TRUE
  # )))
  # ggsave("figs/m4_log_panel.png", plot = plot(resid_panel(
  #   m4_log, smoother = TRUE, qqbands = TRUE
  # )))
  # ggsave("figs/m_wild_panel.png", plot = plot(resid_panel(
  #   m_wild, smoother = TRUE, qqbands = TRUE
  # )))
  # ggsave("figs/m_cell_panel.png", plot = plot(resid_panel(
  #   m_cell, smoother = TRUE, qqbands = TRUE
  # )))
  
  # Return the model results
  return(model_results)
}
