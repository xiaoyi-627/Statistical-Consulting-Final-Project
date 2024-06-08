# This function performs exploratory data analysis (EDA) on the data file
#
# Args:
#   wif: The cleaned data file
#
# Returns:
#   A list containing the results of the EDA:
#     - unique_values: A list of unique values for cell line, treatment, and name
#     - skim_wif: A summary of the data frame using the skim function
#     - rjS_wif: A table of the subset of data where name is 'rjS'
#     - bar_wif: A plot showing bar charts of each subgroup in two cell lines
#     - histogram_wif: A plot showing histograms of gene expression for two cell lines
#     - relation_wild: A plot showing the relationship between concentration and gene expression for 'Wild-type' cell line
#     - relation_cell: A plot showing the relationship between concentration and gene expression for 'Cell-type 101' cell line
#     - relation: A plot showing the relationship between concentration and gene expression across different treatments and cell lines


EDA_wif <- function(wif) {
  # Store all unique values of cell line, treatment, and name
  unique_values <- list(
    cell_line = unique(wif$cell_line),
    treatment = unique(wif$treatment),
    name = unique(wif$name)
  )
  
  # Provide a broad overview of the data frame
  skim_wif <- skim(wif)
  
  # Overview of the data where name is 'rjS'
  rjS_wif <- wif[wif$name == "rjS", ] %>%
    gt() %>%
    cols_label(
      cell_line = "Cell Line",
      treatment = "Treatment",
      name = "Name",
      conc = "Concentration",
      gene_expression = "Gene Expression"
    ) %>%
    tab_header(title = "Overview of Data for 'rjS'")
  # rjS_wif %>%
  #   gtsave("tabs/rjS_wif.docx")
  
  # Provide a plot showing bar charts of each subgroup in both cell line types
  bar_wif <- wif %>%
    ggplot(aes(name, fill = treatment)) +
    geom_bar(col = "black") +
    scale_fill_hp(discrete = TRUE, option = "Ravenclaw") +
    geom_text(
      stat = "count",
      aes(label = ..count..),
      vjust = -0.5,
      size = 3,
      color = "black"
    ) +
    facet_wrap(~ cell_line, scales = "free") +
    ylim(0, 11.5) +
    labs(x = "Name", y = "Count", fill = "Treatment") +
    ggtitle("Bar Charts of Each Subgroup in Two Cell Lines") +
    theme_bw()
  
  # Provide a plot showing histograms of gene expression for both cell line types
  histogram_wif <- wif %>%
    ggplot(aes(x = gene_expression, fill = cell_line)) +
    geom_histogram(position = "identity", col = "black") +
    scale_fill_hp(discrete = TRUE, option = "Ravenclaw") +
    facet_wrap(~ cell_line, scales = "free") +
    labs(x = "Gene Expression", y = "Count", fill = "Cell Line") +
    ggtitle("Histograms of Gene Expression for Two Cell Lines") +
    theme_bw()
  
  # Divide the data by cell line type
  wif_wild <- wif[wif$cell_line == "Wild-type", ]
  wif_cell <- wif[wif$cell_line == "Cell-type 101", ]
  
  # Provide plots showing the relationship between concentration and gene expression for both cell lines
  relation_wild <- wif_wild %>%
    ggplot(aes(
      x = conc,
      y = gene_expression,
      color = treatment,
      linetype = name
    )) +
    geom_line(size = 1.5) +
    scale_colour_hp(discrete = TRUE, option = "Ravenclaw") +
    labs(
      x = "Concentration",
      y = "Gene Expression",
      color = "Group",
      linetype = "Name"
    ) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    guides(linetype = guide_legend(keywidth = unit(1.5, "cm")),
           color = guide_legend(keywidth = unit(1.5, "cm"))) +
    ggtitle("'Wild-Type' Cell Line: \n Relationship between Concentration and Gene Expression") +
    theme_bw()
  relation_cell <- wif_cell %>%
    ggplot(aes(
      x = conc,
      y = gene_expression,
      color = treatment,
      linetype = name
    )) +
    geom_line(size = 1.5) +
    scale_colour_hp(discrete = TRUE, option = "Ravenclaw") +
    labs(
      x = "Concentration",
      y = "Gene Expression",
      color = "Group",
      linetype = "Name"
    ) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    guides(linetype = guide_legend(keywidth = unit(1.5, "cm")),
           color = guide_legend(keywidth = unit(1.5, "cm"))) +
    ggtitle("'Cell-Type 101' Cell Line: \n Relationship between Concentration and Gene Expression") +
    theme_bw()
  
  # Provide plots showing the relationship between concentration and gene expression across different treatments and cell lines
  relation <- ggplot(wif, aes(x = conc, y = gene_expression)) +
    geom_point(aes(color = name), size = 3) +
    facet_grid(cell_line ~ treatment, scales = "free") +
    labs(x = "Concentration", y = "Gene Expression", color = "Name") +
    scale_colour_hp(discrete = TRUE, option = "Ravenclaw") +
    geom_smooth(
      aes(color = name, group = name),
      method = "auto",
      se = FALSE,
      linetype = "dashed"
    ) +
    ggtitle("Relationship between Concentration and Gene Expression") +
    theme_bw()
  
  # Store the results of EDA
  EDA_results <- list(
    unique_values <- unique_values,
    skim_wif <- skim_wif,
    rjS_wif <-  rjS_wif,
    bar_wif <- bar_wif,
    histogram_wif <- histogram_wif,
    relation_wild <- relation_wild,
    relation_cell <- relation_cell,
    relation <- relation
  )
  
  # # Save the results of EDA
  # ggsave("figs/bar_wif.png", plot = bar_wif)
  # ggsave("figs/histogram_wif.png", plot = histogram_wif)
  # ggsave("figs/relation_wild.png", plot = relation_wild)
  # ggsave("figs/relation_cell.png", plot = relation_cell)
  # ggsave("figs/relation.png", plot = relation)
  
  # Return the results of EDA
  return(EDA_results)
}
