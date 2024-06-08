# This function provides the relationship plot for the conference
#
# Args:
#   wif: The cleaned data file
#
# Returns:
#   plot: A plot showing the relationship between concentration and gene expression for two cell lines

conference_wif <- function(wif) {
  # Use Times New Roman font
  font_add(family = "times",
           regular = here::here("font/Times New Roman.ttf"))
  
  # Turn on the automatic use of showtext
  showtext_auto()
  
  # Divide the data by cell line type
  wif_wild <- wif[wif$cell_line == "Wild-type", ]
  wif_cell <- wif[wif$cell_line == "Cell-type 101", ]
  
  # Relationship between concentration and gene expression for 'Wild-type' cell line
  wild <- ggplot(wif_wild, aes(x = conc, y = gene_expression, fill = treatment)) +
    geom_point(size = 3,
               shape = 21,
               color = "black") +
    labs(x = expression(paste(mu, "g/ml")),
         y = "Gene Expression",
         fill = "Treatment") +
    ggtitle("Wild-type") +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    scale_fill_manual(values = c("#78A8D1", "#D5BF98")) +
    theme_bw() +
    geom_label_repel(
      data = wif_wild %>% filter(conc > 9),
      aes(label = name),
      nudge_x = 1,
      nudge_y = 0,
      na.rm = TRUE,
      show.legend = FALSE,
      family = "times",
      size = 20
    ) +
    theme(text = element_text(family = "times", size = 65),
          legend.position = "none")
  
  # Relationship between concentration and gene expression for 'Cell-type 101' cell line
  cell <- ggplot(wif_cell, aes(x = conc, y = gene_expression, fill = treatment)) +
    geom_point(size = 3,
               shape = 21,
               color = "black") +
    labs(x = expression(paste(mu, "g/ml")),
         y = "Gene Expression",
         fill = "Treatment") +
    ggtitle("Cell-type 101") +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    scale_fill_manual(values = c("#78A8D1", "#D5BF98")) +
    theme_bw() +
    geom_label_repel(
      data = wif_cell %>% filter(conc > 9),
      aes(label = name),
      nudge_x = 1,
      nudge_y = 0,
      na.rm = TRUE,
      show.legend = FALSE,
      family = "times",
      size = 20
    ) +
    theme(text = element_text(family = "times", size = 65),
          legend.position = "none")
  
  # Combine two plots
  plot <- wild + cell +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # # Save as a TIFF file
  # ggsave(
  #   "figs/conference_plot.tiff",
  #   plot = plot,
  #   width = 9,
  #   height = 6,
  #   units = "in",
  #   dpi = 500
  # )
  
  # Turn off the automatic use of showtext
  showtext_auto(FALSE)
  
  # Return the plot
  return(plot)
}
