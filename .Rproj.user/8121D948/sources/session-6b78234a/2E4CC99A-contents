# Load packages required to define the pipeline
library(targets)

# Load other packages as needed
library(tarchetypes)

# Set target options
tar_option_set(
  # Packages that targets need for their tasks
  packages = c(
    "tidyverse",
    "gt",
    "quarto",
    "readxl",
    "skimr",
    "ggrepel",
    "pwr",
    "ggResidpanel",
    "lme4",
    "lmerTest",
    "merTools",
    "harrypotter",
    "ggplot2",
    "showtext",
    "patchwork",
    "tidyverse"
  ),
  # Set the default storage format
  format = "rds"
)

# Run the R scripts with custom functions
tar_source()

# Replace the target list below with your own:
list(
  tar_file(raw_wif, "raw-data/2024-03-04-WIF-tis4d.xlsx"),
  tar_target(cleaned_wif, clean_wif(raw_wif)),
  tar_target(EDA_results_wif, EDA_wif(cleaned_wif)),
  tar_target(conference_plot, conference_wif(cleaned_wif)),
  tar_target(sample_size_results_wif, sample_size_wif(5, 0.1, 0.9, 0.05)),
  tar_target(model_results_wif, model_wif(cleaned_wif)),
  tar_quarto(README, "README.qmd")
)
