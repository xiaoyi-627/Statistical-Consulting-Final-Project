# This function cleans the raw data
#
# Args:
#   raw_wif: The raw data file
#
# Returns:
#   wif: The cleaned data file


clean_wif <- function(raw_wif) {
  # Read the raw data file
  wif <- read_excel(raw_wif)
  
  # Convert the letters of cell line and treatment to lowercase for easier cleaning
  wif <- wif %>%
    mutate_at("cell_line", tolower) %>%
    mutate_at("treatment", tolower)
  
  # Capitalize the first letter
  wif$cell_line <- gsub("wild-type", "Wild-type", wif$cell_line)
  wif$cell_line <- gsub("cell-type 101", "Cell-type 101", wif$cell_line)
  wif$treatment <- gsub("placebo", "Placebo", wif$treatment)
  wif$treatment <- gsub("activating factor 42", "Activating factor 42", wif$treatment)
  
  # Standardize the format of the name column
  wif$name <- gsub("GL-XIb", "XIb", wif$name)
  wif$name <- gsub("GL-cDZ", "cDZ", wif$name)
  wif$name <- gsub("Gl-Xib", "XIb", wif$name)
  wif$name <- gsub("GL-rjS", "rjS", wif$name)
  wif$name <- gsub("GL-Xik", "Xik", wif$name)
  wif$name <- gsub("Gl-Rjs", "rjS", wif$name)
  wif$name <- gsub("GL-cwN", "cwN", wif$name)
  wif$name <- gsub("GL-kYH", "kYH", wif$name)
  wif$name <- gsub("Gl-Cwn", "cwN", wif$name)
  wif$name <- gsub("GL-ZHw", "ZHw", wif$name)
  wif$name <- gsub("GL-MFA", "MFA", wif$name)
  wif$name <- gsub("Gl-Zhw", "ZHw", wif$name)
  
  # Return the cleaned data file
  return(wif)
}
