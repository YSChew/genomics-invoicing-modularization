# requirements.R
required_packages <- c("shiny", "dplyr", "ggplot2") # Insert the necessary packages here

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}