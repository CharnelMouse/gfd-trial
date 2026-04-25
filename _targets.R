library(targets)
library(tarchetypes)

tar_source()

list(
  tar_quarto(
    report,
    "report.qmd"
  )
)
