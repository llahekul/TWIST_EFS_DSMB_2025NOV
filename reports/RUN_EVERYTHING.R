library(officer)
library(magrittr)
library(quarto)

setwd("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/reports")

# run .qmd file 
quarto_render("TWIST_EFS_ROP_TLFs.qmd", output_format = "docx")

# Define file paths
cover_path <- "template.docx"
report_path <- "TWIST_EFS_ROP_TLFs.docx"  # This is the output from quarto_render
output_path <- "TWIST_EFS_ROP_TLFs_FINAL.docx"

# Read in both documents
cover_doc <- read_docx(cover_path)
report_doc <- read_docx(report_path)

# Combine them: add report content to cover
final_doc <- cover_doc %>% body_add_docx(src = report_path)

# Save the merged document
print(final_doc, target = report_path)
