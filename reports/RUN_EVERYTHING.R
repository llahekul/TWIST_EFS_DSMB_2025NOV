library(quarto)
library(officer)
library(magrittr)

# Set working directory
setwd("C:/Users/luke_hall/OneDrive - Edwards Lifesciences/lhall/TWIST_EFS_DSMB_2025NOV/reports")

# Step 1: Render Quarto file to Word
quarto_render("TWIST_EFS_ROP_TLFs.qmd", output_format = "docx")

# Step 2: Define paths
input_doc <- "TWIST_EFS_ROP_TLFs.docx"
output_doc <- "TWIST_EFS_ROP_TLFs_with_logo.docx"
logo_path <- "edwards_logo.png"

# Step 3: Create a new document using the original as a reference template
doc <- read_docx(path = input_doc)  # this preserves styles

# Step 4: Remove existing body content (to avoid duplication)
doc <- body_remove(doc)

# Step 5: Insert logo at the top (centered)
doc <- doc %>%
  body_add_fpar(
    fpar(
      external_img(src = logo_path, width = 2, height = 1),
      fp_p = fp_par(text.align = "center")
    )
  ) %>%
  body_add_par("", style = "Normal")  # spacing

# Step 6: Append original content (preserving styles)
doc <- body_add_docx(doc, src = input_doc)

# Step 7: Save final document
print(doc, target = output_doc)
