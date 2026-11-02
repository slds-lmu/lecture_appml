library(knitr)
library(dplyr)
library(kableExtra)
library(gridExtra)
library(grid)
root = rprojroot::find_root(rprojroot::is_git_root)

data = read.csv(paste0(root, "/data/ames_housing_extended.csv"), stringsAsFactors = TRUE)

# Generate foundation counts table
foundation_counts_data = data %>%
  filter(!is.na(Foundation)) %>%
  select(Foundation) %>%
  group_by(Foundation) %>%
  tally() %>%
  t() %>%
  as.data.frame()

# Save foundation counts table as PDF
pdf("../figure/foundation_counts_table.pdf", width = 10, height = 3)
grid.table(foundation_counts_data)
dev.off()

# Generate wood foundation examples table
wood_foundation_data = data %>%
  mutate(house.id  = row_number()) %>%
  select(house.id, SalePrice, Foundation) %>%
  filter(Foundation == "Wood") %>%
  t() %>%
  as.data.frame()

# Save wood foundation examples table as PDF
pdf("../figure/wood_foundation_examples.pdf", width = 10, height = 3)
grid.table(wood_foundation_data)
dev.off()

# Generate foundation encoding table
foundation_encoding_data = data %>%
  select(SalePrice, Foundation) %>%
  group_by(Foundation) %>%
  dplyr::summarize(`Foundation(enc)` = mean(SalePrice, na.rm = TRUE)) %>%
  t() %>%
  as.data.frame()

# Save foundation encoding table as PDF
pdf("../figure/foundation_encoding_table.pdf", width = 10, height = 3)
grid.table(foundation_encoding_data)
dev.off()
