devtools::install()

devtools::load_all()

devtools::document()

folders <- list_folders('X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/testData')

data_pipeline(folders, '1m',
              false_movement = 5, zero_ranging = 5,
              create_graphics = F, clean_data = TRUE,
              rmd_file = "graphics_V5.Rmd",
              'X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/raw',
              'X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/clean')
