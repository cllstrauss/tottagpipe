devtools::install()
devtools::document()

devtools::load_all()

folders <- list_folders('X:/Daily_2/ABC/CARE/TotTag Files')

attr(folders, "one_kb_files")
attr(folders, "invalid_names")

folders_run <- folders[81:85]
folders_run

data_pipeline(folders_run, '12m',
              false_movement = 5, zero_ranging = 5,
              create_graphics = T, clean_data = T,
              rmd_file = "graphics_V5.Rmd",
              'X:/Daily_2/ABC/tottag R code/Chris Processed/CARE Data/Raw Data',
              'X:/Daily_2/ABC/tottag R code/Chris Processed/CARE Data/Quality Check',
              'X:/Daily_2/ABC/tottag R code/Chris Processed/CARE Data/Clean Data')

#processing logs for will
folders <- list_folders('X:/Daily_2/ABC/tottag R code/Chris Processed/Log Batch Test/1-Month Data')

data_pipeline(folders, '1m',
              false_movement = 5, zero_ranging = 5,
              create_graphics = T, clean_data = TRUE,
              rmd_file = "graphics_V5.Rmd",
              'X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/raw',
              'X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/graphics',
              'X:/Daily_2/ABC/tottag R code/Chris Processed/Batch Test/clean')
