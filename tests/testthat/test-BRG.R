testthat::skip_if(!exists("data_path"))

pipeline_output <- format_BRG(db = paste0(data_path, "/BRG_Bergen_Norway"))
