# download data from tennis-data.co.uk

suppressPackageStartupMessages({
  library(rvest)
  library(readxl)
  library(httr)
  library(dplyr)
})

rm(list = ls())

# there is an error reading excel files from the web, probably due to security issues
# download files (xls, xlsx, 1 zip)
load_excel <- function(URL) {
  
  ext <- tools::file_ext(URL)
  temp_file <- tempfile(fileext = paste0(".", ext))
  GET(URL, write_disk(temp_file, overwrite = TRUE))
  
  if (ext %in% c("xls", "xlsx")) {
    data <- read_excel(temp_file, na = "N/A", .name_repair = make.names)
  } else if (ext == "zip"){
    
    unzip_dir <- tempdir()
    
    unzip(temp_file, exdir = unzip_dir)
    
    unzipped_files <- list.files(unzip_dir, full.names = TRUE)
    excel_file <- grep("/\\d{4}\\.xls$|/\\d{4}\\.xlsx$", unzipped_files, value = TRUE) 
    
    data <- if (length(excel_file) == 1) read_excel(excel_file, na = "N/A", .name_repair = make.names) else NULL
    
  } else {
    print(ext)
    data <- NULL
  }
  file.remove(temp_file)
  
  return (data)
}

url_main <- "http://www.tennis-data.co.uk/"

links <- read_html(paste0(url_main, "all_data.php")) %>% html_nodes("a") %>% html_attr("href")

excel_url <- paste0(url_main, grep("\\.xlsx$|\\.xls|\\.zip$", links, value = TRUE))

tennis_data <- lapply(excel_url, load_excel) %>% plyr::rbind.fill() %>%
  mutate(Sex = ifelse(!is.na(ATP), "M", "W"), .before = 1)

saveRDS(tennis_data, file = "./data-raw/tennis-data.rds")
