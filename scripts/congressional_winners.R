library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(tidyverse)

combined <- data.frame()
filenames <- dir("data-raw/congbio-html/")

for(item in 1:length(filenames)){
  html_file <- read_html(paste0("data-raw/congbio-html/", filenames[item]))
  html_list <- html_table(html_file)
  congress <- html_list[[2]]

  names(congress) <- names(congress) %>%
    str_to_lower() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("\\s", "_")

  congress_links <- html_file %>%
    html_nodes('a') %>%
    html_attr("href") %>%
    data_frame()

  link_counter <- 1
  link_join <- list(bio_url = NA)

  for(i in 1:nrow(congress)){
    if(congress$member_name[i]!=""){
      tmp <- congress_links$.[link_counter]
      link_join$bio_url[i] <- tmp
      link_counter <- link_counter + 1
    }else{
      tmp <- ""
      link_join$bio_url[i] <- tmp
    }
  }

  congress_num <- list(congress_number = NA)
  for(x in 1:nrow(congress)){
    if(congress$`congress(year)`[x]!=""){
      tmp <- str_split_fixed(congress$`congress(year)`[x], "\\(", n=2)
      congress_num$congress_number[x] <- tmp[1,1]
    }else{
      tmp <- ""
      congress_num$congress_number[x] <- tmp
    }
  }


  link_counter <- 1
  rep_id <- list(cong_bio_id = NA)
  for(y in 1:nrow(congress)){
    if(congress$member_name[y]!=""){
      tmp <- str_sub(congress_links$.[link_counter], 58, 64)
      rep_id$cong_bio_id[y] <- tmp
      link_counter <- link_counter + 1
    }else{
      tmp <- ""
      rep_id$cong_bio_id[y] <- tmp
    }
  }

  congress_total <- bind_cols(congress, link_join, rep_id)

  if(is.null(combined[1,1])){
    combined <- congress_total
  }else{
    combined <- bind_rows(combined, congress_total)
  }
}

