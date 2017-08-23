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
  link_join <- list(congbio_url = NA)

  for(i in 1:nrow(congress)){
    if(congress$member_name[i]!=""){
      tmp <- congress_links$.[link_counter]
      link_join$congbio_url[i] <- tmp
      link_counter <- link_counter + 1
    }else{
      tmp <- ""
      link_join$congbio_url[i] <- tmp
    }
  }

  congress_num <- list(congbio_congress = NA)
  for(x in 1:nrow(congress)){
    if(congress$`congress(year)`[x]!=""){
      tmp <- str_split_fixed(congress$`congress(year)`[x], "\\(", n=2)
      congress_num$congbio_congress[x] <- tmp[1,1]
    }else{
      tmp <- ""
      congress_num$congbio_congress[x] <- tmp
    }
  }

  link_counter <- 1
  rep_id <- list(congbio_id = NA)
  for(y in 1:nrow(congress)){
    if(congress$member_name[y]!=""){
      tmp <- str_sub(congress_links$.[link_counter], 58, 64)
      rep_id$congbio_id[y] <- tmp
      link_counter <- link_counter + 1
    }else{
      tmp <- ""
      rep_id$congbio_id[y] <- tmp
    }
  }

  proper_name <- list(join_name = NA)
  for(n in 1:nrow(congress)){
    if(congress$member_name[n]!=""){
      tmp <- congress$member_name[n] %>%
        str_to_title() %>%
        str_split(", ")
      p_name <- paste0(tmp[[1]][2], " ", tmp[[1]][1])
      proper_name$join_name[n] <- p_name
    }else{
      tmp <- ""
      proper_name$join_name[n] <- tmp
    }
  }

  birth_date <- list(congbio_birth = NA)
  death_date <- list(congbio_death = NA)
  for(d in 1:nrow(congress)){
    if(congress$member_name[d]!=""){
      tmp <- congress$`birth-death`[d] %>%
        str_split("-")
      birth_date$congbio_birth[d] <- tmp[[1]][1]
      death_date$congbio_death[d] <- tmp[[1]][2]
    }else{
      tmp <- ""
      birth_date$congbio_birth[d] <- tmp
      death_date$congbio_death[d] <- tmp
    }
  }

  congress <- congress %>%
    select(-`birth-death`, -`congress(year)`)

  colnames(congress)[colnames(congress)=="member_name"] <- "congbio_member_name"
  colnames(congress)[colnames(congress)=="position"] <- "congbio_position"
  colnames(congress)[colnames(congress)=="party"] <- "congbio_party"
  colnames(congress)[colnames(congress)=="state"] <- "congbio_state"

  congress_total <- bind_cols(congress, birth_date, death_date, congress_num, proper_name, rep_id, link_join)

  for(e in 1:nrow(congress)){
    if(congress$congbio_member_name[e]==""){
      tmp <- e - 1
      congress_total$congbio_member_name[e] <- congress_total$congbio_member_name[tmp]
      congress_total$join_name[e] <- congress_total$join_name[tmp]
      congress_total$congbio_id[e] <- congress_total$congbio_id[tmp]
      congress_total$congbio_url[e] <- congress_total$congbio_url[tmp]
    }

    congress_total$congbio_member_name[e] <- str_to_title(congress_total$congbio_member_name[e])
  }

#   if(is.null(combined[1,1])){
#     combined <- congress_total
#   }else{
#     combined <- bind_rows(combined, congress_total)
#   }

  congress_reps <- congress_total %>%
    filter(congbio_position=="Representative" | congbio_position == "Senator")

  if(is.null(combined[1,1])){
    combined <- congress_reps
  }else{
    combined <- bind_rows(combined, congress_reps)
  }
}

write_csv(combined, path = "data/congressional_bio_winners.csv")
