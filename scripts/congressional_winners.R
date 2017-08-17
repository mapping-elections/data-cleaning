library(rvest)
library(dplyr)
library(xml2)
library(tidyverse)

html <- read_html("data-raw/congbio-html/congress01.html")
html_list <- html_table(html)
congress <- html_list[[2]]

test <- xml_find_all(".//a", html[[2]])
