library(data.table)
library(stringr)
library(magrittr)
library(rvest)

scrape_gradcafe <- function(page = 1, subject = "biostatistics") {
  url <- paste0("https://thegradcafe.com/survey/index.php?q=", subject, "&t=a&pp=250&o=&p=", page)
  webpage <- read_html(url)
  
  web_text <- webpage %>% html_nodes("td") %>% html_text()
  res_dt <- as.data.table(matrix(web_text, ncol = 6, byrow = T))
  
  names(res_dt) <- as.vector(as.matrix(res_dt)[1,])
  res_dt <- res_dt[-1]
  res_dt[, Program := gsub("\\,.*", "", `Program (Season)`)]
  res_dt[, Season := str_extract(tolower(`Program (Season)`), "\\(f[0-9]+\\)|\\(s[0-9]+\\)")]
  res_dt[, Degree := trimws(gsub("\\(f[0-9]+\\)|\\(s[0-9]+\\)", "", gsub(".*\\,", "", tolower(`Program (Season)`))))]
  res_dt[, Decision := gsub("\\ via.*", "", tolower(`Decision & Date`))]
  res_dt[, ContactMode := gsub("\\ on.*", "", gsub(".*via\\ ", "", `Decision & Date`))]
  res_dt[, DateContacted := str_extract(tolower(`Decision & Date`), "[0-9]+\\ [a-z]{3}\\ [0-9]{4}")]
  res_dt[, GRE := str_extract(`Decision & Date`, "[0-9]{3}\\/[0-9]{3}\\/[0-9]\\.[0-9]{2}")]
  res_dt[, V := substr(GRE, 1, 3)]
  res_dt[, Q := substr(GRE, 5, 7)]
  res_dt[, W := substring(GRE, 9)]
  res_dt[, GRE_Sub := str_extract(str_extract(`Decision & Date`, "Subject\\:\\ .*"), "[0-9]{3}")]
  res_dt[, GPA := gsub("GPA\\:\\ ", "", str_extract(`Decision & Date`, "GPA\\:\\ [0-9]\\.[0-9]+"))]
  return(res_dt)
}

gradcafe_data <- rbindlist(lapply(1:12, scrape_gradcafe, subject = "biostatistics"))
gradcafe_data <- rbind(gradcafe_data, scrape_gradcafe(1, "biostat"))
saveRDS(gradcafe_data, "Data/gradcafe_data.RDS")
