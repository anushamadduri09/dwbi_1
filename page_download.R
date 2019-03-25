# Load library
library(tm)
library(stringr)
library(rvest)
library(SnowballC)

#url <- "https://www.tripadvisor.ie/Hotel_Review-g304551-d301777-Reviews-ITC_Maurya_New_Delhi-New_Delhi_National_Capital_Territory_of_Delhi.html"
#url <- paste("https://www.tripadvisor.ie/Hotel_Review-g304551-d301777-Reviews-or", x, "-ITC_Maurya_New_Delhi-New_Delhi_National_Capital_Territory_of_Delhi.html", sep = "")

args<-commandArgs(TRUE)

csv_file <- args[1]
df_csv <- read.csv(csv_file, header = FALSE, sep=" ")

work_dir="practice_dir/"

for(i in 1:nrow(df_csv)){

dest_dir <- paste(work_dir,df_csv[i, 1], sep="")
dir.create(file.path(dest_dir), showWarnings = FALSE)

num_review_pages <- df_csv[i, 3]
base_url <- as.character(df_csv[i,2])
base_url <- strsplit(base_url, split="\\#")
base_url <- base_url[[1]][1]
# Web scraping live reviews of JW Marriott hotel from Trip-Advisor
df <- data.frame(Date=as.Date(character()), File=character(), User=character(), stringsAsFactors=FALSE)
x <- 0
for(i in c(1:num_review_pages)){
  if(x == 0){    
   url <- base_url
   print(url)
   file_name <- paste(dest_dir,"/scrapedpage",x,".html", sep = "")
   download.file(url, destfile = file_name, quiet=TRUE, method='auto')
   x <- x + 5
  } else{
    
   split_url = strsplit(url, split="Reviews")
   new_url <- paste(split_url[[1]][1], "Reviews-or",x,split_url[[1]][2], sep="")
   print(new_url)
   file_name <- paste(dest_dir,"/scrapedpage",x,".html", sep = "")
   download.file(new_url, destfile = file_name, quiet=TRUE, method='auto')
    x <- x + 5
  }
  reviews <- file_name %>%
    read_html() %>%
    #html_nodes("#REVIEWS .innerBubble")
    html_nodes("#REVIEWS")
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  if(nrow(df) == 0){
    df <- data.frame(id, review, stringsAsFactors = FALSE)
  }
  else{
    temp <- df
    df <- rbind(temp, data.frame(id, review, stringsAsFactors = FALSE))
  }
}

# Write the dataframe to a csv
csv_file_name <- paste(dest_dir,"/tripadvisor_reviews.csv",sep="")
write.csv(df, csv_file_name)
}
