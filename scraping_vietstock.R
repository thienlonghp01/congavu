library(tidyverse)
library(rvest)
library(RSelenium)
library(stringi)

# congavu
# con ga lan 2

# Link web
link <- "https://finance.vietstock.vn/chung-khoan-phai-sinh/chung-quyen.htm"

# Open browser
rD <- rsDriver(browser="chrome", 
               port = 164L,
               verbose=F, 
               chromever = "92.0.4515.43")
remDr <- rD[["client"]]
remDr$navigate(link)

# Reading the html content
html <- remDr$getPageSource()[[1]]

webpage <- read_html(html)

# Lấy dữ liệu bảng đầu tiên
webpage %>% 
    html_nodes(xpath = "//table[@class='table no-border-t m-b-xs table-striped']") %>%
    .[[1]] %>% 
    html_table() -> df

# Format lại tên bảng
names(df) <- names(df) %>% 
    str_replace_all(pattern = "[▲▼]", replacement = "") %>% 
    stri_trans_general(id = "Latin-ASCII") %>% 
    str_to_lower() %>% 
    str_replace_all(pattern = " ", replacement = "_")


# Chạy vòng loop
i = 2

while(i > 1){
    remDr$findElements(using = "xpath", 
                       "//ul[@id = 'list-paging']//li[@class = 'next']//i[@class = 'fa fa-angle-right']") -> x
    
    # Check nếu không có dấu tick next thì thoát loop
    if(length(x) == 0){
        break
    }
    
    # Click tick next
    remDr$findElements(using = "xpath", 
                       "//ul[@id = 'list-paging']//li[@class = 'next']//i[@class = 'fa fa-angle-right']")[[1]]$clickElement()
    
    # Sleep 2s để load web
    Sys.sleep(2)
    
    # Lấy web sau khi click
    html <- remDr$getPageSource()[[1]]
    
    webpage <- read_html(html)
    
    # Lấy dữ liệu bảng
    webpage %>% 
        html_nodes(xpath = "//table[@class='table no-border-t m-b-xs ']") %>%
        .[[1]] %>% 
        html_table() -> df1
    
    # Format lại tên cột
    names(df1) <- names(df1) %>% 
        str_replace_all(pattern = "[▲▼]", replacement = "") %>% 
        stri_trans_general(id = "Latin-ASCII") %>% 
        str_to_lower() %>% 
        str_replace_all(pattern = " ", replacement = "_")
    
    # Ghép bảng
    df <- df %>% 
        rbind(df1)
}

# Đóng trình duyệt
remDr$close()

# Kill port
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
