library(tidyverse)

# Bai 1
link <- "https://raw.githubusercontent.com/rsquaredacademy/datasets/master/mock_strings.csv"

df <- read.csv(file = link)

str(df)

# Tìm các quan sát có email kết thúc bằng “.com”.
str_detect(df$email,
           pattern = "\\.com$")

str_subset(df$email,
           pattern = "\\.com$")

# Tìm các quan sát có email bắt đầu bằng chữ “v” và kết thúc bằng “.org”
x <- str_subset(string = df$email,
                pattern = "^v")

str_subset(string = x,
           pattern = "\\.org$")

str_subset(string = df$email,
           pattern = "^v.*\\.org$")

str_detect(string = df$email,
           pattern = "^v.*\\.org$")

View(df[str_detect(string = df$email,
                   pattern = "^v.*\\.org$"), ])  #View du lieu

# Tìm các quan sát có email:
# Bắt đầu bởi 6-9 chữ cái.
# Sau đó là 1 chữ số.
# Sau đó là “@”
# Kết thúc là “.org”

pattern <- "^[aA-zZ]{6,9}[0-9]@.*\\.org$" 

str_subset(df$email,
           pattern = pattern)

str_detect(df$email,
           pattern = pattern)

View(df[str_detect(df$email,
                   pattern = pattern),])

df$num <- str_count(df$email, "@")

table(df$num)

# -	Tìm các số điện thoại “phone” có dạng xxx-(xxx)xxx-xxxx với x là chữ số bất kỳ.
pattern <- "^[0-9]{3}\\-\\([0-9]{3}\\)[0-9]{3}\\-[0-9]{4}"

str_detect(df$phone,
           pattern = pattern)

View(df[str_detect(df$phone,
                   pattern = pattern),])

str_subset(df$phone,
           pattern = pattern)

# Bai 2
library(readr)
email <- read_csv("D:/MCI/Lesson 3/email.csv")

# Làm sạch tên cột trong bảng dữ liệu
library(stringr)

# Tạo vector chứa tên cột của data
name <- names(email)

str_to_lower(name) -> name
str_replace(string = name,
            pattern = " & ",
            replacement = "_") -> name

names(email) <- name

# Convert sang dạng thời gian
library(lubridate)

email$date_time <- dmy_hm(email$date_time)

## Toa cot User_text
email$user_text <- str_extract(email$user, 
                               pattern = "[aA-zZ]{3,4}")

str_extract(email$user, pattern = "[aA-zZ]+")

## Tao cot User_num
email$user_num <- str_extract(email$user, pattern = "[0-9]{3}")
str_extract(email$user, pattern = "[0-9]+")

## Tao cot chua email
email$email_check <- str_detect(str_to_lower(email$action), 
                                pattern = "@.*\\.com")

email$email_check_2 <- ifelse(str_detect(str_to_lower(email$action), 
                                       pattern = "@.*\\.com"), 
                            "Co email", "Khong email")

email$email_check_3 <- ifelse(email$email_check == TRUE, 
                              "Co email", "Khong email")

# Bai 3
text <- "practice does not make perfect, only perfect practice make perfect"

text_split <- str_split(string = text,
                        pattern = "")

count_e <- 0
count_p <- 0
for(i in 1:length(text_split[[1]])){
  if(text_split[[1]][i] == "y"){
    break
  }
  
  if(text_split[[1]][i] == "e"){
    count_e <- count_e + 1
    print(paste("e =", count_e))
  }
  
  if(text_split[[1]][i] == "p"){
    count_p <- count_p + 1
    print(paste("p =", count_p))
  }
}




