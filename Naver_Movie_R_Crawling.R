#### 네이버 영화 정보 크롤링

# "네이버 영화"에서 영화에 관한 기본 정보와 평점을 R로 수집한다
# 다음 링크에는 2,000개의 영화를 평점순으로 나열한 정보가 있다
# 링크를 이용하여 2,000개 영화의 제목과 코드를 수집한다
# 링크 : https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124

# 2021년 12월 18일 현재 평점이 제일 높은 영화는 "그린 북"이다
# 이 영화의 세부 정보는 다음 링크에서 수집한다
# 링크 마지막의 코드만 바꾸어 다른 모든 영화의 세부 정보를 수집한다. 
# 링크 : https://movie.naver.com/movie/bi/mi/point.nhn?code=171539

# rvest import
library(rvest)

## 2,000개 영화의 제목과 코드를 수집
url = "https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124"

html <- read_html(url, encoding = "UTF-8"); html ## 네이버 영화는 인코딩이 "UTF-8"로 되어있다

# 크롬 개발자 도구를 보면 영화의 제목과 코드는 table에서 "a"태그안에 존재한다
# Ctrl + Shift + c를 눌러 확인 가능하다
# "a"태그안에 "href"속성은 영화코드이고 "title"속성은 영화제목이다
## 코드
codes <- html %>% html_nodes("table")%>%
    html_nodes("a") %>% html_attr("href") ## 영화코드

codes <- codes[1:100][c(TRUE, FALSE)] ## 1~100까지가 영화코드인데 2번씩 중복되므로 하나만 취한다

# "=(등호)" 다음숫자가 영화코드에 해당한다
codes <- strsplit(codes, "=")
codes <- unlist(lapply(codes, `[[`, 2))

## 제목
titles <- html %>% html_nodes("table") %>%
    html_nodes("a") %>% html_attr("title") ## 영화제목

titles <- titles[1:100][c(TRUE, FALSE)] ## 영화코드와 마찬가지로 작업한다

# 영화제목과 코드를 합쳐 50*2 데이터프레임으로 만들기
df <- matrix(data = c(titles, codes), ncol = 2); df

## 위의 영화코드와 제목은 1페이지에 해당하는 50개의 영화이다
## 2000개의 영화를 수집해야하므로 1~40페이지에 대하여 위의 작업을 반복하면 된다
# 2000개의 네이버 영화제목과 영화코드를 가져와 데이터프레임으로 만드는 함수
get_naver_movie <- function(){
    base_url <- "https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=pnt&date=20201124"
    other_str <- "&page="
    code_list <- c()
    title_list <- c()
    
    for (page in 1:40) {
        url <- paste0(base_url, other_str, page)
        html <- read_html(url, encoding = "UTF-8")
        tag <- html %>% html_nodes("table") %>% html_nodes("a")
        
        codes <- tag %>% html_attr("href") ## 영화코드
        codes <- codes[1:100][c(TRUE, FALSE)] 
        codes <- strsplit(codes, "=")
        codes <- unlist(lapply(codes, `[[`, 2))
        code_list <- append(code_list, codes)
        
        titles <- tag %>% html_attr("title") ## 영화제목
        titles <- titles[1:100][c(TRUE, FALSE)] 
        title_list <- append(title_list, titles)
    }
    
    # 영화제목과 코드를 합쳐 데이터프레임으로 만들기
    df <- data.frame(matrix(data = c(title_list, code_list), ncol = 2))
    names(df) <- c("영화제목", "영화코드")
    return (df)
}

## 2000개의 네이버 영화제목과 영화코드를 담은 데이터프레임
df <- get_naver_movie(); str(df)

