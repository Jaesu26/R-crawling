#### R 크롤링 연습

# install.packages("rvest")
library(rvest)

# 네이버에서 시가총액 순으로 주가 정보를 제공하는 웹 주소
url <- "https://finance.naver.com/sise/sise_market_sum.nhn"

# read_html() 함수는 주어진 웹 주소에서 html문서를 읽어서 xml 문서로 저장
html <- read_html(url, encoding = "euc-kr")

# html_nodes() 함수를 이용하여 table 노드를 선택
# table은 모두 3개
tables <- html %>% html_nodes("table") 
length(tables)

# tables를 출력하여 확인하면 다음과 같다
# 첫 번째 테이블에는 선택할 수 있는 항목들을 모아놓았다
# 두 번째 테이블에는 50개 주식회사에 대한 정보이다
# 세 번째 테이블은 페이지 네비게이션 리스트이다
# 두 번째 테이블의 데이터를 가져오겠다
# 테이블의 각 셀의 데이터는 td 태그 사이에 있다

sise <- tables[2] %>% html_nodes("td"); sise

# 회사는 총 50개인데 td는 총 681개이다
# 즉 쓸모없는 데이터가 있으니 이를 제거할 필요가 있다
# html_text() 함수로 태그 안의 데이터를 추출하고 내용을 확인한다

data <- sise %>% html_text(); data

# 각 항목에서 "\n" 또는 "\t"를 제거해야 한다
# # gsub() 함수로 "\t"를 모두 ""으로 변경하여 삭제하자
# "\"는 이스케이프 문자이므로 패턴을 기술할 때에는 "\t"는 "\\t"로 표기해야 한다

data_t <- gsub("\\t", "", data); data_t
data_n <- gsub("\\n", "", data_t); data_n

# 테이블에서 셀 값이 빈칸("")이 아닌 항목만 선택

data <- data_n[data_n != ""]
length(data)

# 개수를 확인하면 모두 600개이다
# 50으로 나누면 12이다
# 각 회사는 12개의 변수로 구성되어 있다
 
#  열의 수가 12인 행렬로 만든 후 데이터 프레임으로 변환한다
sise_df <- data.frame(matrix(data, ncol = 12, byrow = TRUE)); sise_df

# 변수 이름을 변경한다
item_name <- c("N",	"종목명", "현재가", "전일비", "등락률",
               "액면가", "시가총액", "상장주식수", 
               "외국인비율", "거래량", "PER", "ROE")

names(sise_df) <- item_name

# 숫자 안에 있는 콤마(,)삭제
for(i in c(3, 4, 6, 7, 8, 10)){
    sise_df[[i]] <- gsub(",", "", sise_df[[i]])
}

#### 회사별 상세 주식 정보 얻기
url <- "https://finance.naver.com/sise/sise_market_sum.nhn"
html <- read_html(url, encoding = "euc-kr")
tables <- html %>% html_nodes("table")
tables

# 두 번째 테이블에서 "a" 태그 안에서 속성 "href"의 값을 html_attr() 함수로 추출한다
hrefs <- tables[2] %>% html_nodes("a") %>% html_attr("href"); hrefs

# 마지막 6자리 숫자가 종목 코드이다
# substr() 함수로 마지막 숫자 6개를 추출한다
# nchar() 함수는 문자열 개수를 구한다
codes <- substr(hrefs, nchar(hrefs)-5, nchar(hrefs)); codes   

# 종목 코드가 중복되었으니 홀수 데이터만 추출하자
stock_code <- codes[c(TRUE, FALSE)]
    
## Tip
# class는 .을 앞에 붙이고 id는 #을 앞에 붙이면 된다 


