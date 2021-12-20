#### 네이버 영화 정보 크롤링


# "네이버 영화"에서 영화에 관한 기본 정보와 평점을 R로 수집한다
# 다음 링크에는 2,000개의 영화를 평점순으로 나열한 정보가 있다
# 링크를 이용하여 2,000개 영화의 제목과 코드를 수집한다
# 링크 : https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20211217


# 2021년 12월 17일 기준 평점이 제일 높은 영화는 "밥정"이다
# 이 영화의 세부 정보는 다음 링크에서 수집한다
# 링크 마지막의 코드만 바꾸어 다른 모든 영화의 세부 정보를 수집한다. 
# 링크 : https://movie.naver.com/movie/bi/mi/point.naver?code=186114


# rvest import
library(rvest)


## 2,000개 영화의 제목과 코드를 수집
url = "https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20211217"

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
df <- data.frame(matrix(data = c(titles, codes), ncol = 2))
names(df) <- c("title", "code")


## 위의 영화코드와 제목은 1페이지에 해당하는 50개의 영화이다
## 2000개의 영화를 수집해야하므로 1~40페이지에 대하여 위의 작업을 반복하면 된다
# 2000개의 네이버 영화제목과 영화코드를 가져와 데이터프레임으로 만드는 함수
get_moive_code_title <- function(){
    base_url <- "https://movie.naver.com/movie/sdb/rank/rmovie.naver?sel=pnt&date=20211217&page="
    code_list <- c()
    title_list <- c()
    
    for (page in 1:40) {
        url <- paste0(base_url, page)
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
    names(df) <- c("title", "code")
    return (df)
}


## 2000개의 네이버 영화제목과 영화코드를 담은 데이터프레임
df <- get_movie_code_title()
str(df)


## \t, \n, \r을 제거해주는 함수
remove_escape_sequence <- function(char){
    clean_char <- char %>% gsub("\\t", "", .) %>%
        gsub("\\n", "", .) %>% gsub("\\r", "", .) %>% .[. != ""]
    return(clean_char)
}


## 문자열 인덱싱 함수
my_substr <- function(char, start, end){
    return(substr(char, start, nchar(char)-end))
} ## 끝에 붙은 "더보기"를 제외하기 위함


# 링크 마지막의 코드만 바꾸어 다른 모든 영화의 세부 정보를 수집한다
# 그린 북 링크 : https://movie.naver.com/movie/bi/mi/point.naver?code=171539
url <- "https://movie.naver.com/movie/bi/mi/point.naver?code=171539"
html <- read_html(url, encoding = "UTF-8"); html
# url <- "https://movie.naver.com/movie/bi/mi/point.naver?code=189027"

## <head> 내부에는 필요한 정보가 없다
html <- html %>% html_node("body"); html


## 주요 정보가 "div"태그에서 "article" class에 존재한다
## "article" class안에 있는 "mv_info_area" class안에 있는
## "info_spec" class에 필요한 정보가 존재 

mv_html <- html %>% html_nodes(".article") %>%
    html_nodes(".mv_info_area") %>% 
    html_nodes(".info_spec") 


## 영화 정보(장르 -> 국가 -> 런타임 -> 개봉날짜 순서이다)
## mv_info에서 런타임, 장르, 국가, 개봉날짜 정보를 가져온다


# mv_info <- mv_html %>% html_nodes("span"); mv_info
# runtime <- mv_info %>% .[3]; runtime  
# genre <- remove_escape_sequence(mv_info[1]); genre
# country <- remove_escape_sequence(mv_info[2]); country
# release <- remove_escape_sequence(mv_info[4]); release


### 위의 코드를 통해 정보를 가져오는것은 좋지 않은 방법이다
### 왜냐면 위에 값 중 하나라도 NA면 순서가 달라지기 때문
### "info_spec" class에 step 계열의 클래스가 있다
### 그러한 class 밑에 있는 "span" 태그에 정보가 담겨있는 구조이다
## ex) "step1"은 장르, 국가, 런타임, 개봉날짜이다
## 물론 없는 step도 있다, 그런데 step1은 존재함
## 장르, 국가, 런타임, 개봉날짜 전부 없는 영화는 없을거야 아마도...


# 일단 mv_info에는 최대 4개의 xml_nodeset이 존재한다
# 첫 번째 노드는 장르, 두 번째 노드는 국가
# 세 번째 노드는 런타임, 네 번째노드는 개봉날짜를 의미한다
################## 일단 런타임은 없을 수가 없다 ####################
## 하지만 NA가 있다면 순서가 바뀌기에 정확히는 아래와 같다


## mv_info의 노드에는 런타임(3번째)을 제외하면 "span"태그가 존재한다
## "span"태그에 "href"속성에 "genre"가 포함되어 있으면 장르
## "nation"이 포함되어 있으면 국가, "open"이 포함되어 있으면 개봉날짜이다


###################영화 개요 크롤링######################
# 일단 마지막 노드에 "open"이 포함되어 있는지 확인한다
# 만약 없다면 개봉 날짜가 NA이다


# 그 다음 mv_info에 첫번째 "href"속성에 "genre"가 포함되어 있는지 확인한다
# 만약 없다면 장르가 NA이다
# 그 다음에는 첫번째 "href"속성에 "nation"이 포함되어 있는지 확인한다
# 만약 없다면 국가도 NA이다, 있으면 ok


# "genre"가 포함되어 있다면 문제가 없으니 두 번째 노드로 넘어간다
# 두 번째 "href"속성에 "nation"이 있다면 ok, 없다면 국가는 NA이다


### 문자열 포함여부 함수는 tidyverse에 있는 str_detect 함수를 써도 되지만 내가 구현해보자
str_check <- function(char, str){
    char_len <- nchar(char)
    str_len <- nchar(str)
    
    for (i in 1:(char_len-str_len+1)){
        if(substr(char, i, i+str_len-1) == str){
            return(TRUE)
        }
    }
    return(FALSE)
}

###### 영화 개요 크롤링 함수 ######
get_movie_outline <- function(naver_movie_url){
    html <- read_html(naver_movie_url, encoding = "UTF-8")
    html <- html %>% html_node("body"); html
    
    ## 영화 개요 정보가 ("div"태그에서) "article" class에 존재한다
    ## "article" class안에 있는 "mv_info_area" class안에 있는
    ## "info_spec" class에 개요 정보가 존재 
    
    mv_info_ <- html %>% html_nodes(".article") %>%
        html_nodes(".mv_info_area") %>% 
        html_nodes(".info_spec")
    
    ### 만약 "step1" class가 없다면 영화 개요도 없으니 NA를 반환
    ### 영화 개요 : 장르, 국가, 런타임, 개봉날짜
    if(length(mv_info_ %>% html_nodes(".step1")) == 0) return(rep(NA, 4)) 
    
    ## "span"노드 
    mv_info <- mv_info_ %>% html_nodes("span")
    
    len <- length(mv_info) ## 노드 길이
    mv_1 <- mv_info[1] %>% html_nodes("a") %>% html_attr("href") ## 1번째 노드(error 안생김)
    
    ### 런타임은 무조건(아마도) 존재하므로 적어도 1개 이상의 노드가 존재
    mv_outline <- mv_info %>% html_text() %>% remove_escape_sequence() ## 영화 개요 정보
    
    ### 장르, 국가, 런타임,개봉날짜 디폴트값을 NA로 설정(런타임은 혹시 모르니...) 
    genre <- NA
    country <- NA
    runtime <- NA
    release <- NA
    
    ### 런타임과 개봉날짜, release_는 길이가 2인 벡터
    ## 1번째 원소는 연도만, 2번째 원소는 8자리 완전한 날짜
    release_ <- mv_info[len] %>% html_nodes("a") %>% html_attr("href") 
    
    if(str_check(release_[1], "open") == TRUE){
        release <- mv_outline[len]
        runtime <- mv_outline[len-1]
    }
    else{
        runtime <- mv_outline[len]
    }
    
    ### 장르, 국가
    if(str_check(mv_1, "genre") == TRUE){
        genre <- mv_outline[1]
        mv_2 <- mv_info[2] %>% html_nodes("a") %>% html_attr("href") ## 2번째 노드
        ### 노드가 1개일 수 도 있으니 2번째 노드는 장르가 존재할 때 만든다
        
        if(str_check(mv_2, "nation") == TRUE){
            country <- mv_outline[2]
        }
    }
    else if(str_check(mv_1, "nation")) nation <- mv_outline[1]
    
    return(c(genre, country, runtime, release)) ## 일단은 벡터로 리턴하자
}

test <- get_movie_outline("https://movie.naver.com/movie/bi/mi/point.naver?code=171539")
test ## 일단 영화 "그린북"에 대해서는 성공적으로 크롤링함



## mv_info2는 개요 -> 감독 -> 출연 -> 등급 순서이다
## 개요는 장르 -> 국가 -> 영화시간 -> 개봉날짜 순서이다
## mv_info2에서는 감독, 출연자, 등급을 가져온다

# mv_info2 <- mv_html %>% html_nodes("dd") %>% html_text(); mv_info2
# 
# director <- mv_info2[2]; director
# actor <- my_substr(mv_info2[3], 1, 3); actor
# view_class <- my_substr(remove_escape_sequence(mv_info2[4]), 1, 3); view_class

### mv_info2도 mv_info과 같은 방식으로 함수를 구현하자


# item_name <- c(title, code, genre, country, runtime,
#                release, director, actor, view_class,
#                audience_age_10, audience_age_20, audience_age_30,
#                audience_age_40, audience_age_50, netizen_score,
#                netizen_count, ntz_male, ntz_female, ntz_10,
#                ntz_20,	ntz_30,	ntz_40,	ntz_50,	audience_score,
#                audience_count, audience_male, audience_female, 
#                audience_10, audience_20, audience_30, audience_40, audience_50)


## 영화 평점같은 숫자 정보가 "strong" 태그에 존재한다
# html %>% html_nodes("strong") %>% html_text()

## 그런데 위와 같이 하면 필요한 정보를 얻기 위해 인덱스로 접근해야 한다 
## 인덱스로 접근하는 방법의 문제점은 값이 비어있으면 순서가 달라지는 것
## 클래스로 접속한 후 태그로 접속하는 것이 더 안정적일 것 같다 

 
## "viewing_graph" class에 있는 "graph_percent" class에 관람가의 나이대 비율이 있다
age_prop <- html %>% html_nodes(".viewing_graph") %>% 
    html_nodes(".graph_percent") %>% html_text() %>% .[1:5]
age_prop

### 순서대로 10대, 20대, 30대, 40대, 50대 이상 관람가의 비율이다


## "actual_point_tab_inner" id에 있는 "star_score" class에 관람객평점 정보가 있다
actual_score <- html %>% html_nodes("#actual_point_tab_inner") %>% 
    html_node(".star_score") %>% html_text() %>% remove_escape_sequence()
actual_score


## "actual_point_tab_inner" id에 있는 "user_count" class에 관람객 참여수 정보가 있다
actual_N <- html %>% html_nodes("#actual_point_tab_inner") %>%
    html_node(".user_count") %>% html_text() 
actual_N


## "grade_netizen" class에 있는 "star_score" class에 "em" 태그에 네티즌평점 정보가 있다
ntz_scroe_ <- html %>% html_nodes(".grade_netizen") %>% 
    html_nodes(".star_score") %>% html_nodes("em") %>% 
    html_text()

ntz_scroe <- paste0(ntz_scroe_[1], ntz_scroe_[2], ntz_scroe_[3], ntz_scroe_[4])
ntz_scroe


## "grade_netizen" class에 있는 "user_count" class에 네티즌 참여수 정보가 있다
ntz_N <- html %>% html_nodes(".grade_netizen") %>%
    html_nodes(".user_count") %>% html_text()
ntz_N



# "netizen_point_graph" id에 있는
## "grp_gender" class에 있는 "graph_point" class에 남자, 여자 평점이 있다
## "grp_age" class 에 있는 "graph_point" class에 나이대별 네티즌 평점이 있다

html %>% html_nodes("#netizen_point_graph") %>% 
    html_nodes(".grp_gender") %>% html_nodes(".graph_point") %>% 
    html_text() ## 남자, 여자 평점

html %>% html_nodes("#netizen_point_graph") %>% 
    html_nodes(".grp_age") %>% html_nodes(".graph_point") %>% 
    html_text()
### 순서대로 10대, 20대, 30대, 40대, 50대 이상 네티즌 평점이다


# "actual_point_graph" id에 있는
## "grp_gender" class에 있는 "graph_point" class에 남자, 여자 평점이 있다
## "grp_age" class 에 있는 "graph_point" class에 나이대별 네티즌 평점이 있다

html %>% html_nodes("#actual_point_graph") %>% 
    html_nodes(".grp_gender") %>% html_nodes(".graph_point") %>% 
    html_text() ## 남자, 여자 평점

html %>% html_nodes("#actual_point_graph") %>% 
    html_nodes(".grp_age") %>% html_nodes(".graph_point") %>% 
    html_text() 
### 순서대로 10대, 20대, 30대, 40대, 50대 이상 네티즌 평점이다


