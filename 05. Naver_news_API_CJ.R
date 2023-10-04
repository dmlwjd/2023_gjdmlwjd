#실습에 필요한 packages를 설치
install.packages("RCurl")
install.packages("XML")
install.packages("wordcloud")
install.packages("RmecabKo")

#실습에 필요한 packages를 라이브러리에 등록
library(RCurl)
library(XML)
library(wordcloud)
library(RmecabKo)

#RmecabKo package에서 사용하는 형태소 기본기능 설치(폴더 자동생성) 및 등록
install_mecab("C:/Rlibs/mecab")
library(RmecabKo)

#뉴스 API 설정(Client_ID와 Client_Secret은 개인별로 추가)
searchUrl <- "https://openapi.naver.com/v1/search/news.xml"
Client_ID <- "MUjojk_X4YplUFdLG0f8"
Client_Secret <- "uXzx3PeeYO"

#뉴스 URL 작성(UTF-8로 암호화, API 요청할 URL 정의, 검색결과는 20로 요청)
query <- URLencode(iconv("CJ", "euc-kr", "UTF-8"))
url <- paste(searchUrl, "?query=", query, "&display=20", sep="")

#문서 다운로드_URI 다운로드하기
doc_2 <- getURL(url, 
                httpheader = c('Content-Type' = "apllication/xml",
                               'X-Naver-CLient-Id' = Client_ID, 
                               'X-Naver-CLient-Secret' = Client_Secret))
doc_2

#뉴스 추출 및 단어 간 빈도 비교
#XML 문서구조로 변환
xmlFile <- xmlParse(doc_2)
#뉴스 추출
df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))
#데이터 프레임 구조
str(df)

#뉴스 내용
description <- df[,4]
description

#뉴스내용 데이터 정체
description2 <- gsub("\\d|<b>|</b>|&quot;","",description)
description2

#20개 뉴스에서 단어 추출
nouns <- nouns(iconv(description2, "utf-8"))
nouns

#각 리스트의 단어들을 하나의 벡터로 통합(use.names=F는 뉴스 원문을 제외한다는 의미)
nouns.all <- unlist(nouns, use.names=F)
nouns.all

#글자 수가 1이하인 단어 추출(분석 대상에서 제외)
nouns.all1 <- nouns.all[nchar(nouns.all) <=1]
nouns.all1

#글자 수가 2이상인 단어 추출
nouns.all2 <- nouns.all[nchar(nouns.all) >=2]
nouns.all2

#단어 빈도
nouns.freq <- table(nouns.all2)
nouns.freq

#단어 빈도를 데이터 프레임으로 생성(문자열의 경우 팩터 유형으로 하지 않음)
nouns.df <- data.frame(nouns.freq, stringsAsFactors=F)
nouns.df

#단어 빈도 역순으로 정렬
nouns.df.sort <- nouns.df[order(-nouns.df$Freq),]
nouns.df.sort

#단어의 워드 클라우드 작성
wordcloud(nouns.df.sort[,1],
          freq=nouns.df.sort[,2],
          min.freq=1,
          scale=c(3,0.7),
          rot.per=0.25,
          random.order=F,
          random.color=T,
          colors=rainbow(10))