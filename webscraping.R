library(rvest)
library(tibble)
library(glue)
library(purrr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(plotly)
library(sendmailR)
library(wordcloud)
library(tm)
google.news<-read_html("https://news.google.com/topstories?hl=en-NG&gl=NG&ceid=NG:en")
google.news %>%
  html_nodes(".boy4he") %>%
  html_attr("aria-label")

####Test 2####
bbc<-read_html("https://www.bbc.com/")
bbc%>%
html_nodes(".media__link")%>%
html_text()
bbc

####Test 3####
url<-read_html("https://www.vanguardngr.com/2020/05/buhari-nigeria-and-the-eighth-may-29/")
text.html<-html_nodes(url,xpath="//li[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]//a | //li[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]//a | //li[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//div | //li[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a")
text.html
text.html.words<-html_text(text.html)
text.html.words
links<-html_attrs(text.html)

####Test 4####
vanguard<-read_html("https://www.vanguardngr.com/2020/05/buhari-nigeria-and-the-eighth-may-29/")
url<-vanguard %>%
html_nodes(".entry-content") %>%
html_text()  

####Test 5####
aljazeera <- read_html("https://www.aljazeera.com/indepth/opinion/indigenous-peoples-guard-lungs-planet-200610111532981.html")
opinion<-aljazeera %>%
html_nodes(".article-p-wrapper p") %>%
html_text()  

opinion

####Test 6####
businessNews<-read_html("https://businessday.ng/lead-story/article/nigerian-pension-funds-see-plunge-in-new-accounts-as-jobs-dry-up/")
businessNews.text<-businessNews %>%
  html_nodes("p") %>%
  html_text()
businessNews.text

businessNews.clean<-str_squish(businessNews.text)

businessNews.clean

cloud<-doc.cloud(businessNews.clean) #This is an algorithm for creating word cloud

write(businessNews.clean,file="business News.doc")
write.csv(businessNews.text,file="businessNews.csv")
businessNews
businessNews.urls<-businessNews %>%
  html_nodes("a") %>%
  html_attr("href")
businessNews.urls
business.df<-data.frame(Urls=businessNews.urls)
business.df
dim(business.df)
write.csv(business.df,file="businessNews.urls.csv")


####Test 7####
jumia<-read_html("https://www.jumia.com.ng/")
jumia.products<-jumia %>%
  html_nodes(".name") %>%
  html_text()

JumiaProducts<-str_squish(jumia.products)

JumiaProducts

str(JumiaProducts)
jumia.prices<-jumia %>%
  html_nodes(".prc") %>%
  html_text()

JumiaPrices<-str_squish(jumia.prices)

JumiaPrices

JumiaPrices.clean<-gsub("???","",JumiaPrices)

jumia.df<-data.frame(Prices=names(jumia.matrix))


jumia.matrix<-as.matrix(JumiaPrices.clean)



jumia.matrix
JumiaPrices.clean
str(JumiaPrices)
write.csv(jumia.matrix,file="JumiaPrices.csv")

####Test 8####
naijaquesturl<-read_html("https://naijaquest.com/problems-of-air-transportation-in-nigeria/")
naijaquesttext<-naijaquesturl %>%
  html_nodes(".entry :nth-child(1)")%>%
  html_text()

naijaquesttext

naijaquestclean<-str_squish(naijaquesttext)
naijaquestclean
write.csv(naijaquestclean,file="naijaquest.csv")
write(naijaquestclean,file="naijaquest.doc")
####Mail####

from <- "<mavisnconsulting@gmail.com>"
to <- "<mavisnconsulting@gmail.com>"
subject<-"These are the urls"
body<-businessNews.urls
control=list(smtpServer="ASPMX.L.GOOGLE.COM") #Use Google for Gmail
sendmail(from=from,to=to,subject=subject,msg=body,control=control)

####Hydroxychloroquine####
HCQurl<-read_html("https://edition.cnn.com/2020/07/31/health/hydroxychloroquine-study-henry-ford-letters/index.html")
HCQtext<-HCQurl %>%
  html_nodes(".zn-body__paragraph") %>%
  html_text()
HCQtext
HCQCLean<-str_squish(HCQtext)
HCQCLean
HCQCleaned<-gsub("\"","",HCQCLean)
HCQCleaned
HCQCleaned<-Corpus(VectorSource(HCQCleaned))
HCQCleaned<-tm_map(HCQCleaned,removePunctuation)
HCQCleaned<-tm_map(HCQCleaned,removeNumbers)
HCQCleaned<-tm_map(HCQCleaned,tolower)
HCQCleaned<-tm_map(HCQCleaned,stripWhitespace)
HCQCleaned<-tm_map(HCQCleaned,removeWords,stopwords("english"))
inspect(HCQCleaned)
tmd<-TermDocumentMatrix(HCQCleaned)
a<-as.matrix(tmd)
v<-sort(rowSums(a),decreasing=TRUE)
d<-data.frame(Words=names(v),Frequency=v)
wordcloud(d$Words,d$Frequency,min.freq=1,max.words=300,use.r.layout=FALSE,
          random.order=FALSE,scale=c(3,0.3),colors=brewer.pal(7,"Set1"))
barplot(d[1:10,]$Frequency,names.arg=d[1:10,]$Words,
        col="#3498db",border="white",xlab="Words",ylab="Frequency",
        main="Bar plot of the most frequently occurring words",beside=TRUE)

text<-head(d,10)
text %>%
  ggplot()+aes(x=Words,y=Frequency,fill=Words)+geom_bar(stat="identity")+
  scale_fill_hue(c=90)+theme_get()+labs(fill="Top frequently occuring words",title="Bar plot of the most frequently occuring words in article",caption="Source: Author")
  
####Life Expectancy####
expectancy.url<-read_html("https://www.worldometers.info/demographics/life-expectancy/")
expectancy.tab<-expectancy.url %>%
  html_node("table") %>%
  html_table()
str(expectancy.tab)
expectancy.tab
write.csv(expectancy.tab,file="Life expectancy data.csv")
LifeEx<-read.csv("Life expectancy data.csv",header=TRUE,sep=",")
LifeEx<-head(LifeEx,10)
LifeEx %>%
  ggplot()+aes(x=Country,y=Expectancy,fill=Country)+
  geom_bar(stat="identity")+scale_fill_hue(c=85)+labs(x="Countries",y="Average Life Expectancy",caption="Source: Author",title="Average Life Expectancy by Country")+
  coord_flip()

####Amazon ASICS Mens Gel-Nimbus 22 Running Shoes####

asics_url<-read_html("https://www.amazon.com/ASICS-Mens-Gel-Nimbus-Running-Shoes/product-reviews/B07X1W4VHK/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber=1:9")
asics_reviews<-asics_url %>%
  html_nodes(".review-text-content span") %>%
  html_text()

asics_reviews
asics_reviews_clean<-cleanText(asics_reviews)
asics_reviews_clean<-str_squish(asics_reviews_clean)
asics_reviews_clean

asics_links<-asics_url %>%
  html_nodes(".a-link-normal") %>%
  html_attr("href") %>%
  str_squish()

asics_links

asics_links_df<-data.frame(Links = asics_links)
asics_links_df


for(i in asics_links){
  
  read_html(i)
 asicsText<-html_nodes(".review-text-content span") %>%
    html_text()
 
 text<-asicsText
}

url<-read_html("https://www.amazon.com/ASICS-Mens-Gel-Nimbus-Running-Shoes/product-reviews/B07X1W4VHK/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber=1")

reviews<-character()

for(reviews_pages in 1:9){
  
  Link = paste0("https://www.amazon.com/ASICS-Mens-Gel-Nimbus-Running-Shoes/product-reviews/B07X1W4VHK/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber=",reviews_pages)
  
  pages = read_html(Link)
  pagereviews = pages %>% html_nodes(".review-text-content span") %>% html_text()
  
  reviews<-c(reviews,pagereviews)
  print("Scraping reviews in progress")  
}

reviews
View(reviews)

## Using Tibble, Purrr, and Glue

urls<-glue("https://www.amazon.com/ASICS-Mens-Gel-Nimbus-Running-Shoes/product-reviews/B07X1W4VHK/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber={1:9}")

scraper<-function(url){
  
  read_html(url) %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>%
    enframe("Id","Text")
  
}
productReviews<-map_dfr(urls,scraper,.id = "page")

view(productReviews)

####Covid update table####

url <- read_html("https://www.worldometers.info/coronavirus/?")

covid_table <- url %>%
  html_node("table") %>%
  html_table()

covid_table

write.csv(covid_table,file = "covidtable.csv")
