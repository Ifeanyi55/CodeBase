library(udpipe)
library(tm)
library(textrank)
library(SentimentAnalysis)
library(reactable)
library(dplyr)
library(ggplot2)
library(wordcloud2)
library(RColorBrewer)
library(plotly)

cleanText<-function(text){
  a<-gsub("-","",text)
  b<-gsub("/","",a)
  c<-gsub("_","",b)
  d<-gsub("&","",c)
  e<-gsub("%","",d)
  f<-gsub("???","",e)
  g<-gsub("http[^[:space:]]*","",f)
  h<-gsub("\\d","",g)
  i<-gsub("[[:cntrl:]]","",h)
  j<-gsub("@\\S+","",i)
  
  
  
  return(j)
  
}


## Arthritis Bios

mystopwords <- readLines("stopwords.txt")
arthritis <- read.csv("arthritis_bios.csv")
arthritis_clean <- cleanText(arthritis$Text)
arthritis_clean

## Convert to corpus

arthritis_corp <- Corpus(VectorSource(arthritis_clean))

## Clean corpus
arthritis_corp <- arthritis_corp %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(removeWords,mystopwords)

## Convert to character vector

arthritis_bios <- as.character(arthritis_corp)

## Gram_graph

gram_graph <- function(data,char_number = 2,color = "lightblue",plot_title){
  udmodel <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  data_annotate <- udpipe_annotate(udmodel,data)
  data_df <- data.frame(data_annotate)
  data_rake <- keywords_rake(data_df,term = "lemma",group = "doc_id",
                             relevant = data_df$upos %in% c("NOUN","ADJ"))
 
   rake_filter <- data_rake %>%
    filter(nchar(keyword) >= char_number)
  
  f <- head(rake_filter,50)
  
  d <- data.frame(
    Keyword = reorder(f$keyword,+f$rake),
    Score = f$rake
  )
  
  gg_d <- d %>%
    ggplot()+aes(x=Keyword,y=Score)+geom_bar(stat = "identity",fill=color)+
    scale_y_continuous(expand = c(0,0))+theme(
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold",size = 10),
      axis.text.y = element_text(hjust = 0.7),
      axis.title = element_text(face = "bold",size = 12),
      plot.title = element_text(face = "bold",size = 13)
    )+labs(title = plot_title)+coord_flip()
  
  g <- ggplotly(gg_d)
  
  return(g)
}

## Rake frequency graph function

gram_freq <- function(data,frequency = 3,word_number = 2,color = "lightblue",plot_title){
  udmodel <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  data_annotate <- udpipe_annotate(udmodel,data)
  data_df <- data.frame(data_annotate)
  data_rake <- keywords_rake(data_df,term = "lemma",group = "doc_id",
                             relevant = data_df$upos %in% c("NOUN","ADJ"))
  
  rake_filter <- data_rake %>%
    mutate(countWords(keyword)) %>%
    filter(WordCount >= 2)
  
  e <- head(rake_filter,50)
  
  g <- e %>%
    filter(freq >= frequency) 
  
  h <- g %>%
    arrange(desc(freq))
    
  f <- head(h,10)
  
  d <- data.frame(
    Keyword = reorder(f$keyword,+f$freq),
    Frequency = f$freq
  )
  
  gg_d <- d %>%
    ggplot()+aes(x=Keyword,y=Frequency)+geom_bar(stat = "identity",fill=color)+
    scale_y_continuous(expand = c(0,0))+theme(
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold",size = 10),
      axis.text.y = element_text(hjust = 0.7),
      axis.title = element_text(face = "bold",size = 12),
      plot.title = element_text(face = "bold",size = 13)
    )+labs(title = plot_title)+coord_flip()
  
  g <- ggplotly(gg_d)
  
  return(g)
}



## Table output function

freq_table <- function(data,char_number = 2,color_head = "lightblue",color_foot = "lightblue"){
  udmodel <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  data_annotate <- udpipe_annotate(udmodel,data)
  data_df <- data.frame(data_annotate)
  data_rake <- keywords_rake(data_df,term = "lemma",group = "doc_id",
                             relevant = data_df$upos %in% c("NOUN","ADJ"))
  
  rake_filter <- data_rake %>%
    filter(nchar(keyword) >= char_number)
  
  f <- head(rake_filter,50)
  
  d <- data.frame(
    Keyword = f$keyword,
    Frequency = f$freq
  )
  
t <- reactable(d,searchable = TRUE,compact = TRUE,highlight = TRUE,resizable = TRUE,
               showPageSizeOptions = TRUE,pageSizeOptions = c(10,30,50),defaultPageSize = 10,
               bordered = TRUE,striped = TRUE,defaultColDef = colDef(
                 align = "center",
                 headerStyle = list(background = color_head),
                 footerStyle = list(background = color_foot)
               ))
  
  return(t)
}




arthritis_gram <- gram_graph(data = arthritis_bios,plot_title = "Most Relevant Bi-grams in Arthritis Bios")
arthritis_gram

arthritis_rake_freq <- gram_freq(arthritis_bios,color = "lightblue",plot_title = "Most Frequently Occuring Bi-grams")
arthritis_rake_freq

arthritis_bigram_freq <- freq_table(arthritis_bios,color_head = "lightblue",color_foot = "lightblue")
arthritis_bigram_freq

## CIO Bios

cio <- read.csv("CIO_bios.csv")
cio_clean <- cleanText(cio$Text)

## Convert to corpus

cio_corp <- Corpus(VectorSource(cio_clean))

## Clean corpus

cio_corp <- cio_corp %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(removeWords,mystopwords)

## Convert to character vector

cio_bio <- as.character(cio_corp)

## Extract and plot bi-grams

CIO <- gram_graph(cio_bio,color = "#bb8fce",plot_title = "Most Relevant Bi-grams in CIO Bios")
CIO

CIO_rake_freq <- gram_freq(cio_bio,color = "#bb8fce",plot_title = "Most Frequently Occuring Bi-grams")
CIO_rake_freq

CIO_bigram_freq <- freq_table(cio_bio,color_head = "#bb8fce",color_foot = "#bb8fce")
CIO_bigram_freq

## Cultuurfilosoof Bios

cult <- read.csv("cultuurfilosoof_bios.csv")
cult_clean <- cleanText(cult[['Text']])

## Convert to corpus
dutchstop <- readLines("stopwords_nl.txt")
cult_corp <- Corpus(VectorSource(cult_clean))

## Clean corpus

cult_corp <- cult_corp %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(removeWords,mystopwords) %>%
  tm_map(removeWords,dutchstop)

## Convert to character vector

cult_bios <- as.character(cult_corp)

## Extract and plot bi-grams

cultuur <- gram_graph(data = cult_bios,color = "#58d68d",plot_title = "Most Relevant Bi-grams in Cultuurfilosoof Bios")
cultuur

cultuur_rake_freq <- gram_freq(cult_bios,color = "#58d68d",plot_title = "Most Frequently Occuring Bi-grams")
cultuur_rake_freq

## Seafarer Bios

sea <- read.csv("seafarer_bios.csv")
sea_clean <- cleanText(sea[["Text"]])

## Convert to corpus

sea_corp <- Corpus(VectorSource(sea_clean))

## Clean corpus

sea_corp <- sea_corp %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(removeWords,mystopwords) %>%
  tm_map(removeWords,dutchstop)

## Convert to character vector

sea_bios <- as.character(sea_corp)

## Extract and plot bi-grams

seafarers <- gram_graph(data = sea_bios,color = "#ec7063",
                        plot_title = "Most Relevant Bi-grams in Seafarer Bios")
seafarers

seafarers_rake_freq <- gram_freq(sea_bios,color = "#ec7063",plot_title = "Most Frequently Occurring Bi-grams")
seafarers_rake_freq


## TextRank algorithm

# Load model and annotate text

udmodel <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
annotate_arthritis_bios <- udpipe_annotate(udmodel,arthritis_bios)
df_arthritis_bios <- as.data.frame(annotate_arthritis_bios)

# Deploy TextRank algorithm

arthritis_text_rank <- textrank_keywords(df_arthritis_bios$lemma,
                                         relevant = df_arthritis_bios$upos %in% c("NOUN","ADJ"),
                                         ngram_max = 8, sep = " ")
arthritis_text_rank_sub <- subset(arthritis_text_rank$keywords,ngram >= 2)
arthritis_text_rank_sub <- data.frame(id = 1:nrow(arthritis_text_rank_sub),
                                      keyword = arthritis_text_rank_sub$keyword,
                                      ngram = arthritis_text_rank_sub$ngram,
                                      freq = arthritis_text_rank_sub$freq)
arthritis_text_rank$pagerank
View(arthritis_text_rank_sub)

top50_arthritis_keywords <- head(arthritis_text_rank_sub,50)
top50_arthritis_keywords


top50_arthritis_keywords %>%
  ggplot()+aes(x = reorder(keyword,+freq),y = freq)+geom_bar(stat = "identity",fill="lightblue")+
  coord_flip()+scale_y_continuous(expand = c(0,0))+theme(axis.ticks = element_blank())

write.csv(top50_arthritis_keywords,file = "arthritis_bios_textrank.csv")

arthritis_rank_df <- data.frame(
  Keywords = arthritis_text_rank_sub$keyword,
  Frequency = arthritis_text_rank_sub$freq
)

wordcloud2(arthritis_rank_df,shape = "hexagon",size = 1,color = c("skyblue","red"),backgroundColor = "white")
RColorBrewer::brewer.pal.info

## Seafarers bios
annotate_sea_bios <- udpipe_annotate(udmodel,sea_bios)
df_seafarers <- as.data.frame(annotate_sea_bios)

## TextRank algorithm

sea_rank <- textrank_keywords(df_seafarers$lemma,relevant = df_seafarers$upos %in% c("NOUN","ADJ"),
                              ngram_max = 2,sep = " ")

sea_rank$keywords_by_ngram


sea_rank_sub <- subset(sea_rank$keywords,ngram >= 2)
sea_rank_sub

sea_rank_sub_df <- data.frame(
  id = 1:nrow(sea_rank_sub),
  keyword = sea_rank_sub$keyword,
  ngram = sea_rank_sub$ngram,
  freq = sea_rank_sub$freq
)

sea_rank_sub_df
sea_sub_df <- head(sea_rank_sub_df,50)

sea_sub_df %>%
  ggplot()+aes(x=reorder(keyword,+freq),y=freq)+geom_bar(stat = "identity",fill="maroon")+
  coord_flip()+scale_y_continuous(expand = c(0,0))+theme(axis.ticks = element_blank())
