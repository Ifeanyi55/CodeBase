
trendsearch <- function(keyword,geo,time,where,interactive = FALSE){
  
  library(gtrendsR,quietly = TRUE)
  library(plotly,quietly = TRUE)
  
  if(interactive){
    
    trend <- gtrends(keyword = keyword,
                     geo = geo,
                     time = time,
                     gprop = where)
    
    itrend <- ggplotly(plot(trend))
    
    return(itrend)
    
    
  } else{
    
    trend <- gtrends(keyword = keyword,
                     geo = geo,
                     time = time,
                     gprop = where)
    
    ptrend <- plot(trend)
    
    return(ptrend)
    
  }
  
}

trendsearch(keyword = "omicron",
            geo = c("US","GB","CA","JP","NG"),
            time = "now 7-d",
            where = "news",
            interactive = TRUE)

View(countries)

