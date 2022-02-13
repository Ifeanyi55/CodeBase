library(rgdal)
library(leaflet)
library(maptools)
library(mapdata)
library(rgeos)
library(sf)
library(geojsonio)
library(raster)
library(rtweet)
library(jsonlite)

## rtweet authentification

appname <- "Social Media Researcher"
twitter_token <- create_token(
  app = appname,
  consumer_key = "rs0pgh8zPkmGdjs6DGJwMd4Kl",
  consumer_secret = "mkiXsVMgcvRMrX0J37B9sB55gWX6erUSsg3BYV877rtvQd50Cy",
  access_token = "622691806-cKVY9ThSyzvmboLZ4vye8mZwhTAHi4bmGU6xQzHW",
  access_secret = "Bl9n6EHJrqdIwHM5zCSi9bnmHhf4OZCSSwt91CNAW2Hu3"
)
b <- search_tweets("binance",n = 100)
View(b)

write_as_csv(b,file_name = "binance.csv",fileEncoding = "UTF-8")
## Data source
states <- geojson_read("states.geojson",what = "sp")

states$STATEFP
states$STUSPS

## Check data class
class(states)

names(states)

## Basic leaflet map

mapUS <- leaflet(states) %>% 
  setView(-96,37.8,4) %>% 
  addTiles()

mapUS %>% addPolygons()

## Convert STATEFP into integers

states$STATEFP <- as.numeric(states$STATEFP)
str(states$STATEFP)

## Add color by STATEFP

bins <- c(0,10,20,50,100,200,500,1000,Inf)
pal <- colorBin("YlOrRd",domain = states$STATEFP,bins = bins)

mapUS %>% addPolygons(
  fillColor = ~pal(states$STATEFP),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
)

## Add interaction with "highlight" argument in addPolygons

bins <- c(0,10,20,50,100,200,500,1000,Inf)
pal <- colorBin("YlOrRd",domain = states$STATEFP,bins = bins)

mapUS %>% 
  addPolygons(
    fillColor = ~pal(states$STATEFP),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
 )


## Add custom info

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$NAME,states$STATEFP
) %>% lapply(htmltools::HTML)

mapUS %>% 
  addPolygons(
    fillColor = ~pal(states$STATEFP),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight"= "normal",padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )

## Add legend

mapUS %>% 
  addPolygons(
    fillColor = ~pal(states$STATEFP),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight"= "normal",padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>% 
  addLegend(pal = pal,values = ~states$STATEFP,opacity = 0.7,
            title = "States FP",position = "bottomright")

## Add html title

htmltitle <- "<h3> Map Data of US States </h3>"
source <- "<h5> Data Sourced from GEO web </>"

mapUS %>% 
  addPolygons(
    fillColor = ~pal(states$STATEFP),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight"= "normal",padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>% 
  addLegend(pal = pal,values = ~states$STATEFP,opacity = 0.7,
            title = "States FP",position = "bottomright") %>% 
  addControl(html = htmltitle,position = "topright") %>% 
  addControl(html = source,position = "bottomleft")


## Nigeria population

nigeria_pop <- readxl::read_excel("Nigeria.xlsx")
nigeria_pop$Population

nigeria <- geojson_read("nigeria.geojson",what = "sp")

class(nigeria)
names(nigeria)

nigeria$state
nigeria$capcity
nigeria$source
nigeria$timestamp
nigeria$globalid
nigeria$shape_area
nigeria$shape_len
nigeria$geozone
nigeria$cartodb_id
nigeria$created_at
nigeria$updated_at



nigeriaT <- spTransform(nigeria,CRS("+proj=longlat +datsum=WGS84"))

class(nigeriaT)
names(nigeriaT)

bins <- c(0,10,20,50,100,200,500,1000,Inf)
pal <- colorBin("YlGnBu",domain = nigeria_pop$Population,bins = bins)

labels <- paste(
  "States :",nigeria$state, "<br/>",
  "Population:",nigeria_pop$Population, "<br/>"
) %>% lapply(htmltools::HTML)

title <- "<h3> 2015 Population of Nigeria Distributed by State</h3>"
source <- "<h4> Source: Nigerian Census Portal </h4>"

mapNaija <- leaflet() %>% 
  setView(-96,37.8,4) %>% 
  addTiles() %>% 
  addPolygons(data = nigeria,
    fillColor = ~pal(nigeria_pop$Population),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions(
      style = list("font-weight" = "normal",padding = "3x 8x"),
      textsize = "15px",
      interactive = TRUE,
      direction = "auto"
    )
    
  ) %>% 
  addLegend(pal = pal,values = ~nigeria_pop$Population,position = "bottomright") %>% 
  addControl(html = title,position = "topright") %>% 
  addControl(html = source,position = "bottomleft")


mapNaija <- leaflet(nigeria) %>% 
  setView(-96,37.8,4) %>% 
  addTiles() %>% 
  addPolygons()
mapNaija

## Canada Map

canada <- geojson_read("canada.json",what = "sp")
province <- readxl::read_excel("Province.xlsx")
canadaLT <- readxl::read_excel("canadaLT.xlsx")

canada$Province 
canada$Abbreviation

class(province$Population)

class(canada$Province)

names(canada)

canadaT <- spTransform(canada,CRS("+proj=longlat +datsum=WGS84"))

class(canadaT)

canPro <- leaflet(canada) %>% 
  setView(-96,37.8,4) %>% 
  addTiles() %>% 
  addPolygons()
canPro

bins <- c(0,35000,60000,150000,500000,4000000,8000000,10000000,Inf)
pal <- colorBin("YlGnBu",domain = province$Population ,bins = bins)

labels <- paste(
  "Province:",canada$Province, "<br/>",
  "Population:",province$Population, "<br/>"
) %>% lapply(htmltools::HTML)

title <- "<h3> 2015 Population of Canada Distributed by Province</h3>"
source <- "<h4> Source: Canada Census Portal </h4>"

canPro <- leaflet(data = canada) %>% 
  setView(-96,37.8,4) %>% 
  addTiles() %>% 
  addPolygons(
              lng = canadaLT$Longitude,
              lat = canadaLT$Latitude,
              fillColor = ~pal(province$Population),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "3",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = labels,
              labelOptions(
                style = list("font-weight" = "normal",padding = "3x 8x"),
                textsize = "15px",
                direction = "auto"
              )
              
  ) %>% 
  addLegend(pal = pal,values = ~province$Population,title = "Population",position = "bottomright") %>% 
  addControl(html = title,position = "topright") %>% 
  addControl(html = source,position = "bottomleft")

canPro

NGA <- shapefile("NGA.shp")


## European map

europe <- geojson_read("europe.geojson",what = "sp")

class(europe)
names(europe)

europe$income_grp
europe$wikipedia
europe$woe_id
europe$pop_est
europe$continent
europe$pop_year
europe$mapcolor8
europe$name_sort


min(europe$pop_est)
max(europe$pop_est)
range(europe$pop_est)

## Create a basic map

euMap <- leaflet(europe) %>%
  setView(-96,37.8,4) %>% 
  addTiles() %>% 
  addPolygons()
euMap

## Add some color

bins <- c(0,400000,1000000,2500000,5000000,10000000,50000000,80000000,Inf)
pal <- colorBin("Dark2",domain = europe$pop_est ,bins = bins)

title <- "<h3> The Population and Income of European Countries </h3>"
source <- "<h5> Source: European Geo Data </h5>"

labels <- paste(
  "<strong>Country</strong>:",trendsdf$country, "<br/>",
  "Trending:",trendsdf$trend1, "<br/>",
  "Trending:",trendsdf$trend2, "<br/>",
  "Trending:",trendsdf$trend3, "<br/>",
  "Trending:",trendsdf$trend4, "<br/>",
  "Trending:",trendsdf$trend5, "<br/>"
  ) %>% lapply(htmltools::HTML)

euMap <- leaflet(europe) %>%
  setView(14.415556, 50.094722, 4) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(europe$pop_est),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>% addLegend(pal = pal,values = ~europe$pop_est,opacity = 0.7,
                  title = "Population",position = "bottomright") %>% 
  addControl(html = title,position = "topright") %>% 
  addControl(html = source,position = "bottomleft")
  
euMap


## Twitter trends by geolocation

woeids <- data.frame(
  country = europe$name_sort,
  woeid = europe$woe_id
)

woeids

UKtwTrend <- get_trends(woeid = 23424975)
View(UKtwTrend)
write_as_csv(UKtwTrend,file_name = "UKTrends.csv")

GMtwTrend <- get_trends(woeid = 23424829)
View(GMtwTrend)
write_as_csv(GMtwTrend,file_name = "GMTrends.csv")

FRtwTrend <- get_trends(woeid = 23424819)
View(FRtwTrend)
write_as_csv(FRtwTrend,file_name = "FRTrends.csv")

SPtwTrend <- get_trends(woeid = 23424950)
View(SPtwTrend)
write_as_csv(SPtwTrend,file_name = "SPTrends.csv")

PTtwTrend <- get_trends(woeid = 23424925)
View(PTtwTrend)
write_as_csv(PTtwTrend,file_name = "PTTrends.csv")

CYtwTrend <- get_trends(woeid = 26812346)
View(CYtwTrend)

NTtwTrend <- get_trends(woeid = 23424909)
View(NTtwTrend)
write_as_csv(NTtwTrend,file_name = "NTTrends.csv")

HGtwTrend <- get_trends(woeid = 23424844)
View(HGtwTrend)

BGtwTrend <- get_trends(woeid = 23424757)
View(BGtwTrend)
write_as_csv(BGtwTrend,file_name = "BGTrends.csv")

FNtwTrend <- get_trends(woeid = 2403441)
View(FNtwTrend)

IRtwTrend <- get_trends(woeid = 23424803)
View(IRtwTrend)
write_as_csv(IRtwTrend,file_name = "IRTrends.csv")

GRtwTrend <- get_trends(woeid = 23424833)
View(GRtwTrend)
write_as_csv(GRtwTrend,file_name = "GRTrends.csv")

RMtwTrend <- get_trends(woeid = 23424933)
View(RMtwTrend)

EStwTrend <- get_trends(woeid = 23424805)
View(EStwTrend)

ITtwTrend <- get_trends(woeid = 23424853)
View(ITtwTrend)
write_as_csv(ITtwTrend,file_name = "ITTrends.csv")

LTtwTrend <- get_trends(woeid = 23424875)
View(LTtwTrend)

PLtwTrend <- get_trends(woeid = 23424923)
View(PLtwTrend)
write_as_csv(PLtwTrend,file_name = "PLTrends.csv")

DNtwTrends <- get_trends(woeid = 23424796)
View(DNtwTrends)
write_as_csv(DNtwTrends,file_name = "DNTrends.csv")

CRtwTrend <- get_trends(woeid = 23424843)
View(CRtwTrend)

SLtwTrend <- get_trends(woeid = 23424945)
View(SLtwTrend)

AUtwTrend <- get_trends(woeid = 23424750)
View(AUtwTrend)
write_as_csv(AUtwTrend,file_name = "AUTrends.csv")

LXtwTrend <- get_trends(woeid = 23424881)
View(LXtwTrend)

SWtwTrend <- get_trends(woeid = 23424954)
View(SWtwTrend)
write_as_csv(SWtwTrend,file_name = "SWTrends.csv")

CZtwTrend <- get_trends(woeid = 23424810)
View(CZtwTrend)

MTtwTrend <- get_trends(woeid = 23424897)
View(MTtwTrend)

SKtwTrend <- get_trends(woeid = 23424877)
View(SKtwTrend)

LVtwTrend <- get_trends(woeid = 23424874)
View(LVtwTrend)
write_as_csv(LVtwTrend,file_name = "LVTrends.csv")

BLtwTrend <- get_trends(woeid = 23424771)
View(BLtwTrend)

## Read trend data

UKrdTrend <- read_twitter_csv("UKTrends.csv")
GMrdTrend <- read_twitter_csv("GMTrends.csv")
FRrdTrend <- read_twitter_csv("FRTrends.csv")
SPrdTrend <- read_twitter_csv("SPTrends.csv")
PTrdTrend <- read_twitter_csv("PTTrends.csv")
NTrdTrend <- read_twitter_csv("NTTrends.csv")
BGrdTrend <- read_twitter_csv("BGTrends.csv")
IRrdTrend <- read_twitter_csv("IRTrends.csv")
ITrdTrend <- read_twitter_csv("ITTrends.csv")
GRrdTrend <- read_twitter_csv("GRTrends.csv")
DNrdTrend <- read_twitter_csv("DNTrends.csv")
AUrdTrend <- read_twitter_csv("AUTrends.csv")
SWrdTrend <- read_twitter_csv("SWTrends.csv")
LVrdTrend <- read_twitter_csv("LVTrends.csv")
PLrdTrend <- read_twitter_csv("PLTrends.csv")


View(GRrdTrend)


UKrdTrend[["trend"]][4]

trendsdf <- data.frame(
  country = europe$name_sort,
  trend1 = c("0",FRrdTrend[["trend"]][1],"0",BGrdTrend[["trend"]][1],IRrdTrend[["trend"]][1],"0",LVrdTrend[["trend"]][1],PLrdTrend[["trend"]][1],DNrdTrend[["trend"]][1],"0",ITrdTrend[["trend"]][1],"0",UKrdTrend[["trend"]][1],
             "0","0",GRrdTrend[["trend"]][1],"0",SPrdTrend[["trend"]][1],AUrdTrend[["trend"]][1],"0",NTrdTrend[["trend"]][1],"0",GMrdTrend[["trend"]][1],SWrdTrend[["trend"]][1],PTrdTrend[["trend"]][1],"0","0","0"),
  trend2 = c("0",FRrdTrend[["trend"]][2],"0",BGrdTrend[["trend"]][2],IRrdTrend[["trend"]][2],"0",LVrdTrend[["trend"]][2],PLrdTrend[["trend"]][2],DNrdTrend[["trend"]][2],"0",ITrdTrend[["trend"]][2],"0",UKrdTrend[["trend"]][2],
             "0","0",GRrdTrend[["trend"]][2],"0",SPrdTrend[["trend"]][2],AUrdTrend[["trend"]][2],"0",NTrdTrend[["trend"]][2],"0",GMrdTrend[["trend"]][2],SWrdTrend[["trend"]][2],PTrdTrend[["trend"]][2],"0","0","0"),
  trend3 = c("0",FRrdTrend[["trend"]][3],"0",BGrdTrend[["trend"]][3],IRrdTrend[["trend"]][3],"0",LVrdTrend[["trend"]][3],PLrdTrend[["trend"]][3],DNrdTrend[["trend"]][3],"0",ITrdTrend[["trend"]][3],"0",UKrdTrend[["trend"]][3],
             "0","0",GRrdTrend[["trend"]][3],"0",SPrdTrend[["trend"]][3],AUrdTrend[["trend"]][3],"0",NTrdTrend[["trend"]][3],"0",GMrdTrend[["trend"]][3],SWrdTrend[["trend"]][3],PTrdTrend[["trend"]][3],"0","0","0"),
  trend4 = c("0",FRrdTrend[["trend"]][4],"0",BGrdTrend[["trend"]][4],IRrdTrend[["trend"]][4],"0",LVrdTrend[["trend"]][4],PLrdTrend[["trend"]][4],DNrdTrend[["trend"]][4],"0",ITrdTrend[["trend"]][4],"0",UKrdTrend[["trend"]][4],
             "0","0",GRrdTrend[["trend"]][4],"0",SPrdTrend[["trend"]][4],AUrdTrend[["trend"]][4],"0",NTrdTrend[["trend"]][4],"0",GMrdTrend[["trend"]][4],SWrdTrend[["trend"]][4],PTrdTrend[["trend"]][4],"0","0","0"),
  trend5 = c("0",FRrdTrend[["trend"]][5],"0",BGrdTrend[["trend"]][5],IRrdTrend[["trend"]][5],"0",LVrdTrend[["trend"]][5],PLrdTrend[["trend"]][5],DNrdTrend[["trend"]][5],"0",ITrdTrend[["trend"]][5],"0",UKrdTrend[["trend"]][5],
             "0","0",GRrdTrend[["trend"]][5],"0",SPrdTrend[["trend"]][5],AUrdTrend[["trend"]][5],"0",NTrdTrend[["trend"]][5],"0",GMrdTrend[["trend"]][5],SWrdTrend[["trend"]][5],PTrdTrend[["trend"]][5],"0","0","0")

)

View(trendsdf)


mywoeid <- c(23424829,23424819,23424909,23424950,23424925)
dat_woeid <- for(mywoeid in 1:5){
  
  datwoeid <- get_trends(mywoeid)
  
  woeidf <- as.data.frame(datwoeid)
  
  return(woeidf)
  
}

View(woeidf)


woeidJS <- fromJSON("codebeautify.json")
woeidJS
View(woeidJS)

get_tr
