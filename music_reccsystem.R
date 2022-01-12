#musicrecc system
rm(list=ls())
#install.packages("tidyverse") #has ggplot2.dplyr,tidyr etc.
library(tidyverse)
#1st dataset
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
#2nd dataset
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')
polls
rankings
#title
polls %>% #(used for nesting)
  count(title,sort=TRUE)
#histogram per decade
polls %>%
  count(year)%>%
  mutate(decade=floor(year/10)*10) %>%
  mutate(decade=as.factor(decade))%>%
  ggplot(aes(x=year,y=n,fill=decade))+geom_col()
#bar graph for decade
polls %>%
  count(year)%>%
  mutate(decade=floor(year/10)*10) %>%
  mutate(decade=as.factor(decade))%>%
  ggplot(aes(x=decade,y=n,fill=decade))+geom_col()
#graph- n(x axis) vs density(y axis)
polls %>%
  count(artist,sort=TRUE) %>%
  ggplot(aes(x=n))+geom_density()
#graphs abt rankings
rankings%>%
  select(artist,n,n1,n2,n3,n4,n5) %>%
  group_by(artist)%>%
  summarise_all(sum)%>%
  filter(!str_detect(artist,"ft."))%>%
  ggplot(aes(x=n1,y=n5))+geom_jitter()
#Ranking of artist
rankings %>%
  select(artist,n,n1,n2,n3,n4,n5) %>%
  group_by(artist)%>%
  summarise_all(sum)%>%
  filter(!str_detect(artist,"ft."))%>%
  arrange(desc(n1)) %>%
  slice(1:10)
#checking abt country bias
polls %>%
  count(title,critic_country,name = "song_nom") %>%
  add_count(title,name="number_of_countries") %>%
  filter(number_of_countries==1 & critic_country !="US") %>%
  nrow()/nrow(polls)
#ranking songs Alphabetically
polls%>%
  count(title,critic_country,name = "song_nom_country") %>%
  add_count(title,name="number_of_countries") %>%
  filter(number_of_countries !=1)%>%
  select(-number_of_countries) %>%
  pivot_wider(names_from = "critic_country",values_from = "song_nom_country")
#recommender
#install.packages("recommenderlab")
library(recommenderlab)
#unique(polls["critic_country"])
#sum(is.na(polls$rank))
#polls %>% summarise_all(~ sum(is.na(.)))
#create matrix
rap_matrix <- polls %>%
  select(critic_name,title)%>%
  mutate(n=1)%>%
  arrange(title)%>%
  pivot_wider(names_from = "title",values_from = "n",values_fill = list(n=0))%>%
  select(-critic_name) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")
#using matrix-split into training set
training_schema <- evaluationScheme(rap_matrix,method= "split",train=.8,given=-1)
print(training_schema)
#create_models
UBCF_Model <-evaluate(training_schema,method ="UBCF", type="topNList",n=5)
#ibcf
IBCF_Model <-evaluate(training_schema,method ="IBCF", type="topNList",n=5)
UBCF_Model %>% avg()
#tibbles-better than data frames
IBCF_Model %>% avg() %>% as_tibble()
tune_engines <- function(schema,parameters){
  #create_new models
  UBCF_Model <-evaluate(schema,method ="UBCF", type="topNList",n=5,param=list(nn=parameters))
  IBCF_Model <-evaluate(schema,method ="IBCF", type="topNList",n=5,param=list(k=parameters))
  UBCF_Model %>%
    avg() %>%
    as_tibble() %>%
    mutate(model="UBCF") %>%
    rbind(IBCF_Model %>%
    avg() %>%
    as_tibble() %>%
    mutate(model="IBCF")) %>%
    return()
}
tune_grid <- tibble(parameters =c(3,5,10,15,20,25))
#mutate() adds new variables and preserves; transmute() adds new variables and drops existing

history <- tune_grid %>%
  mutate(results = map(parameters, ~tune_engines(training_schema,.x))) %>%
  #unnest used to handle columns that contain atomic vectors, lists or df
  unnest()
#use 5 nearest neighbors
history %>%
  ggplot(aes(x=parameters,y=TPR,fill = model, label = parameters)) + geom_col(position= "dodge") + geom_text(aes(x=parameters,y=TPR))
#Recommender for the last time
UBCF_Final_Model <- Recommender(getData(training_schema,"train"),"UBCF",param=list(nn=5))
# view output
UBCF_Final_Model
#predict
predictions <- predict(UBCF_Final_Model,getData(training_schema,"known"),type="topNList")
calcPredictionAccuracy(predictions,getData(training_schema,"unknown"),given=-1)
#rec engine
rec_engine <- Recommender(rap_matrix,"UBCF",param=list(nn=5))
rec_engine
polls %>% filter(str_detect(artist,"2Pac")) %>% distinct(title) %>% arrange(title)

andrew_songs <- polls %>%
  select(title) %>%
  distinct() %>%
  arrange(title) %>%
  filter(title %in% c("All of the Lights","Alright","Bitch Don't Kill My Vibe","m.A.A.d. city","Changes")) %>%
  rbind(polls %>% select(title) %>% distinct()) %>%
  count(title) %>%
  mutate(n= n-1) %>%
  pivot_wider(names_from="title", values_from= "n",values_fill = list(n=0)) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")
rec_engine
#recc songs based on data
predict(rec_engine,andrew_songs) %>% as("list") %>% as.data.frame()
  
