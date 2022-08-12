# libs
library(readr)
library(tidyverse)
library(lubridate)
library(psych)
library(usmap)
library(kableExtra)
library(stargazer)

# Task 1
df = read_csv("selected_bargaining_data.csv")

df$listing_date  = (df$listing_date %>% str_split(pattern = ' ', simplify = T))[,1]%>% mdy()
df$off_market_date = df$off_market_date %>% mdy()

event_df = df %>% 
  group_by(event_id) %>%
  summarise(
    
    age = (year(off_market_date) - max(year_built)) %>% mean(na.rm = T),
    buyer_nr = unique(buyer_id) %>% length(),
    revision = unique(round_id) %>% length(),
    dur = max(off_market_date - listing_date) %>% as.numeric(),
    price = de_sales_price %>% max()
    
  ) %>% 
  filter( (price %>% is.finite() )& (age >= 0)& (age < 200))

stats_df = describe(event_df[,-1])[, c(3, 4, 8, 5, 9)] %>% as.data.frame() %>% round(digits = 2)

rownames(stats_df) = c("Property Age", "Buyer Represented by the Platform", "Revisions", "Duration", "Sales Price")
colnames(stats_df) = c("Mean", "Std.Dev.","Min", "Median", "Max")

stats_df %>% kable()


# Task 2
geo_df = df %>% 
  select(event_id, censustract) %>% 
  unique()

geo_df$fips = geo_df$censustract %>% 
  lapply(., function(x){
    
    x = as.character(x)
    if(nchar(x) == 10){
      return(substr(x, 1, 1))
      
    }else{
      return(substr(x, 1, 2))
    }
    
  }) %>% 
  as.integer()

event_state = geo_df %>%
  group_by(fips) %>% 
  summarise(nums = n())

plot_usmap(data = event_state, regions ="states",values = "nums", labels = T,label_color = "white") + 
  scale_fill_continuous(name = "Number of Bargaining Events") + 
  theme(legend.position = "right") 


# Task 3
Xs = df %>% select(event_id, num_bathrooms, num_bedrooms, approx_sq_ft, walk_score, bike_score, lot_sq_ft, total_num_buyers_event, de_original_list_price) %>% unique() 

model_df =left_join(event_df, Xs)[,-1] 

model_df$num_bathrooms[is.na(model_df$num_bathrooms)]<-mean(model_df$num_bathrooms,na.rm=TRUE)
model_df$num_bedrooms[is.na(model_df$num_bedrooms)]<-mean(model_df$num_bedrooms,na.rm=TRUE)
model_df$approx_sq_ft[is.na(model_df$approx_sq_ft)]<-mean(model_df$approx_sq_ft,na.rm=TRUE)
model_df$walk_score[is.na(model_df$walk_score)]<-mean(model_df$walk_score,na.rm=TRUE)
model_df$bike_score[is.na(model_df$bike_score)]<-mean(model_df$bike_score,na.rm=TRUE)
model_df$lot_sq_ft[is.na(model_df$lot_sq_ft)]<-mean(model_df$lot_sq_ft,na.rm=TRUE)
model_df$total_num_buyers_event[is.na(model_df$total_num_buyers_event)]<-mean(model_df$total_num_buyers_event,na.rm=TRUE)

par(mfrow = c(1,2))
model_df$price %>% density()  %>% plot(xlab = "price",main = "Original Price Distribution")
model_df$price %>% log() %>% density()  %>% plot(xlab = "log price", main = "Log Transformation")

Full_model = lm(log(price) ~., data = model_df)
none = lm(log(price)~1, data=model_df)
MSE = (summary(Full_model)$sigma)^2 
Stepwise_model = step(none, scope=list(upper=Full_model), scale=MSE, trace = FALSE)
summary(Stepwise_model)

stargazer(Full_model,Stepwise_model, type = "latex", title = "Models")
