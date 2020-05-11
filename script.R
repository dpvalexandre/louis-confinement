library(rjson)
library(tidyverse)
library(lubridate)


raw <- fromJSON(file = "messages.json")


raw<- raw[["conversations"]][[1]][["MessageList"]]


raw<- raw%>%map(compact)%>%map_dfr(as_tibble)%>%filter(messagetype=="Event/Call")



cleaned <- raw %>% select(id, originalarrivaltime, messagetype, content)%>%mutate( date = str_extract(originalarrivaltime, "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")%>%ymd(),
                                                                                   heure = str_extract(originalarrivaltime, "[:digit:]{2}:[:digit:]{2}")%>%hm()%>% + hours(2),
                                                                                   debut_fin = if_else(str_detect(content, "started"), "debut", "fin"))


cleaned<- cleaned %>% group_by(date) %>% summarize(duree = max(period_to_seconds(heure)) - min(period_to_seconds(heure)),
                                         duree = duree / 60)


cleaned %>%ggplot(aes(x = date, y = duree))+ 
  geom_col()+ 
  labs(x="", y = "duree en minutes", title= "Date et durée des skypes avec Louis durant le confinement")+  
  theme_bw()+ 
  scale_x_date(date_breaks = "4 days", date_labels = "%d%-%m", limits = c(ymd("2020-3-19"), ymd("2020-5-10")))+
  scale_y_continuous(breaks = c(0,15,30,45,60,75,90))
  
                  