library(rvest)
library(magrittr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
#DETAINEES (From OVD-Info https://ovdinfo.org/) ----

#get latest page

latest_url <- read_html("https://ovdinfo.org/") %>%
  html_element(".main-values > .value-first") %>%
  html_element("a.active") %>%
  html_attr("href")

#get previous pages
urls <- read_html(latest_url) %>%
  html_element("div.field-name-body") %>%
  html_node("p") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  url_absolute("https://ovd.news") %>%
  c(latest_url)

#function to scrape pages and get data

scrape_detainees <- function(url){
  
  #get page
  page <- read_html(url)
  
  #get date
  date <- page %>%
    html_nodes("span.date") %>%
    html_text()
  
  cat(date,"\n")
  
  #get content section
  content <- page %>%
    html_element("div.field-name-body")
  
  #get city
  city <- content %>%
    html_elements("h2") %>% html_text()
  
  #get city cumulative totals
  city_total_xpath <- sprintf("//p[preceding-sibling::h2[1][contains(., '%s')]]/strong", city)
  city_total <- map(city_total_xpath, function(city_total_xpath){
    html_nodes(x = content, xpath = city_total_xpath) %>% 
      html_text()})
  
  #get district following each city
  district_xpath <- sprintf("//h3[preceding-sibling::h2[1][contains(., '%s')]]", city)
  
  district <- map(district_xpath, function(district_xpath){
    html_nodes(x = content, xpath = district_xpath) %>% 
      html_text()})
  
  city_district <- tibble(city,district,city_total) %>% mutate(city_total=str_extract(as.character(city_total),"[0-9]+")) %>% unnest(c("district","city_total"))
  
  #get first paragraph following each district (second is overnight which we might not bother with)
  para_xpath <- sprintf("//p[preceding-sibling::h3[1][contains(., '%s')]/preceding-sibling::h2[1][contains(., '%s')]][1]",city_district$district,city_district$city)
  
  paras <- map(para_xpath, function(para_xpath){
    html_nodes(x = content, xpath = para_xpath) %>% 
      html_text()})
  
  #join up and clean
  data <- tibble(city_district,paras) %>% 
    unnest(paras) %>% 
    mutate(paras = str_trim(paras),
           district = str_trim(district),
           district_detainees = ifelse(!is.na(as.numeric(word(paras,1,sep="\\D"))),as.numeric(word(paras,1,sep="\\D")),as.numeric(word(district,-1,sep="\\D"))),
           date=date) %>%
    select(city,district,date,city_detainees=city_total,district_detainees)
  
}

#run function on all our pages
data <- map_dfr(urls,scrape_detainees)

#clean data 
data <- data %>%
  mutate(across(c("district","city"), str_replace_all,"\\n|\\s"," "),
         across(c("district","city"), trimws))

write_csv(data,paste0("data/detainees/",Sys.Date(),"_daily_detainees.csv"))

#get district locations
old_district_locations <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRqShs3EMyAriB_xQhUQfY0LL49JMNw0eK97eyjOcZk4N0vr0TCVQZYHtG0VRkFAxepHBb164yPywPp/pub?gid=0&single=true&output=csv")

locations <-  data %>% select(city,district) %>%
  filter(!str_detect(district,"\\u043D\\u0435\\u0438\\u0437\\u0432\\u0435\\u0441\\u0442\\u043D\\u043E")) %>%
  unique() %>%
  mutate(contains = str_match(district,"\\u0020\\u0433\\u002E\\u0020"),
         location=ifelse(is.na(contains),paste0(district,", ",city),district)) %>% unnest(c("location")) %>%
  left_join(select(old_locations,-city_en))%>%
  select(city,district,district_en,location,lat,lon)

missing_district_locations <- locations %>% filter(is.na(lon))
  
write_excel_csv(locations,"data/district_locations.csv")

#get city locations
old_city_locations <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRqShs3EMyAriB_xQhUQfY0LL49JMNw0eK97eyjOcZk4N0vr0TCVQZYHtG0VRkFAxepHBb164yPywPp/pub?gid=864898366&single=true&output=csv")

city_locations <-  data %>% select(city) %>% unique() %>%
  left_join(old_city_locations)

missing_city_locations <- city_locations %>% filter(is.na(city_lon))

write_excel_csv(city_locations,"data/city_locations.csv")

#join data
data_geo <- data %>% left_join(locations) %>% left_join(city_locations)

write_csv(data_geo,paste0("data/detainees/latest_daily_detainees.csv"))

#summary tables
cumulative_district <- data %>% group_by(city,district,city_en,district_en) %>%
  summarise(cumulative_detainees = sum(district_detainees,na.rm=T))
write_csv(data,paste0("data/detainees/latest_cumulative_detainees_district.csv"))
cumulative_city <- data %>% group_by(city,city_en)%>%
  summarise(cumulative_detainees = sum(district_detainees,na.rm=T))
city2 <- data %>% select(city,city_en,city_detainees) %>% unique() %>% group_by(city,city_en)%>%
  summarise(cumulative_detainees = sum(as.numeric(city_detainees),na.rm=T))
write_csv(cumulative_city,paste0("data/detainees/latest_cumulative_detainees_city.csv"))
