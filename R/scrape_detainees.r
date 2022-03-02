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

write_csv(data,paste0("data/detainees/",Sys.Date(),"_daily_detainees.csv"))

#get locations

locations <-  data %>% select(city,district) %>%
  filter(!str_detect(district,"неизвестно")) %>%
  unique() %>%
  mutate(across(c("district","city"), str_replace_all,"\\n|\\s"," "),
         across(c("district","city"), trimws),
         contains = str_match(district," г. "),
         location=ifelse(is.na(contains),paste0(district,", ",city),district)) %>% unnest(c("location")) %>%
  left_join(read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRqShs3EMyAriB_xQhUQfY0LL49JMNw0eK97eyjOcZk4N0vr0TCVQZYHtG0VRkFAxepHBb164yPywPp/pub?gid=0&single=true&output=csv"))%>%
  select(city,district,location,lat,lon)

write_excel_csv(locations,"data/locations.csv")
