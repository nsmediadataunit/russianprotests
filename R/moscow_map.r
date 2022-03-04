#moscow map
library(leaflet)
library(htmlwidgets)

moscow_dw <- read_csv("data/moscow_dw.csv") %>%
  rowwise() %>%
  mutate(Title = paste0(stringi::stri_wrap(Title,40),collapse="<br />"),
         label = htmltools::HTML(paste0("<strong>",Title,"</strong><br /><big>",cumulative_detainees,"</big> detained in total.")))

moscow_map <- leaflet(moscow_dw) %>% 
  setView(lng = 37.6173, lat = 55.7558, zoom = 10) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(
    lng=~Lon,
    lat=~Lat,
    radius = ~cumulative_detainees/5,
    color = "#d82d1d",
    stroke = FALSE, fillOpacity = 0.8,
    label = ~label,
    labelOptions(style = list("border" = "none"))
  )
  
saveWidget(moscow_map,"moscow_map.html")
#add pym.js
x <- read_file("moscow_map.html") %>%
  str_replace("\\<\\/body\\>",'<script type=\"text/javascript\" src=\"https://pym.nprapps.org/pym.v1.min.js\"></script>\r\n<script>var pymChild = new pym.Child({polling: 500});</script>\r\n</body>')
  write_file(x,"moscow_map.html")
