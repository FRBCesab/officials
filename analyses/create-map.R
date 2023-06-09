fra <- readRDS(here::here("data", "raw-data", 
                          "shp-FRA_DOM-composite_lowres.rds"))
fra <- sf::st_as_sf(fra)
fra <- fra[ , -1]

ans <- readxl::read_xlsx(here::here("data", "raw-data", 
                                    "data-forms-20230609.xlsx"))

ans <- as.data.frame(ans)

col_departs <- grep("département|départment", colnames(ans))
col_region  <- grep("région", colnames(ans))

ans <- ans[!is.na(ans[ , col_region]), ]


dom <- c("La Réunion", "Mayotte", "Guadeloupe", "Guyane", "Martinique")

dom <- ans[which(ans[ , col_region] %in% dom), ]
dom <- tapply(dom[ , col_region], dom[ , col_region], length)

departs <- unlist(ans[ , col_departs])
departs <- departs[!is.na(departs)]

departs <- unlist(strsplit(departs, ";"))
names(departs) <- NULL

departs <- gsub("69- Rhône", "69 - Rhône", departs)
departs <- gsub("40 -Landes", "40 - Landes", departs)
departs <- gsub("Côtes d'Armor", "Côtes-d'Armor", departs)
departs <- gsub("Ille et Vilaine", "Ille-et-Vilaine", departs)
departs <- gsub("Pyrénées-Atlantique", "Pyrénées-Atlantiques", departs)
departs <- gsub("Hautes-Pyrénnées", "Hautes-Pyrénées", departs)
pos <- grep("Alsace", departs)
departs[pos] <- "Bas-Rhin"
departs <- c(departs, rep("Haut-Rhin", length(pos)))
departs <- trimws(departs)

departs <- strsplit(departs, " - ")
departs <- unlist(lapply(departs, function(x) x[2]))

departs <- table(departs)



departs <- c(departs, dom)
departs <- data.frame("departement" = names(departs), 
                      "n"           = departs, 
                      row.names     = NULL)

length(which(!(departs$"departement" %in% fra$"departement")))


fra <- merge(fra, departs, by = "departement", all = TRUE)

# fra[which(is.na(fra$"n")), "n"] <- 0

bins <- seq(1, max(fra$"n", na.rm = TRUE), length.out  = 9)
pal  <- leaflet::colorBin("YlOrRd", domain = fra$"n", bins = bins, na.color = "#cccccc")

labels <- sprintf(
  "<strong>%s</strong><br/>(n=%g)",
  fra$"departement", ifelse(is.na(fra$"n"), 0, fra$"n")) |>
  lapply(htmltools::HTML)


map <- leaflet::leaflet() |> 
  
  leaflet::setView(lng = 2.25, lat = 46.50, zoom = 5.85)  |> 

  leafem::garnishMap(
    leaflet::addPolygons, data = fra, 
    group = 'Data', weight = 1.0, smoothFactor = 0.5, 
    opacity = 1, fillOpacity = 1, color = "#212121", 
    fillColor = ~pal(fra$"n"),
    highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                 weight = 2, 
                                                 bringToFront = TRUE),
    label = labels,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "text-align" = "center"),
      textsize = "15px",
      direction = "auto")) |> 
  
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = 'fa-globe', title = 'Zoom initial',
      onClick = leaflet::JS('function(btn){ location.reload(); }')))



htmlwidgets::saveWidget(widget = map, file = here::here("content", 
                                                        "core.html"), 
                        selfcontained = FALSE)


## Change background color of leaflet map ----

html <- readLines(here::here("content", "core.html"))

html[grep('id=\"htmlwidget-', html)] <- 
  gsub('width:100%;', 
       'width:100%;background-color:#212121;', 
       html[grep('id=\"htmlwidget-', html)])

cat(paste0(html, collapse = "\n"), file = here::here("content", "core.html"))
