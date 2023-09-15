devtools::load_all()


## Import composite shp ----

fra <- readRDS(here::here("data", "raw-data", 
                          "shp-FRA_DOM-composite_lowres.rds"))
fra <- sf::st_as_sf(fra)
fra <- sf::st_transform(fra, crs = "epsg:4326")
fra <- fra[ , -1]


## Import answers to the form ----

ans <- readxl::read_xlsx(here::here("data", "raw-data", 
                                    "data-forms-20230731-clean.xlsx"))

ans <- as.data.frame(ans)


## Subset columns ----

col_s <- c("Prénom", 
           "NOM", 
           "Courriel professionnel", 
           "Établissement employeur propre",
           "Ville",
           "Disciplines et spécialités", 
           "Formation des agents",
           "Formation des formateurs",
           "RGPD", 
           "Scan R",
           "région")

dat <- ans[ , grep(paste0(col_s, collapse = "|"), colnames(ans))]
colnames(dat) <- c("firstname", "lastname", "email", "institution", "city", 
                   "keywords", "speciality_agents", "region", 
                   "speciality_trainers", "rgpd", "scanr")


## Clean names ----

dat$"lastname"  <- trimws(dat$"lastname")
dat$"lastname"  <- gsub("\\s+", " ", dat$"lastname")
dat$"lastname"  <- toupper(dat$"lastname")

dat$"firstname" <- trimws(dat$"firstname")
dat$"firstname"  <- gsub("\\s+", " ", dat$"firstname")
dat$"firstname" <- unlist(lapply(dat$"firstname", to_name_case)) 


## Clean emails ----

dat$"email" <- trimws(dat$"email")
dat$"email" <- gsub("\\s+", " ", dat$"email")
dat$"email" <- tolower(dat$"email")


## Clean disciplines ----

dat$"speciality_agents" <- gsub("Ressources", "Ressources naturelles", 
                                dat$"speciality_agents")

disciplines <- paste(dat$"speciality_agents", dat$"speciality_trainers", sep = ";")
disciplines <- strsplit(disciplines, ";")

disciplines <- lapply(disciplines, function(x) {
  x <- x[!is.na(x)]
  x <- x[x != "NA"]
  x <- x[x != ""]
  x <- trimws(x)
  paste0(sort(unique(x)), collapse = " ; ")
})

disciplines <- unlist(disciplines)

dat$"climate"      <- as.numeric(grepl("Climat", disciplines))
dat$"biodiversity" <- as.numeric(grepl("Biodiversité", disciplines))
dat$"natresources" <- as.numeric(grepl("Ressources", disciplines))

to_del <- which(colnames(dat) %in% c("speciality_agents", "speciality_trainers"))

if (length(to_del) > 0) dat <- dat[ , -to_del]


## Add noid ----

dat <- data.frame("noid" = 1:nrow(dat), dat)


## Add departments ----

col_departs <- grep("département|départment", colnames(ans))
departments <- ans[ , col_departs]

departments <- apply(departments, 1, function(x) paste0(x, collapse = " ; "))
departments <- strsplit(departments, ";")

departments <- lapply(departments, function(x) {
  x <- trimws(x)
  x <- gsub("NA", "", x)
  x <- x[x!= ""]
  
  x <- gsub("69- Rhône", "69 - Rhône", x)
  x <- gsub("40 -Landes", "40 - Landes", x)
  x <- gsub("Côtes d'Armor", "Côtes-d'Armor", x)
  x <- gsub("Ille et Vilaine", "Ille-et-Vilaine", x)
  x <- gsub("Pyrénées-Atlantique", "Pyrénées-Atlantiques", x)
  x <- gsub("Hautes-Pyrénnées", "Hautes-Pyrénées", x)
  x <- gsub("2A : Corse du Sud", "2A - Corse-du-Sud", x)
  x <- gsub("2B : Haute Corse", "2B - Haute-Corse", x)
  x <- gsub("76 - Seine Maritime", "76 - Seine-Maritime", x)
  x <- gsub("Eure-et-Loire", "Eure-et-Loir", x)
  
  if (length(grep("Alsace", x)) > 0) {
    x <- x[-grep("Alsace", x)]
    x <- c("67 - Bas-Rhin", "68 - Haut-Rhin")
  }
  
  x <- strsplit(x, " - ")
  x <- lapply(x, function(x) x[2])
  
  unlist(x)
})


list_of_dep <- data.frame("department" = c(sort(unique(unlist(departments))),
                          "La Réunion", "Mayotte", "Guadeloupe", 
                            "Guyane", "Martinique"))

list_of_dep$"parser" <- list_of_dep$"department"
list_of_dep$"parser" <- tolower(list_of_dep$"parser")
list_of_dep$"parser" <- gsub("\\s|-|\\'", "_", list_of_dep$"parser")
list_of_dep$"parser" <- stringi::stri_trans_general(list_of_dep$"parser", "Latin-ASCII") 



deps <- data.frame("noid" = 1:length(departments))

for (i in 1:nrow(list_of_dep)) {
  
  dep <- unlist(lapply(departments, function(x) 
    sum(x == list_of_dep$"department"[i])))
  pos <- which(is.na(dep))
  if (length(pos) > 0) dep[pos] <- 0
  
  deps <- cbind(deps, dep)
  colnames(deps)[ncol(deps)] <- list_of_dep$"parser"[i]
}

dom <- c("La Réunion", "Mayotte", "Guadeloupe", "Guyane", "Martinique")

for (i in 1:length(dom)) {
  tom <- dat[which(dat[ , "region"] == dom[i]), "noid"]
  co_l <- which(list_of_dep$department == dom[i])
  deps[tom, list_of_dep[co_l, "parser"]] <- 1
}
  
dat <- merge(dat, deps, by = "noid", all = TRUE)


## Merge duplicates ----

pos_dup_val <- which(duplicated(paste(dat$"lastname", dat$"firstname")))

dup_val <- dat[pos_dup_val, ]

dup_key <- apply(dup_val[ , c("lastname", "firstname")], 1, 
                 function(x) paste(x[1], x[2], sep = ", "))
dup_key <- sort(unique(dup_key))

dup <- strsplit(dup_key, ", ")
dup <- do.call(rbind.data.frame, dup)

colnames(dup) <- c("lastname", "firstname")

for (i in 1:nrow(dup)) {
  
  pos <- which(dat$"lastname" == dup[i, "lastname"] & 
                 dat$"firstname" == dup[i, "firstname"])
  
  sub_dat <- dat[pos, ]
  
  sub_dat <- apply(sub_dat[ , 11: ncol(sub_dat)], 2, sum)
  sub_dat <- ifelse(sub_dat > 0, 1, 0)
  
  dat[pos[1], names(sub_dat)] <- sub_dat
}


dat <- dat[-pos_dup_val, ]


## Filter ----

dat <- dat[which(dat$"rgpd" == "Oui"), ]


## Synthesis by disciplines ----

dat_climate <- dat[dat$"climate" == 1, ]
dat_climate <- dat_climate[order(paste(dat_climate$"lastname", 
                                      dat_climate$"firstname")), ]

dat_biodiv  <- dat[dat$"biodiversity" == 1, ]
dat_biodiv  <- dat_biodiv[order(paste(dat_biodiv$"lastname", 
                                      dat_biodiv$"firstname")), ]

dat_resour  <- dat[dat$"natresources" == 1, ]
dat_resour  <- dat_resour[order(paste(dat_resour$"lastname", 
                                      dat_resour$"firstname")), ]


## Export data for tables ----

saveRDS(dat_climate, file = here::here("data", "derived-data", "data_climate.rds"))
saveRDS(dat_biodiv, file = here::here("data", "derived-data", "data_biodiversity.rds"))
saveRDS(dat_resour, file = here::here("data", "derived-data", "data_resources.rds"))


synt <- data.frame(
  "climate"      = apply(dat_climate[ , 14: ncol(dat_climate)], 2, sum),
  "biodiversity" = apply(dat_biodiv[ , 14: ncol(dat_biodiv)], 2, sum),
  "resources"    = apply(dat_resour[ , 14: ncol(dat_resour)], 2, sum))

synt <- data.frame("parser" = rownames(synt), synt)
rownames(synt) <- NULL

synt <- merge(list_of_dep, synt, by = "parser", all = TRUE)


## Add data to shapefile -----

fra <- merge(fra, synt, by.x = "departement", by.y = "department", all = TRUE)


## Tooltips ----

list_of_dep <- list_of_dep[order(list_of_dep$"parser"), ]

bars <- list.files(here::here("content", "tables"), pattern = "climate.*html$")

bars_clim <- readRDS(here::here("data", "derived-data", 
                               paste0("popup_tables-climate.rds")))

for (i in 1:length(bars_clim)) {
  bars_clim[[i]] <- gsub("src='.*graphs/tmp_[0-9]{1,}\\.html'", 
                        paste0("src='tables/table-climate-", list_of_dep[i, "parser"], 
                               ".html'"), bars_clim[[i]])
}

names(bars_clim) <- NULL

bars <- list.files(here::here("content", "tables"), pattern = "biodiversity.*html$")

bars_biodiv <- readRDS(here::here("data", "derived-data", 
                                paste0("popup_tables-biodiversity.rds")))

for (i in 1:length(bars_biodiv)) {
  bars_biodiv[[i]] <- gsub("src='.*graphs/tmp_[0-9]{1,}\\.html'", 
                         paste0("src='tables/table-biodiversity-", list_of_dep[i, "parser"], 
                                ".html'"), bars_biodiv[[i]])
}

names(bars_biodiv) <- NULL

bars <- list.files(here::here("content", "tables"), pattern = "resources.*html$")

bars_resources <- readRDS(here::here("data", "derived-data", 
                                  paste0("popup_tables-resources.rds")))

for (i in 1:length(bars_resources)) {
  bars_resources[[i]] <- gsub("src='.*graphs/tmp_[0-9]{1,}\\.html'", 
                           paste0("src='tables/table-resources-", list_of_dep[i, "parser"], 
                                  ".html'"), bars_resources[[i]])
}

names(bars_resources) <- NULL

# bars_pop <- lapply(bars_pop, function(x) {
#   gsub("leaflet-popup-content \\{", 
#        "leaflet-popup-content \\{ \twidth: 755px !important;", x)
# })


## Create color palettes ----

bornes <- min(fra$"climate"):(max(fra$"climate") + 1)
cols <- colorRampPalette(RColorBrewer::brewer.pal("YlOrRd", n = 9))(length(bornes))

fra$"color_climate" <- NA
for (i in 1:length(cols)) {
  pos <- which(fra$"climate" >= bornes[i] & fra$"climate" < bornes[i + 1])
  if (length(pos) > 0) {
    fra[pos, "color_climate"] <- cols[i]
  }
}

bornes <- min(fra$"biodiversity"):(max(fra$"biodiversity") + 1)
cols <- colorRampPalette(RColorBrewer::brewer.pal("Greens", n = 9))(length(bornes))

fra$"color_biodiv" <- NA
for (i in 1:length(cols)) {
  pos <- which(fra$"biodiv" >= bornes[i] & fra$"biodiv" < bornes[i + 1])
  if (length(pos) > 0) {
    fra[pos, "color_biodiv"] <- cols[i]
  }
}

bornes <- min(fra$"resources"):(max(fra$"resources") + 1)
cols <- colorRampPalette(RColorBrewer::brewer.pal("Blues", n = 9))(length(bornes))

fra$"color_resources" <- NA
for (i in 1:length(cols)) {
  pos <- which(fra$"resources" >= bornes[i] & fra$"resources" < bornes[i + 1])
  if (length(pos) > 0) {
    fra[pos, "color_resources"] <- cols[i]
  }
}


## Create labels on hover ----

labels_climate <- sprintf(
  "<strong>%s</strong><br/>(n=%g)",
  fra$"departement", ifelse(is.na(fra$"climate"), 0, fra$"climate")) |>
  lapply(htmltools::HTML)

labels_biodiv <- sprintf(
  "<strong>%s</strong><br/>(n=%g)",
  fra$"departement", ifelse(is.na(fra$"biodiversity"), 0, fra$"biodiversity")) |>
  lapply(htmltools::HTML)

labels_resources <- sprintf(
  "<strong>%s</strong><br/>(n=%g)",
  fra$"departement", ifelse(is.na(fra$"resources"), 0, fra$"resources")) |>
  lapply(htmltools::HTML)


## Map ----

map <- leaflet::leaflet() |> 
  
  leaflet::setView(lng = 2.25, lat = 46.50, zoom = 5.50)  |> 

  leafem::garnishMap(
    leaflet::addPolygons, data = fra, 
    group = 'Climat', weight = 1.0, smoothFactor = 0.5, 
    opacity = 1, fillOpacity = 1, color = "#212121", 
    fillColor = fra$"color_climate",
    highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                 weight = 2, 
                                                 bringToFront = TRUE),
    label = labels_climate,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "text-align" = "center"),
      textsize = "15px",
      direction = "auto"),
    popup = bars_clim) |> 
  
  leafem::garnishMap(
    leaflet::addPolygons, data = fra, 
    group = 'Biodiversité', weight = 1.0, smoothFactor = 0.5, 
    opacity = 1, fillOpacity = 1, color = "#212121", 
    fillColor = fra$"color_biodiv",
    highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                 weight = 2, 
                                                 bringToFront = TRUE),
    label = labels_biodiv,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "text-align" = "center"),
      textsize = "15px",
      direction = "auto"),
    popup = bars_biodiv) |> 
  
  leafem::garnishMap(
    leaflet::addPolygons, data = fra, 
    group = 'Ressources naturelles', weight = 1.0, smoothFactor = 0.5, 
    opacity = 1, fillOpacity = 1, color = "#212121", 
    fillColor = fra$"color_resources",
    highlightOptions = leaflet::highlightOptions(color = "#212121", 
                                                 weight = 2, 
                                                 bringToFront = TRUE),
    label = labels_resources,
    labelOptions = leaflet::labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "text-align" = "center"),
      textsize = "15px",
      direction = "auto"),
    popup = bars_resources) |> 
  
  leaflet::addEasyButton(
    leaflet::easyButton(
      icon = 'fa-globe', title = 'Zoom initial',
      onClick = leaflet::JS('function(btn){ location.reload(); }'))) |> 
  
  leaflet::addLayersControl(
    baseGroups = c("Climat", "Biodiversité", "Ressources naturelles"),
    options = leaflet::layersControlOptions(collapsed = TRUE),
    position = 'topleft')



htmlwidgets::saveWidget(widget = map, file = here::here("content", 
                                                        "core.html"), 
                        selfcontained = FALSE)


## Change background color of leaflet map ----

html <- readLines(here::here("content", "core.html"))

html[grep('id=\"htmlwidget-', html)] <- 
  gsub('width:100%;', 
       'width:100%;background-color:white;', 
       html[grep('id=\"htmlwidget-', html)])

cat(paste0(html, collapse = "\n"), file = here::here("content", "core.html"))
