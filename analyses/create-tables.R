list_of_dep <- readRDS(here::here("data", "raw-data", 
                                  "list_of_departments.rds"))

list_of_dep <- list_of_dep[order(list_of_dep$"parser"), ]

classes <- c("climate", "biodiversity", "resources")

for (j in 1:length(classes)) {
  
  list_of_tables <- list()
  
  k <- 1
  
  dat <- readRDS(here::here("data", "derived-data", 
                            paste0("data_", classes[j], ".rds")))
  
  dat$"institution" <- ifelse(is.na(dat$"institution"), "", 
                              dat$"institution")
  
  dat$"scanr" <- ifelse(is.na(dat$"scanr"), "", dat$"scanr")
  
  dat$"scanr" <- ifelse(dat$"scanr" != "", 
                        paste0("<a href='", dat$"scanr", "'>ScanR</a>"),
                        "")
  
  dat$"email" <- paste0("<a href='mailto:", dat$"email", "'>", dat$"email", "</a>")
  
  for (i in 1:nrow(list_of_dep)) {
    
    dep <- which(colnames(dat) == list_of_dep[i, "parser"])
    sub_data <- dat[dat[ , dep] == 1, ]
    
    sub_data <- sub_data[ , c("firstname", "lastname", "email", "institution", 
                              "city", "scanr")]
    
    list_of_tables[[k]] <- DT::datatable(
      sub_data,
      caption  = htmltools::tags$caption(
        style = 'text-align: left;font-weight:bold;padding-bottom:20px;',
        paste0("Liste des experts - ", list_of_dep[i, "department"])),
      escape   = FALSE,
      rownames = FALSE,
      colnames = c("Prénom", "Nom", "Email", "Institution", "Ville", "ScanR"),
      options  = list(pageLength = 6, 
                      dom        = 'tip',
                      language   = list(info       = "Résultats _START_ à _END_ sur un total de _TOTAL_", 
                                        paginate = list("next"     = ">", 
                                                        "previous" = "<"))), 
      filter   = 'top')
    
    
    ## Export tooltip ----
    
    filename <- paste0(classes[j], "-", list_of_dep[i, "parser"])
    filename <- paste0("table-", filename, ".html")
    
    htmlwidgets::saveWidget(widget = list_of_tables[[k]], 
                            file = here::here("content", "tables", filename), 
                            selfcontained = FALSE)
    
    
    ## Move JS libs (only one for all barplots) ----
    
    dirname <- gsub("\\.html", "_files", filename)
    
    if (k == 1) {
      
      invisible(file.copy(here::here("content", "tables", dirname, "/"), 
                          here::here("content", "tables", "libs"), 
                          recursive = TRUE))
    }
    
    
    ## Delete JS libs ----
    
    unlink(here::here("content", "tables", dirname), recursive = TRUE)
    
    
    ## Correct path to JS libs ----
    
    html <- readLines(here::here("content", "tables", filename))
    html <- gsub(dirname, "libs", html)
    # html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    
    cat(paste0(html, collapse = '\n'), 
        file = here::here("content", "tables", filename))

    k <- k + 1
  }
  
  
  ## Export list of barplots ----
  
  # pop_up_graph <- leafpop::popupGraph(list_of_tables, type = "html", 
  #                                     width = 1150, height = 650)
  
  pop_up_graph <- leafpop::popupGraph(list_of_tables, type = "html", 
                                      width = 1000, height = 500)
  
  names(pop_up_graph) <- list_of_dep$"department"
  
  saveRDS(pop_up_graph, here::here("data", "derived-data", 
                                   paste0("popup_tables-", classes[j], ".rds")))
}
