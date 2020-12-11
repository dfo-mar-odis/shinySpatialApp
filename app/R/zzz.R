# load template 
names(rmd_list) <- rmd_list <- list.files("Rmd",
  pattern = "\\.[Rr]md$", full.names = TRUE)

#
fl <- list.files("data/", full.names = TRUE)
lay <- lapply(fl, function(x) sf::st_layers(x)$name)
tb_ref <- data.frame(
         id = seq_len(sum(lengths(lay))), 
         dsn = rep(unlist(fl), lengths(lay)),
         layer = unlist(lay)
       )
spa <- lapply(list.files("data", full.names = TRUE), readSpatial)

msgInfo("R files loaded")

