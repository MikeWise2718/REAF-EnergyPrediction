getErgPaths <- function(bpath="../5-prescriptive/"){
  path <- list()
  path$graph <- paste0(bpath,"graphs")
  path$qdata <- paste0(bpath,"qdata")
  # these later paths appear unused?
  path$data <- paste0(bpath,"data")
  path$tdata <- paste0(bpath,"tdata")
  path$runparm <- paste0(bpath,"rundata")
  path$wdata <- paste0(bpath,"wdata")
  return(path)
}