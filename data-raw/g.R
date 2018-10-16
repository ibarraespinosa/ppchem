library(sf)
w = matrix(c(-180,-90,
             180,-90,
             180,90,
             -180,90,
             -180,-90),
           ncol=2,
           byrow=TRUE)
pts = list(w)
(pl1 = st_sfc(st_polygon(pts)))
length(pl1)
df <- data.frame(id = 1:length(pl1))
g <- st_sf(df, geometry = pl1, crs = 4326)
system.time(
  g <- st_make_grid(x = g, cellsize = 0.1)
  )
g$id <- 1:nrow(g)
sysdata <- list(g = g)
system.time(
save(g, file = "R/sysdata.rda", compress = "xz")
)
