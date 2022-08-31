## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

oldpar <- par(no.readonly = TRUE)

## ----eriacr, fig.width = 7, fig.align='center', fig.cap = "_Erigeron acris_ L. subsp. _acris_ distribution taken from [@zajacAtlasRozmieszczeniaRoslin2019]"----
par(mar = c(0, 0, 0, 0))
tif <- system.file("extdata/eriacr.tif", package = "atpolR")
r <- terra::rast(tif)
terra::plotRGB(r)

## ----rgbraster, echo = FALSE, message=FALSE, warning=FALSE, results='hide', fig.width = 7, fig.align='center', fig.cap="R, G and B layers of a raster"----
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 3))
reds <- colorspace::sequential_hcl(255, "Reds", rev = TRUE)
grns <- colorspace::sequential_hcl(255, "Greens", rev = TRUE)
blus <- colorspace::sequential_hcl(255, "Blues", rev = TRUE)

terra::plot(r[[1]], col = reds,  legend = FALSE, axes = FALSE)
terra::plot(r[[2]], col = grns,  legend = FALSE, axes = FALSE)
terra::plot(r[[3]], col = blus,  legend = FALSE, axes = FALSE)

par(mfrow = c(1, 1))

## ----rraster, echo = FALSE, message=FALSE, warning=FALSE, results='hide', fig.width = 7, fig.asp = 0.7, fig.align='center'----
terra::plot(r[[1]], col = colorspace::sequential_hcl(255, "Reds", rev = TRUE), axes = FALSE)

## ----echo = TRUE, message=FALSE, warning=FALSE, results='hide'----------------
m <- c(0,120, 0,
       120,255, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- terra::classify(r[[1]], rclmat, include.lowest = TRUE)

## ----reclasified, echo = FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.asp=0.7, fig.align='center', fig.cap="Reclasiffied raster with 0 --- as black and 1 --- as white"----
terra::plot(rc, axes = FALSE, col = c("black", "white"), legend = FALSE)

## ----eriacr-reverse, echo=TRUE, message=FALSE, warning=FALSE------------------
library(atpolR)

eriacr <- atpol10k()|>
  dplyr::mutate(a = mapply(function(x) check_atpol_square(x, rc), centroid)) |>
  dplyr::filter(a == "YES")

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
eriacr

## ----BE, echo=TRUE, fig.width=7, fig.asp=0.7, fig.align='center', message=FALSE, warning=FALSE----
BE <- atpol100k() |>
  subset(Name == "BE") |>
  sf::st_bbox()
par(pty = "s")
plot(NA, type = "n", xlim = c(BE[1], BE[3]), ylim = c(BE[2], BE[4]), axes = FALSE, xlab = "", ylab = "")
terra::plot(rc, legend = FALSE, add = TRUE)

atpol100k() |>
  subset(Name == "BE") |>
  sf::st_cast("LINESTRING") |>
  terra::plot(add = TRUE, col = "blue", lwd = 1.2)

eriacr |>
  subset(substr(Name, 1, 2) == "BE") |>
  sf::st_set_geometry("centroid") |>
  terra::plot(pch = 16, cex = 1.2, col = "blue", add = TRUE)

## ----myData, echo = TRUE, warning=FALSE---------------------------------------
myData <- atpol10k() |>
  dplyr::filter(Name %in% c("BE68", 
                            latlon_to_grid(51.13619, 16.95069, 4))) |>
  dplyr::mutate(a = "myData")

## ----echo = TRUE, eval=FALSE--------------------------------------------------
#  myData |>
#    sf::st_set_geometry("centroid") |>
#    terra::plot(pch = 16, cex = 1.8, col = "red", add = TRUE)

## ----echo=FALSE, fig.align='center', fig.asp=0.7, fig.cap="Data set extended with our observations in grids BE48 and BE68", message=FALSE, warning=FALSE, myDataplot, fig.width=7----
BE <- atpol100k() |>
  subset(Name == "BE") |>
  sf::st_bbox()
par(pty = "s")
plot(NA, type = "n", xlim = c(BE[1], BE[3]), ylim = c(BE[2], BE[4]), axes = FALSE, xlab = "", ylab = "")
terra::plot(rc, legend = FALSE, add = TRUE)

atpol100k() |>
  subset(Name == "BE") |>
  sf::st_cast("LINESTRING") |>
  terra::plot(add = TRUE, col = "blue", lwd = 1.2)

eriacr |>
  subset(substr(Name, 1, 2) == "BE") |>
  sf::st_set_geometry("centroid") |>
  terra::plot(pch = 16, cex = 1.2, col = "blue", add = TRUE)

myData |>
  sf::st_set_geometry("centroid") |>
  terra::plot(pch = 16, cex = 1.8, col = "red", add = TRUE)

## -----------------------------------------------------------------------------
eriacr <- eriacr |>
  rbind(myData) |>
  unique.data.frame()

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  plotPoitsOnAtpol(eriacr$centroid, main = "Erigeron acris subsp. acris", cex = 0.6)

## ----eriacratpol, echo = FALSE, fig.width=7, fig.asp=1, fig.align='center', fig.cap="Combined dataset drawn on ATPOL grid"----
par(mar = c(0, 0, 0, 0))
plotPoitsOnAtpol(eriacr$centroid, main = "Erigeron acris subsp. acris", cex = 0.6)

## -----------------------------------------------------------------------------
latlon_to_grid(51.01234, 17.23456, 4)
latlon_to_grid(51.01234, 17.23456, 6)

## -----------------------------------------------------------------------------
grid_to_latlon("CE50")

## -----------------------------------------------------------------------------
grid_to_latlon("CE50", xoffset = 1, yoffset = 1)

## -----------------------------------------------------------------------------
atpol100k()

## -----------------------------------------------------------------------------
atpol10k()

## ----atpoldiv, echo = FALSE, message=FALSE, warning=FALSE, results='hide', fig.width = 7, fig.align='center', fig.cap="Division by 2, 4 and 5 with adopted naming convection d, c, p"----
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 3))
for (d in c(2, 4, 5)) {
  a <- atpol_div("BE23", d)
  a$centroid <- sf::st_centroid(a$geometry)

  plot(a$geometry)
  a |>
    sf::st_set_geometry("centroid") |>
    subset(select = c("Name", "centroid")) |>
    terra::vect() |>
    terra::text(labels = substr(a$Name,5,7))
  
}
par(mfrow = c(1, 1))

## ----boundaryPL, fig.width=6, fig.align='center', fig.cap="Boundary of Poland on ATPOL grid."----
par(mar = c(0, 0, 0, 0))
b <- boundaryPL()
plot(atpol100k()$geometry)
plot(b, col = "red", add = TRUE)

## ---- include = FALSE---------------------------------------------------------
par(oldpar)

