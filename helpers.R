### Functions
# Adapted from Claus Wilke's code
# https://github.com/clauswilke/isoband
sf_from_image <- function(image, nbands) {
  image_gray <- image %>% image_quantize(colorspace = "gray")
  image_raster <- as.raster(image_gray)
  d <- dim(image_raster)
  m <- matrix(c((255 - col2rgb(image_raster)[1, ])),
              nrow = d[1], ncol = d[2], byrow = TRUE
  )
  b <- isobands(1:d[2], d[1]:1, m, 20 * (0:(nbands - 1)), 20 * (1:nbands))
  bands <- iso_to_sfg(b)
  data <- st_sf(
    level = letters[1:length(bands)],
    geometry = st_sfc(bands)
  )
}

# specify a rainbow palette
lgbt <- c(
  `a` = "#FFEF00", # Canary Yellow
  `b` = "#FF8C00", # Dark Orange
  `c` = "#E70000", # Electric Red
  `d` = "#00811F", # La Salle Green
  `e` = "#0044FF", # Blue (RYB)
  `f` = "#760089", # Patriarch
  `g` = "#333333" # Grey (highlight)
)

filter_from_cam <- function(filter){
  if(filter=="brickr") {
    mosaic1 <- jpeg::readJPEG("images/cam.jpeg") %>%
      image_to_mosaic(img_size = 36)
    brickr_img <- mosaic1 %>% build_mosaic()
    frames <- image_graph(width = 400, height = 400)
    print(brickr_img)
    tmpbrickr <- magick::image_animate(frames, 1) %>%
      image_write(tempfile(fileext = "jpg"), format = "jpg")
  }
}