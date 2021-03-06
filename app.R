library(shiny)
library(ggplot2)
library(shinyMobile)
library(shinysense)
library(tidyverse)
library(magick)
# library(shinyWidgets)  ## confirm if I actually need this?
library(imager)
library(scales)
library(packcircles)
library(isoband)
library(sf)
library(brickr)
library(ggvoronoi)
library(colorfindr)
library(sketcher)
library(ggforce)
library(shinycssloaders)
library(reticulate)

source("helpers.R")

shinyApp(
  ui = f7Page(
    title = "Shiny filteRs",
    preloader = FALSE,
    loading_duration = 3,
    options = list(
      theme = "auto",
      dark = FALSE,
      filled = TRUE,
      color = "#c32aa3",
      touch = list(tapHold = TRUE, tapHoldDelay = 750, iosTouchRipple = FALSE),
      iosTranslucentBars = TRUE,
      navbar = list(iosCenterTitle = TRUE, hideNavOnPageScroll = TRUE),
      toolbar = list(hideNavOnPageScroll = FALSE),
      pullToRefresh = FALSE
    ),
    allowPWA = FALSE,
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Instagram Nation", side = "left", theme = "light", '"As humans, we have a deep-seated fear that we’re wasting every moment we don’t remember. Inwardly, we believe that every moment of existence is essentially our life, and whatever we don’t remember is simply being spent carelessly... Through the use of vintage-looking photo filters, we feel nostalgic about moments as they happen, before they can even be considered nostalgic. And all of this happens subconsciously."- Laura Roja', effect = "cover"),
        f7Panel(title = "DuctTape Programmer", side = "right", theme = "dark", '"He is the guy you want on your team building go-carts, because he has two favorite tools: duct tape and WD-40. And he will wield them elegantly even as your go-cart is careening down the hill at a mile a minute. This will happen while other programmers are still at the starting line arguing over whether to use titanium or some kind of space-age composite material that Boeing is using in the 787 Dreamliner." - Joel Spolsky', effect = "cover")
      ),
      navbar = f7Navbar(
        title = "Tabs",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = TRUE,
        rightPanel = TRUE
      ),
      tags$style(
        ".page-content{
          background: url('https://images.unsplash.com/photo-1579546928686-286c9fbde1ec?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=584&q=80') no-repeat center center fixed;
          -webkit-background-size: cover;
          -moz-background-size: cover;
          -o-background-size: cover;
          background-size: cover;
        }"
      ),
      f7Tabs(
        animated = FALSE,
        swipeable = FALSE,
        f7Tab(
          tabName = "Camera",
          icon = f7Icon("camera_fill"),
          active = TRUE,
          f7Link(label = "Twitter", href = "https://twitter.com/MattOldach", icon = f7Icon("logo_twitter")),
          f7Link(label = "LinkedIn", href = "https://www.linkedin.com/in/matthewoldach/", icon = f7Icon("logo_linkedin")),
          f7Link(label = "Instagram", href = "https://www.instagram.com/lovedrop69/", icon = f7Icon("logo_instagram")),
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "📸 from {shinysense}",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              shinyviewr_UI("myCamera", height = "400px"),
              shinycssloaders::withSpinner(imageOutput("snapshot"))
            )
          ),
          f7ExpandableCard(
            id = "card4",
            title = "About the App",
            fullBackground = TRUE,
            image = "https://images.unsplash.com/photo-1582502580092-0dc3088c7aeb?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=967&q=80",
            "Shiny-filteRs is an Instagram-like application built on top of the latest Framework7 template <https://framework7.io> and features 12 Python & R 'filters', which are not only aesthetically pleasing but also based on mathematical formulae (e.g. Voronoi diagram, Circle Packing, etc.). SPAM Photo by Hannes Johnson on Unsplash. Background Photo by Gradienta on Unsplash"
          ),
        ),
        f7Tab(
          tabName = "Filter",
          icon = f7Icon("paintbrush_fill"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "🖌️ Filter Selection 🎨 ",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              f7Select(
                inputId = "filterPicker",
                label = "😍 Make your selection here 😍",
                selected = "Packed Circles",
                choices = c(
                  "Packed Circles",
                  "Voronoi Diagram",
                  "LGBT🌈",
                  "Lego Mosaic",
                  "BSpline Portrait",
                  "Line Portrait",
                  "Rego Portrait",
                  "Split-Bar Portrait",
                  "Sketcher",
                  "Cascade",
                  "Glitch",
                  "Pixelate"
                )
              ),
              shinycssloaders::withSpinner(imageOutput("filter"))
            )
          )
        ),
        f7Tab(
          tabName = "Magic",
          icon = f7Icon("color_filter"),
          active = FALSE,
          navbar = f7Navbar(
            title = "Tabs",
            hairline = TRUE,
            shadow = TRUE,
            leftPanel = TRUE,
            rightPanel = TRUE
          ),
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Add some ✨🧙‍♂️✨ {magick}",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              shinycssloaders::withSpinner(imageOutput("image_magick")),
              br(),
              footer = tagList(
                f7Button("toggleBlur", color = "pink", label = "Blur", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small"),
                f7Sheet(
                  id = "blurSheet",
                  label = "Blur",
                  orientation = "bottom",
                  rounded = TRUE,
                  size = "small",
                  swipeToClose = TRUE,
                  backdrop = TRUE,
                  f7Slider(
                    "blur",
                    "Blur:",
                    min = 0, max = 20,
                    value = 0,
                    step = 1,
                    color = "green"
                  )
                ),
                f7Button("toggleImplode", color = "yellow", label = "Implode", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small"),
                f7Sheet(
                  id = "implodeSheet",
                  label = "Implode",
                  orientation = "bottom",
                  swipeToClose = TRUE,
                  backdrop = TRUE,
                  f7Slider(
                    "implode",
                    "Implode:",
                    min = -1,
                    max = 1,
                    value = 0,
                    step = 0.1,
                    color = "green"
                  )
                ),
                f7Button("toggleRotate", color = "orange", label = "Rotate", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small"),
                f7Sheet(
                  id = "rotateSheet",
                  label = "Rotate",
                  orientation = "bottom",
                  swipeToClose = TRUE,
                  backdrop = TRUE,
                  f7Slider(
                    "rotation",
                    "Rotate:",
                    min = 0,
                    max = 360,
                    value = 0,
                    step = 1,
                    color = "green"
                  )
                ),
                f7Toggle("toggleCharcoal", color = "red", label = "Charcoal", checked = FALSE),
                f7Toggle("toggleEdge", color = "green", label = "Edge", checked = FALSE),
                f7Toggle("toggleNegate", color = "blue", label = "Negate", checked = FALSE),
                f7Toggle("toggleFlip", color = "yellow", label = "Flip", checked = FALSE),
                f7Toggle("toggleFlop", color = "pink", label = "Flop", checked = FALSE),
              ),
              f7DownloadButton("downloadImage", label = "Download your image!")
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {

    ## Load Virtual env with dependencies
    reticulate::virtualenv_create(envname = "pyFilters", python = "/usr/bin/python3")
    reticulate::virtualenv_install("pyFilters", packages = c("glitchart", "pixelate", "Pillow", "tqdm"))
    reticulate::use_virtualenv("pyFilters", required = TRUE)

    imageLoc <- reactiveVal("images/cam.jpeg")
    ## convert the img location to an img value
    imageVal <- reactive({
      image_convert(image_read(imageLoc()), "jpeg")
    })

    myCamera <- callModule(
      shinyviewr,
      "myCamera",
      output_height = 250,
      output_width = 250
    )

    # logic for what happens after a user has drawn their values.
    observeEvent(myCamera(), {
      photo <- myCamera()

      # Save plot as jpeg first
      jpeg(filename = "images/cam.jpeg")
      plot(as.raster(photo))
      dev.off()
      # load jpeg, trim and crop then re-save
      tmp <- image_read("images/cam.jpeg")
      tmp <- image_trim(tmp) %>%
        image_crop(geometry = "340X340+2+13")
      image_write(tmp, "images/cam.jpeg")

      output$snapshot <- renderPlot(
        {
          # plot the image into tab1
          plot(as.raster(photo))
        },
        bg = "transparent",
        execOnResize = TRUE
      )

      observeEvent(input$filterPicker, {
        if (input$filterPicker == "Cascade") {
          system("python python/cascade.py images/cam.jpeg")
          photo <- load.image("images/cascade_cam.jpeg")
          frames <- image_graph(width = 300, height = 400)
          plot(photo, axes = FALSE)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
          # Check its existence
          if (file.exists("images/cascade_cam.jpeg")) {
            # Delete file if it exists
            file.remove("images/cascade_cam.jpeg")
          }
        } else if (input$filterPicker == "Glitch") {
          py_run_file("python/glitchart.py")
          photo <- imager::load.image("cam_glitch.jpg")
          frames <- image_graph(width = 300, height = 400)
          plot(photo, axes = FALSE)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
          if (file.exists("cam_glitch.jpg")) {
            # Delete file if it exists
            file.remove("cam_glitch.jpg")
          }
        } else if (input$filterPicker == "Pixelate") {
          py_run_file("python/pixelate.py")
          photo <- imager::load.image("images/pixelate_cam.jpeg")
          frames <- image_graph(width = 300, height = 400)
          plot(photo, axes = FALSE)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpeg"), format = "jpg")
          if (file.exists("images/pixelate_cam.jpeg")) {
            # Delete file if it exists
            file.remove("images/pixelate_cam.jpeg")
          }
        } else if (input$filterPicker == "Packed Circles") {
          im <- load.image("images/cam.jpeg")
          ## Convert Image into Data Frame
          im.df.colour <- im %>%
            as.data.frame(wide = "c") %>%
            ## so that rgb value is in separate column.
            rename(im_x = x, im_y = y) %>%
            mutate(hex = rgb(c.1, c.2, c.3))

          ## Step 2 using circleProgressiveLayout function.
          ## Generate circle packing layout using rbeta distribution as size of circles
          pack_layout <- circleProgressiveLayout(rbeta(2000, 1, 2), sizetype = "area") %>%
            ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling.
            mutate(
              im_x = floor(rescale(x, to = range(im.df.colour$im_x))),
              im_y = floor(rescale(y, to = range(im.df.colour$im_y))),
              ## also generate id, so i can join the data frame easily later!
              id = row_number()
            ) %>%
            inner_join(im.df.colour %>% select(im_x, im_y, hex), by = c("im_x", "im_y"))

          ## Step 4
          ## Using the layout above create data frame using circleLayoutVertices function so that you can plot circle using ggplot2
          data_gg <- circleLayoutVertices(pack_layout) %>%
            inner_join(pack_layout %>% select(id, hex), by = c("id"))

          ## Step 5
          photo <- data_gg %>%
            ggplot(aes(x = x, y = y, group = id)) +
            geom_polygon(aes(fill = hex)) +
            scale_fill_identity() +
            coord_equal() +
            scale_y_reverse() + ## you need to reverse y-axis
            theme_void()

          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "LGBT🌈") {
          image_sf <- "images/cam.jpeg" %>%
            image_read() %>%
            image_resize(geometry = "200x200") %>%
            sf_from_image(nbands = 7)
          # draw image
          photo <- image_sf %>%
            ggplot(aes(fill = level, color = level)) +
            geom_sf(size = 0.1, show.legend = FALSE) +
            scale_color_manual(values = lgbt) +
            scale_fill_manual(values = lgbt) +
            coord_sf(expand = FALSE) +
            theme_void() +
            theme(panel.background = element_rect(fill = "black"))

          frames <- image_graph(width = 250, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Lego Mosaic") {
          # Brickr Mosaic
          mosaic1 <- jpeg::readJPEG("images/cam.jpeg") %>%
            image_to_mosaic(img_size = 36) # Length of each side of mosaic in "bricks"
          # Plot 2D mosaic
          photo <- mosaic1 %>% build_mosaic()

          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Voronoi Diagram") {
          img <- load.image("images/cam.jpeg")
          # Represent the image as a data frame
          img_df <- as.data.frame(img)
          # Add more expressive labels to the colors
          img_df <- img_df %>%
            mutate(channel = case_when(
              cc == 1 ~ "Red",
              cc == 2 ~ "Green",
              cc == 3 ~ "Blue"
            ))

          # Reshape the data frame so that each row is a point
          img_wide <- img_df %>%
            select(x, y, channel, value) %>%
            spread(key = channel, value = value) %>%
            mutate(
              color = rgb(Red, Green, Blue)
            )
          # Take a sample of rows from the data frame
          sample_size <- 2000
          img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
          # Create a Voronoi Diagram of the sampled points
          photo <- ggplot(img_sample) +
            geom_voronoi(mapping = aes(x = x, y = y, fill = color)) +
            scale_fill_identity() +
            scale_y_reverse() +
            theme_void()
          frames <- image_graph(width = 250, height = 300)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "BSpline Portrait") {
          img <- image_read("images/cam.jpeg") %>%
            image_convert(colorspace = "gray")
          # Get dimensions
          img_w <- image_info(img)$width
          img_h <- image_info(img)$height
          img_ratio <- img_w / img_h

          # Resize the longest dimension to 80 pixels
          if (img_w >= img_h) {
            img <- image_resize(img, "80")
          } else {
            img <- image_resize(img, ("x80"))
          }

          # Create array and number rows and columns
          img_array <- drop(as.integer(img[[1]]))
          rownames(img_array) <- 1:nrow(img_array)
          colnames(img_array) <- 1:ncol(img_array)

          # Create data frame from array and rename columns
          img_df <- as.data.frame.table(img_array) %>%
            `colnames<-`(c("y", "x", "b")) %>%
            mutate(
              across(everything(), as.numeric),
              # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
              bf = 1 - b / 255,
              n = row_number()
            ) %>%
            group_by(n) %>%
            mutate(
              bx = list(c(x, x + bf * runif(1, 1, 3), x + bf * runif(1, 1, 3), x)),
              by = list(c(y + bf * runif(1, 1, 3), y + bf * runif(1, 1, 3), y, y))
            ) %>%
            ungroup() %>%
            unnest(c(bx, by))

          # Colors, fill and background
          col_fill <- "black"
          col_bg <- "#F1E34C"

          photo <- ggplot(img_df) +
            geom_bspline_closed(aes(x = bx, y = by, group = n, alpha = bf), fill = col_fill, color = NA, size = 0.3) +
            scale_y_reverse() +
            scale_alpha_identity() +
            # coord_fixed(expand = FALSE) +
            theme_void() +
            theme(
              legend.position = "none",
              plot.background = element_rect(fill = col_bg, color = NA)
            )
          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Line Portrait") {
          img <- image_read("images/cam.jpeg") %>%
            image_convert(colorspace = "gray")
          # Get dimensions
          img_w <- image_info(img)$width
          img_h <- image_info(img)$height

          # Resize the longest dimension to 80 pixels
          if (img_w >= img_h) {
            img <- image_resize(img, "80")
          } else {
            img <- image_resize(img, ("x80"))
          }

          # Create array and number rows and columns
          img_array <- drop(as.integer(img[[1]]))
          rownames(img_array) <- 1:nrow(img_array)
          colnames(img_array) <- 1:ncol(img_array)

          # Create data frame from array and rename columns
          img_df <- as.data.frame.table(img_array) %>%
            `colnames<-`(c("y", "x", "b")) %>%
            mutate(
              across(everything(), as.numeric),
              # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
              bf = 1 - b / 255
            ) %>%
            # Create extra "steps" for the sine curves
            rowwise() %>%
            mutate(t = list(x + seq(0, 1, by = 0.05))) %>%
            unnest(t)

          # Colors, fill and background
          col_fill <- "black"
          col_bg <- "#E335C2"

          photo <- ggplot(img_df) +
            geom_path(aes(x = t, y = y + bf * sin(4 * pi * t) / 2, group = y), color = col_fill) +
            scale_y_reverse() +
            coord_fixed(expand = FALSE) +
            theme_void() +
            theme(
              legend.position = "none",
              plot.background = element_rect(fill = col_bg, color = NA)
            )

          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Rego Portrait") {
          img <- image_read("images/cam.jpeg") %>%
            image_convert(colorspace = "gray")
          # Get dimensions
          img_w <- image_info(img)$width
          img_h <- image_info(img)$height
          img_ratio <- img_w / img_h

          # Resize the longest dimension to 80 pixels
          if (img_w >= img_h) {
            img <- image_resize(img, "80")
          } else {
            img <- image_resize(img, ("x80"))
          }

          # Create array and number rows and columns
          img_array <- drop(as.integer(img[[1]]))
          rownames(img_array) <- 1:nrow(img_array)
          colnames(img_array) <- 1:ncol(img_array)

          # Create data frame from array and rename columns
          img_df <- as.data.frame.table(img_array) %>%
            `colnames<-`(c("y", "x", "b")) %>%
            mutate(
              across(everything(), as.numeric),
              # convert b (0-255) to bf (1-0), so that "brighter" values become smaller points
              bf = 1 - b / 255,
              n = row_number()
            )

          # Colors, fill and background
          col_fill <- "black"
          col_bg <- "#FE7F9C" # Watermelon

          photo <- ggplot(img_df) +
            # geom_point(aes(x = x, y = y, size = bf), color = col_fill) +
            geom_regon(aes(x0 = x, y0 = y, angle = 0, r = bf, sides = 1 + bf * 8, group = n), fill = NA, color = "black") +
            scale_y_reverse() +
            # scale_size_continuous(range = c(0, 1)) +
            coord_fixed(expand = FALSE) +
            theme_void() +
            theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA))
          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Split-Bar Portrait") {
          img <- image_read("images/cam.jpeg") %>%
            image_convert(colorspace = "gray")
          # Get dimensions
          img_w <- image_info(img)$width
          img_h <- image_info(img)$height

          # Resize the longest dimension to 80 pixels
          if (img_w >= img_h) {
            img <- image_resize(img, "80")
          } else {
            img <- image_resize(img, ("x80"))
          }

          # Create array and number rows and columns
          img_array <- drop(as.integer(img[[1]]))
          rownames(img_array) <- 1:nrow(img_array)
          colnames(img_array) <- 1:ncol(img_array)

          # Create data frame from array and rename columns
          img_df <- as.data.frame.table(img_array) %>%
            `colnames<-`(c("y", "x", "b")) %>%
            mutate(
              across(everything(), as.numeric),
              # convert b (0-255) to bf (1-0), so that "brighter" values become smaller bars
              bf = 1 - b / 255
            )

          # Colors, fill and background
          col_fill <- "#008080"
          col_bg <- "#FFA500"

          photo <- ggplot(img_df) +
            geom_rect(aes(xmin = x, xmax = x + bf * 0.9, ymin = y, ymax = y + 0.85), fill = col_fill, color = NA) +
            scale_y_reverse() +
            coord_fixed(expand = FALSE) +
            theme_void() +
            theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA))
          frames <- image_graph(width = 300, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Sketcher") {
          # Create a 16 colour target palette from the image
          img <- im_load("images/cam.jpeg")
          im2 <- sketch(img, style = 1, lineweight = 0.8, shadow = 0.25) # may take some seconds
          photo <- plot(im2)
          frames <- image_graph(width = 300, height = 400)
          plot(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        }

        output$filter <- renderImage(
          {
            image <- image_read(tmpimg) %>%
              image_resize("300x400")

            tmpfile <- image %>%
              image_write(tempfile(fileext = "jpg"), format = "jpg")

            # Return a list
            list(src = tmpfile, contentType = "photo/jpeg")
          },
          deleteFile = FALSE
        )

        updatedImageLoc <- reactive({
          image <- image_read(tmpimg)

          ## Boolean operators
          if (input$toggleCharcoal == TRUE) {
            image <- image %>%
              image_charcoal()
          }

          if (input$toggleEdge == TRUE) {
            image <- image %>%
              image_edge()
          }

          if (input$toggleNegate == TRUE) {
            image <- image %>%
              image_negate()
          }

          if (input$toggleFlip == TRUE) {
            image <- image %>%
              image_flip()
          }

          if (input$toggleFlop == TRUE) {
            image <- image %>%
              image_flop()
          }

          tmpfile <- image %>%
            image_implode(input$implode) %>%
            image_blur(input$blur, input$blur) %>%
            image_rotate(input$rotation) %>%
            image_resize("300x400") %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")

          tmpfile
        })

        output$image_magick <- renderImage(
          {
            # Return a list
            list(src = updatedImageLoc(), contentType = "photo/jpeg")
          },
          deleteFile = FALSE
        )

        output$downloadImage <- downloadHandler(
          filename = "Shiny-filteR.jpeg",
          contentType = "image/jpeg",
          content = function(file) {
            ## copy the file from the updated image location to the final download location
            im <- image_read(updatedImageLoc())
            image_write(im, file)
          }
        )
      })
    })

    observeEvent(input$toggleBlur, {
      updateF7Sheet(id = "blurSheet")
    })

    observeEvent(input$toggleImplode, {
      updateF7Sheet(id = "implodeSheet")
    })

    observeEvent(input$toggleRotate, {
      updateF7Sheet(id = "rotateSheet")
    })

    output$pickerval <- renderText(input$filterPicker)

    observeEvent(input$update, {
      updateF7Picker(
        inputId = "filterPicker",
        value = "Packed Circles",
        choices = c(
          "Packed Circles",
          "Voronoi Diagram",
          "LGBT🌈",
          "Lego Mosaic",
          "BSpline Portrait",
          "Line Portrait",
          "Rego Portrait",
          "Split-Bar Portrait",
          "Ditherer",
          "Sketcher",
          "Cascade",
          "Glitch",
          "Pixelate"
        ),
        openIn = "sheet",
        toolbarCloseText = "Close",
        sheetSwipeToClose = TRUE
      )
    })
  }
)
