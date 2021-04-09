library(shiny)
library(ggplot2)
library(shinyMobile)
library(shinysense)
library(tidyverse)
library(magick)
library(shinyWidgets)  ## confirm if I actually need this?
library(imager)
library(scales)
library(packcircles)
library(isoband)
library(sf)
library(brickr)
library(ggvoronoi)
library(ditherer)
library(colorfindr)
library(sketcher)
library(ggforce)


source("helpers.R")

shinyApp(
  ui = f7Page(
    title = "Shiny filteRs",
    preloader = FALSE,
    loading_duration = 3,
    options = list(
      theme = c("ios", "md", "auto", "aurora"),
      dark = TRUE,
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
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover"),
        f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
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
          tabName = "Tab 1",
          icon = f7Icon("camera_fill"),
          active = TRUE,
          f7Flex(
            prettyRadioButtons(
              inputId = "theme",
              label = "Select a theme:",
              thick = TRUE,
              inline = TRUE,
              selected = "md",
              choices = c("ios", "md"),
              animation = "pulse",
              status = "info"
            ),
            prettyRadioButtons(
              inputId = "color",
              label = "Select a color:",
              thick = TRUE,
              inline = TRUE,
              selected = "dark",
              choices = c("light", "dark"),
              animation = "pulse",
              status = "info"
            )
          ),
          f7Link(label = "Twitter", href = "https://twitter.com/MattOldach", icon = f7Icon("logo_twitter")),
          f7Link(label = "LinkedIn", href = "https://www.linkedin.com/in/matthewoldach/", icon = f7Icon("logo_linkedin")),
          f7Link(label = "Instagram", href = "https://www.instagram.com/lovedrop69/", icon = f7Icon("logo_instagram")),
          tags$head(
            tags$script(
              'Shiny.addCustomMessageHandler("ui-tweak", function(message) {
                var os = message.os;
                var skin = message.skin;
                if (os === "md") {
                  $("html").addClass("md");
                  $("html").removeClass("ios");
                  $(".tab-link-highlight").show();
                } else if (os === "ios") {
                  $("html").addClass("ios");
                  $("html").removeClass("md");
                  $(".tab-link-highlight").hide();
                }
                if (skin === "dark") {
                 $("html").addClass("theme-dark");
                } else {
                  $("html").removeClass("theme-dark");
                }
               });
              '
            )
          ),
          
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "️📸 from {shinysense}",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              shinyviewr_UI("myCamera", height = "400px"),
              br(),
              imageOutput("snapshot")
            )
          ),
          f7ExpandableCard(
            id = "card4",
            title = "About the App",
            fullBackground = TRUE,
            image = "https://images.unsplash.com/photo-1582502580092-0dc3088c7aeb?ixlib=rb-1.2.1&ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&auto=format&fit=crop&w=967&q=80",
            "Shiny-filteRs is an instagram-like application built on top of the latest Framework7 template <https://framework7.io> to develop progressive web shinyapps (PWA, similar to native apps). SPAM Photo by Hannes Johnson on Unsplash. Background Photo by Gradienta on Unsplash"
          ),
        ),
        
        f7Tab(
          tabName = "Tab 2",
          icon = f7Icon("paintbrush_fill"),
          active = FALSE,
          
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "🖌️ Filter Selection 🎨",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              f7Picker(
                inputId = "filterPicker",
                placeholder = "Some text here!",
                label = "😍 Make your selection here 😍",
                choices = c("Packed Circles", 
                            "Voronoi Diagram", 
                            "LGBT", 
                            "Lego Mosaic",
                            "BSpline Portrait",
                            "Line Portrait",
                            "Rego Portrait",
                            "Split-Bar Portrait",
                            "Ditherer",
                            "Sketcher")
              ),
              br(),
              imageOutput("filter")
            )
          )
        ),
        f7Tab(
          tabName = "Tab 3",
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
              title = "Add some 🧙✨ {magick}",
              image = "https://images.unsplash.com/photo-1557682224-5b8590cd9ec5?ixid=MXwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHw%3D&ixlib=rb-1.2.1&auto=format&fit=crop&w=1015&q=80",
              imageOutput("image_magick"),
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
                f7Button("toggleImplode", color = "blue", label = "Implode", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small"),
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
                f7Button("toggleRotate", color = "purple", label = "Rotate", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small"),
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
                f7Button(inputId = "go", color = "orange", label = "Charcoal", fill = TRUE, rounded = TRUE, shadow = TRUE, size = "small")
              )
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    myCamera <- callModule(
      shinyviewr,
      "myCamera",
      output_height = 250,
      output_width = 250
    )
    
    ### TRY PLACING YOUR OTHER TWO WHICH ARE DEPENDENT WITHIN THE OBSERVENT? OR DO I CREATE MY OWN?
    # logic for what happens after a user has drawn their values.
    observeEvent(myCamera(), {
      photo <- myCamera()
      
      ##### Above/Below may need to be modified for DPED
      
      # Save plot as jpeg first
      jpeg(filename = "images/cam.jpeg")
      plot(as.raster(photo))
      dev.off()
      # load jpeg, trim and crop then re-save
      tmp <- image_read("images/cam.jpeg")
      tmp <- image_trim(tmp) %>% 
        image_crop(geometry="340X340+2+13")
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
        if(input$filterPicker == "Packed Circles") {
          im <- load.image("images/cam.jpeg")
          ## Convert Image into Data Frame
          im.df.colour <- im %>%
            as.data.frame(wide = "c") %>% ## so that rgb value is in separate column.
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
          
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
          
        } else if(input$filterPicker == "LGBT"){
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
          
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if(input$filterPicker == "Lego Mosaic"){
          # Brickr Mosaic
          mosaic1 <- jpeg::readJPEG("images/cam.jpeg") %>%
            image_to_mosaic(img_size = 36) # Length of each side of mosaic in "bricks"
          # Plot 2D mosaic
          photo <- mosaic1 %>% build_mosaic()
          
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if(input$filterPicker == "Voronoi Diagram"){
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
          
        } else if(input$filterPicker == "BSpline Portrait"){ 
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
            theme(legend.position = "none",
                  plot.background = element_rect(fill = col_bg, color = NA)) 
          
          
        }  else if(input$filterPicker == "Line Portrait"){ 
          img <- image_read("images/cam.jpeg")%>%
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
            theme(legend.position = "none",
                  plot.background = element_rect(fill = col_bg, color = NA))
          
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if(input$filterPicker == "Rego Portrait"){ 
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
          col_bg <- "#FE7F9C"  # Watermelon
          
          photo <- ggplot(img_df) +
            # geom_point(aes(x = x, y = y, size = bf), color = col_fill) +
            geom_regon(aes(x0 = x, y0 = y, angle = 0, r = bf, sides = 1 + bf * 8, group = n), fill = NA, color = "black") +
            scale_y_reverse() +
            # scale_size_continuous(range = c(0, 1)) +
            coord_fixed(expand = FALSE) +
            theme_void() +
            theme(legend.position = "none", plot.background = element_rect(fill = col_bg, color = NA)) 
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if(input$filterPicker == "Split-Bar Portrait"){ 
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
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Ditherer"){
          # Create a 16 colour target palette from the image
          img <- "images/cam.jpeg"
          set.seed(1)
          tp <-
            colorfindr::get_colors(img) %>% 
            colorfindr::make_palette(n = 32, show = FALSE)
          photo <- dither(img, target_palette = tp) 
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        } else if (input$filterPicker == "Sketcher"){
          # Create a 16 colour target palette from the image
          img <- im_load("images/cam.jpeg")
          im2 = sketch(img, style = 1, lineweight = 0.8, shadow = 0.25) # may take some seconds
          photo <- plot(im2)
          frames <- image_graph(width = 400, height = 400)
          print(photo)
          tmpimg <- magick::image_animate(frames, 1) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
        }
        
        
        

      
        output$filter <- renderPlot(
          {
            photo
          },
          bg = "transparent",
          execOnResize = TRUE
        )
          
        output$image_magick <- renderImage({
          image <- image_read(tmpimg)
          tmpfile <- image %>%
            image_implode(input$implode) %>%
            image_blur(input$blur, input$blur) %>%
            image_rotate(input$rotation) %>%
            # image_resize(input$size) %>%
            image_write(tempfile(fileext = "jpg"), format = "jpg")
          # Return a list
          list(src = tmpfile, contentType = "photo/jpeg")
        }, deleteFile = FALSE)
        
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
    
    output$selected_tab <- renderUI({
      HTML(paste0("Access the currently selected tab: ", strong(input$menu)))
    })
    
    output$pickerval <- renderText(input$filterPicker)
    
    observeEvent(input$update, {
      updateF7Picker(
        inputId = "filterPicker",
        value = "Packed Circles",
        choices = c("Packed Circles", 
                    "Voronoi Diagram", 
                    "LGBT", 
                    "Lego Mosaic",
                    "BSpline Portrait",
                    "Line Portrait",
                    "Rego Portrait",
                    "Split-Bar Portrait",
                    "Ditherer",
                    "Sketcher"),
        openIn = "sheet",
        toolbarCloseText = "Close",
        sheetSwipeToClose = TRUE
      )
    })
    
    # send the theme to javascript
    observe({
      session$sendCustomMessage(
        type = "ui-tweak",
        message = list(os = input$theme, skin = input$color)
      )
    })
  }
)
