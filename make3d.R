library(imager)
library(tidyverse)
library(plotly)
library(here)

# Use https://ezgif.com/split to split the Riddler gif into frames
# Read them, reduce resolutino

readshrink <- function(f) {
  img <- load.image(f) %>%
  resize(round(width(.)/4),round(height(.)/4)) %>%
    grayscale() %>%
    threshold() %>%
    as.cimg() %>% 
    as.data.frame() %>%
    select(value)
  
  img <- img[,1] %>% as.vector()
  dim(img) <- c(194, 178)
  img <- 1-img
  img[img == 0] <- NA
  return(img)
         }

setwd(here("jpg"))

files <- list.files(pattern = '*.jpg')
images <- map(files[3:299], readshrink)
setwd(here())

images_shift <- images
for (i in seq_along(images)) {
     images_shift[[i]] <- images[[i]] + (i*0.01)
}


ax <- list(
  title = "",
  zeroline = FALSE,
  showline = T,
  showticklabels = FALSE,
  showgrid = T
)

scene <- list(
  xaxis = ax,
  yaxis = ax,
  zaxis = ax)


p <- plot_ly(showscale=FALSE, hoverinfo='none', colors='Set1') %>%
     layout(scene=scene)
 
for (i in seq_along(images))
{ dfk <- images_shift[[i]]
    p <- add_surface(p, z = dfk, data=dfk) }
p
