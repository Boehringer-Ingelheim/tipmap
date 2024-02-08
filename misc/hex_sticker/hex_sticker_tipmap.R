# load packages
library(tipmap)
library(hexSticker)

# generate subplot
tipmap_data <- load_tipmap_data("tipdat.rds")
p <- tipmap_plot(tipmap_data)
p <- p + theme_void() + theme_transparent()

# colors
tipmap_darkblue <- grDevices::rgb(0, 102, 153, max = 255)
tipmap_lightred <- grDevices::rgb(204, 51, 51, max = 255)

# generate sticker
sticker(
  subplot = p,
  #img_path = "/misc/hex_sticker", 
  package = "tipmap",
  p_size = 20,
  s_x = 2.32, s_y = 1.4075, s_width = 8.2, s_height = 6,
  p_y = 1.4,
  p_color = tipmap_lightred,
  h_fill = "white",
  h_color = tipmap_darkblue,
  white_around_sticker = TRUE,
  filename = "hex_sticker_tipmap.png" # alt: "<...>.svg"
)
