# Set up

setwd("D:/ML/Projects/Image extraction")
library(png)

# Load images

img.a <- readPNG("lena.png")
img.b <- readPNG("lena_modified.png")

# Data checking

str(img.a) ; str(img.b) # Array 512*512*4

# Difference extraction

diff.arr <- array(1L, dim = c(512, 512, 4))
diff.index <- which(img.a != img.b)
diff.arr[diff.index] <- img.b[diff.index]

# Export image

writePNG(diff.arr, "difference.png")
