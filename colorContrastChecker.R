# This script calculates the contrast between two colors, calculations taken from here:
# https://planetcalc.com/7779/

calcLuminance <- function(hexCode = NULL,
                          RGB = NULL) {
  if (!is.null(hexCode)) {
    rgbVal <- as.vector(col2rgb(hexCode))/255
  } 
  
  if (!is.null(RGB)) {
    rgbVal <- rgb/255
  }
  
  linearRGB <- setNames(lapply(rgbVal, function(x) {
    if (x > 0.03928) {
      C <- ((x + 0.055)/1.055)^2.4
    } else {
      C <- x/12.92
    }
  }),
  c("r", "g", "b"))

  L <- 0.2126 * linearRGB$r + 0.7152 * linearRGB$g + 0.0722 * linearRGB$b
}

calcContrast <- function(hexVector,
                         threshold = 3) {
  
  findContrast <- function(hex) {
    hex1 <- calcLuminance(hex[1])
    hex2 <- calcLuminance(hex[2])
    
    # L1 > L2
    if (hex1 > hex2) {
      L1 <- hex1
      L2 <- hex2
    } else {
      L1 <- hex2
      L2 <- hex1
    }
    
    contrast <- round((L1 + 0.05) / (L2 + 0.05), 2)
    pass <- ifelse(contrast > threshold, "yes", "no")
    data.frame(L1 = hex[1],
               L2 = hex[2],
               contrast = contrast,
               threshold = threshold,
               pass = pass)
  }
  
  do.call(rbind, combn(hexVector, 2, findContrast, simplify = F))
}

calcContrast(viridisLite::viridis(5))

