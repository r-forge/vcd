hls <- function(H, L, S) {
  
  RGB <- function(q1, q2, hue) {
    if (hue > 360) hue <- hue - 360
    if (hue < 0) hue <- hue + 360
    if (hue < 60) 
      q1 + (q2 - q1) * hue / 60
    else if (hue < 180)
      q2
    else if (hue < 240) 
      q1 + (q2 - q1) * (240 - hue) / 60
    else q1
  }

  H <- H * 360
  
  p2 <- if (L <= 0.5)
    L * (1 + S)
  else
    L + S - (L * S)
  p1 <- 2 * L - p2;
  if (S == 0)
    R <- G <- B <- L
  else {
    R <- RGB(p1, p2, H + 120)
    G <- RGB(p1, p2, H)
    B <- RGB(p1, p2, H - 120)
  }
  rgb(R, G, B)
}
  
