#  hcl - A  hue/chroma/luminance color generator
#
#  This function generates and R color specification given
#  hue, chroma and luminance values.  You use this exactly
#  the same way as you would use HSV.
#
#  For area fills I would recommend using the default values
#  of 35 for chroma and 85 for luminance.  If you want stronger
#  colors try a chroma of 55 and a luminance of 75.
#
#  These values define a point in CIELUV using polar coordinates.
#
#    hue       - The angle to the color in degrees. 
#                Roughly speaking red=0, 120=green, 240=blue
#
#    chroma    - The ``colorfulness'' of the color (rather like
#                saturation).  The maxiumum chroma possible
#                varies with hue and luminance.  Large values of
#                chroma which are too large for the given hue
#                and luminance result in "out of gammut" errors.
#
#    luminance - The ``brightness of the color'' relative to white.
#                (White is assumed to have luminance 100.)
#
#    correct   - If true, out of gammut colors are ``corrected''
#                by adjusting in RGB space.  If false, an "NA" is
#                returned for out of gammut colors.
#
#    gamma     - The device gamma.  PC CRTs have a gamma of about
#                2.2, Macs about 1.5 (from memory) and SGI displays
#                have a gamma of 1.

hcl =
  function(hue, chroma = 35, luminance = 85, correct = FALSE, gamma = 2.2)
  {
    HEXDIGITS = c("0", "1", "2", "3", "4", "5", "6", "7",
                  "8", "9", "A", "B", "C", "D", "E", "F")
    gtrans =
      function(u, gamma = 2.4)
      ifelse(u > 0.00304, 1.055 * u^(1 / gamma) - 0.055, 12.92 * u)

    #  Assume a D65 whitepoint with luminance 100.
    #  Ultimately, this should be a parameter.
    #  These are the CIE XYZ values.

    XN =  95.047
    YN = 100.000
    ZN = 108.883

    #  uN and vN are the corresponding LUV chromaticities

    tmp = XN + YN + ZN
    xN = XN / tmp
    yN = YN / tmp
    uN = 2 * xN /(6 * yN - xN + 1.5)
    vN = 4.5 * yN / (6 * yN - xN + 1.5)

    #  Convert from polar coordinates to u and v.
    #  Hue is take to be in degrees and needs to be converted.

    U = chroma * cos(.01745329251994329576 * hue)
    V = chroma * sin(.01745329251994329576 * hue)

    # Convert from L*u*v* to CIE-XYZ

    Y = YN * ifelse(luminance > 7.999592,
                    ((luminance + 16)/116)^3,
                    luminance/903.3)
    u = U / (13 * luminance) + uN
    v = V / (13 * luminance) + vN
    X = 9.0 * Y * u / (4 * v)
    Z = - X / 3 - 5 * Y + 3 * Y / v

    #  Map to ``gamma dependent'' RGB

    RGB = round(255 * cbind(
      gtrans(( 3.240479 * X - 1.537150 * Y - 0.498535 * Z) / YN, gamma),
      gtrans((-0.969256 * X + 1.875992 * Y + 0.041556 * Z) / YN, gamma),
      gtrans(( 0.055648 * X - 0.204043 * Y + 1.057311 * Z) / YN, gamma)))

    #  Convert to hexadecimal strings

    if (any(RGB < 0 | RGB > 255, na.rm=TRUE)) {
      warning("out of gammut RGB values")
      if (correct)
        RGB = pmax(pmin(RGB,  255), 0)
      else
        RGB[RGB < 0 | RGB > 255] = NA
    }
    r = RGB[,1]
    g = RGB[,2]
    b = RGB[,3]
    ifelse(is.na(r + g + b), "NA",
           paste("#",
                 HEXDIGITS[(r %/% 16) %% 16 + 1],
                 HEXDIGITS[r %% 16 + 1],
                 HEXDIGITS[(g %/% 16) %% 16 + 1],
                 HEXDIGITS[g %% 16 + 1],
                 HEXDIGITS[(b %/% 16) %% 16 + 1],
                 HEXDIGITS[b %% 16 + 1],
                 sep = ""))
  }

