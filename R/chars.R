# Characters outside ASCII, built at run time so the source files stay ASCII.
EN_DASH <- intToUtf8(0x2013)
MINUS   <- intToUtf8(0x2212)
DEGREE  <- intToUtf8(0x00B0)
SUP2    <- intToUtf8(0x00B2)
E_ACUTE <- intToUtf8(0x00E9)

# "64.838 S, 62.533 W" with degree signs.
fmt_latlon <- function(lon, lat) {
  sprintf("%.3f%s%s, %.3f%s%s", abs(lat), DEGREE, if (lat < 0) "S" else "N",
          abs(lon), DEGREE, if (lon < 0) "W" else "E")
}

# "1986 to 2022" as a dash span, for titles.
fmt_span <- function(a, b) paste0(a, EN_DASH, b)

# "1,234"
fmt_n <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# "7 October 2026"
fmt_date <- function(d) paste(as.integer(format(d, "%d")), format(d, "%B %Y"))

# A minus sign in place of a hyphen, for axis labels.
minus <- function(x) sub("-", MINUS, x, fixed = TRUE)
