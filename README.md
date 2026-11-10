# landfall

Site maps and colony records for Antarctic expeditions, in R.

`landfall` draws a relief map of the surroundings of any Antarctic site and
places a new penguin colony count against the site's record in MAPPPD, the
Mapping Application for Penguin Populations and Projected Dynamics. A site is
given by its four-letter MAPPPD code or by a name and coordinates, the map
covers a chosen radius, and one call renders a figure with the map, the
long-term record and how the new count compares with earlier seasons.

Every input lives in a local folder. `pack_sites()` fetches what a list of
sites needs before a trip, and after that nothing touches the network.

![A site report for Neko Harbor with an example count](man/figures/neko_report_2026.jpg)

The report above is `site_report("NEKO", count = 812, type = "nests", species = "GEPE",
date = "2026-12-14", accuracy = 2)` with an example count. The map alone, from
`site_map("NEKO", radius_km = 12)`:

<img src="man/figures/neko_map_12km.jpg" width="60%" alt="The map of 12 km around Neko Harbor">

## Installation

```r
# install.packages("remotes")
remotes::install_github("CCheCastaldo/mapppdr")
remotes::install_github("M-Fox-Wethington/landfall")
```

`mapppdr` carries the MAPPPD tables inside the install, so the colony records
work offline.

## Setting up

```r
library(landfall)
landfall_paths(
  cache   = "D:/landfall",            # REMA tiles and cached relief go here
  spatial = "D:/spatial_data",        # the ADD layers, in the layout below
  output  = "D:/landfall/figures"
)
```

The ADD layers are expected under `spatial` in this layout. Each can also be set
on its own.

| Setting | Default under `spatial` | Source |
|---|---|---|
| `coastline_hr` | `add_v7_12/`, `add_v7_11/` or `add_coastline_high_res/` | ADD high-resolution coastline polygons, UK Polar Data Centre, CC BY 4.0 |
| `coastline`, `iceshelves` | `Continent_ADD/Antarctica_Coastline_med_res_poly.shp`, `iceshelves/medium_res_IceShelfs.shp` | ADD medium-resolution layers, used when the high-resolution file is absent |
| `rock` | `add_v7_12/`, `add_v7_11/` or `add_rock_outcrop/` | ADD high-resolution rock outcrops |
| `contours` | `add_contours/add_contours_medium_res_v7.3.gpkg` | ADD contours v7.3, EGM2008 heights. The medium file is 500 m intervals; the high-resolution one is 100 m and is the one to set for a site map |
| `bathymetry` | `ibcso_v2/IBCSO_v2_bed.nc` | IBCSO v2 bed model, AWI, CC BY 4.0. Not an ADD layer |

The coastline and the rock outcrops are each named differently in every ADD
release, so their default is the first of a short list of candidates that is
on disk. Setting the path by hand pins a release. The sea floor is tinted
where `bathymetry` points at a bed model and drawn flat where it does not,
and a contour or rock layer that cannot be read is left out with a message
rather than stopping the map.

Keep these files outside a folder that syncs to cloud storage. A layer of
this size gets turned back into a placeholder that exists, reports its full
size and throws when anything reads it, and the map is then drawn without it.
Set such a layer on its own, as `rema_dir` already is:

```r
landfall_paths(contours = "G:/spatial/add_contours_high_res_v7.3.gpkg")
```

Elevation comes from the REMA v2.0 32 m tiles of the Polar Geospatial Center,
which `pack_sites()` fetches into `rema_dir` as a site needs them. Each tile
covers a 100 km square and is 15 to 40 MB on disk, but it arrives inside an
archive of a few hundred megabytes, so budget for the download and not for what
it leaves behind. The tile grid is arithmetic and the mosaic is not: an index
that falls on open sea has no archive, and `pack_sites()` reports that
separately from a transfer that failed.

## Before a trip

```r
pack_sites(c("NEKO", "CUVE", "DANC", "PETE", "HALF"), radius_km = 25)
```

This fetches the missing REMA tiles, checks that the coastline and the other
layers open, and builds and caches each site's relief. `check_offline()` lists
what each site needs and whether it can be read, without fetching anything. It
opens each layer rather than testing that a file is there, because a layer left
as a placeholder by cloud storage is there and still cannot be drawn.

## On the trip

```r
site_report("NEKO", count = 812, type = "nests", species = "GEPE",
            date = "2026-12-14", accuracy = 2, source = "ground count")
```

The figure has the relief map of 25 km around Neko Harbor on the left, with the
other MAPPPD sites in the window marked, and on the right the gentoo nest counts
of every earlier season from MAPPPD with the new count on them in orange, the
range of its accuracy class as a bar. Under the panel, the comparison is
written out: how many earlier seasons had more nests and how many fewer, the
change from the highest recorded count of the most recent earlier season and
how long ago that was, and whether the new count falls within the range of the
ten most recent seasons that hold one. The pieces are available on their own:

```r
site_map("NEKO", radius_km = 15)               # the map alone, at any radius
site_map(site_point("Stonington Island", lon = -67.0, lat = -68.18), radius_km = 20)
colony_record("NEKO")                          # every MAPPPD count at the site
count_context(colony_record("NEKO"), new_count(812, "nests", "GEPE", season = 2026))
```

A site given by name and coordinates is a point of your own and has no MAPPPD
identity, so no count record is read for it and the comparison says so. Its
`code` is a label for titles and file names, and a name whose first four
letters match a real site does not pick up that site's counts. Say which
MAPPPD site a point stands for when that is what you mean:

```r
site_point("Neko Harbor beach", lon = -62.53, lat = -64.84, mapppd_id = "NEKO")
```

## How the comparison is made

MAPPPD records nests, chicks and adults as separate count types, and a season
often holds several counts of one type from different dates and observers. A
new count is therefore compared only with counts of the same species and the
same type, and each season is represented by its highest recorded count. That
is the highest count anybody wrote down, so it is not selected by breeding
stage, and matching the species and the type is all the comparison controls
for. Two seasons may also differ in when in the cycle they were counted, how
much of the colony each visit covered, and what method was used.

A season's peak is a single observation, so the accuracy class, date, vantage
and citation reported beside it are that same observation's and are never
assembled from different counts.

MAPPPD's accuracy field holds the five-point precision scale the counter
assigned in the field (Croxall and Kirkwood 1979), and not an uncertainty
worked out from the count, so a range built from it is not a confidence
interval and says nothing about where the true population lies. The published
bands are within 5% for class 1, 10% for class 2, 10 to 15% for class 3, 25 to
50% for class 4, and the nearest order of magnitude for class 5 (Strycker et
al. 2020, Table 2). `accuracy_bounds()` takes the wider end of each band, and
reads class 5 as a factor of the square root of ten either side, which spans
exactly one order of magnitude. A count with no class recorded gets no range
and no bar, and `accuracy` has no default, because an omitted class is unknown
and not the most precise one.

A season is the austral summer named by the year in which it begins, so a count
made in January 2027 belongs to season 2026.

## Style

The maps follow the BRI neighborhood maps: REMA elevation under a
four-direction Imhof hillshade, a hypsometric ice ramp, the sea floor tinted
from IBCSO, the ADD coastline, ice shelves, contours and rock outcrops, and a
parchment page. The charts use the BRI figure style. The north arrow is a glyph
inside a ring: `blade` by default, a five-cornered sail drawn with straight edges,
with `site_map(arrow = "sastrugi")` for the curved wind-carved ridge and
`arrow = "dart"` for a plain arrowhead.

Both ramps are stretched to each window's own range, the land from the sea to
the 99.5th percentile of the land inside the drawn frame and the sea floor from
the surface to the 99.5th percentile of the depth. A color therefore means a
height, or a depth, within one map and not between two, which is why the caption
carries both numbers.

Three of the four faults found in the printed wall sheets during November 2026
were present here too, and all three are fixed. [NOTES.md](NOTES.md) sets out
each one and what was done about it.
