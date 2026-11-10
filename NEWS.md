# landfall (development version)

A correctness pass over the count record, the site identity, the relief cache
and the file names, following an independent review of 0.1.0. Five of these
change what existing calls do, and each is listed as such.

## Counts and precision

* **Changed.** `accuracy` no longer defaults to class 1 in `new_count()` and
  `site_report()`. Omitting it now means the class was not recorded, which
  yields no range and no error bar. The old default gave an unqualified count
  the most precise class on the scale.
* **Changed.** `accuracy_bounds()` reads class 4 as 50% and class 5 as the
  nearest order of magnitude, a factor of the square root of ten either side.
  It had read them as 25% and 50%, which took the floor of the class 4 band
  where classes 1 to 3 take the ceiling, and turned class 5 into a percentage
  it does not have. The published bands are in Strycker et al. (2020, Table 2)
  and the field is the observer's own precision note (Croxall and Kirkwood
  1979), so the documentation no longer calls a derived range a statement about
  the true population.
* `season_peaks()` selects the observation that supplies a season's maximum and
  returns that observation's accuracy class, date, vantage and citation. It had
  taken the highest count and the best class independently, so a season holding
  1,000 nests at class 1 and 1,100 at class 2 reported 1,100 at class 1 and a
  range a third too narrow. Ties resolve by the better class and then by the
  earlier row.
* A change from a season whose count was zero reports the difference and says
  the percentage is undefined. It had divided by zero. MAPPPD holds 125 season
  peaks of zero across 63 sites, including the most recent gentoo nest count at
  Dundee Island.
* `new_count()` rejects a non-finite count and refuses a date and a season that
  disagree.
* The comparison no longer describes a count as the first in MAPPPD, since
  later seasons may hold counts and nothing here is submitted to MAPPPD, and no
  longer describes a season's maximum as the count nearest the peak of
  incubation, which is not how it is chosen.

## Sites

* **Changed.** A site now carries `mapppd_id` beside `code`, and count records
  are read by `mapppd_id` alone. `site_point()` leaves it empty, so a point of
  your own has no count history. A point named "Neko Ridge" took the default
  code `NEKO` and inherited Neko Harbor's record. Pass `mapppd_id` to say which
  MAPPPD site a point stands for.
* `sites_within()` drops the focal site by its MAPPPD identifier, so a point of
  your own no longer removes the real site its label resembles.

## Relief

* The sea surface is measured from each window's own water and taken off the
  model. REMA reads from the WGS84 ellipsoid, which stands 15 to 23 m below the
  sea around the Peninsula, so every beach had been tinted that much too high
  and the cut at zero never fired at the shore.
* Land for which REMA holds no height is drawn as a flat land tint and the
  share of it is reported. It had been painted as ocean, which invented open
  water on ground the coastline puts above it.
* The hillshade is computed on a copy cut to a collar beyond the drawn land, so
  slopes still have neighbors at the shore, and land below the corrected sea
  surface sits at the bottom of the ramp instead of vanishing.
* The last color of the hypsometric ramp can now be drawn. `findInterval()` was
  called with `all.inside = TRUE` against eight breaks and eight colors, which
  confined the index to seven.
* **Changed.** The cached relief is keyed on a signature of the window's
  coordinates, the projection, the grid, the shading, the elevation and
  coastline files and the ramp, and rebuilds when any of them differs. Caches
  from 0.1.0 have no signature and rebuild once. The file name alone could
  return an image built at another resolution, from another coastline, or for
  another place that shares the name.
* `clear_terrain_cache()` removes cached relief by site or all at once, and
  `build_site_terrain(rebuild = TRUE)` forces one rebuild.

## The label layout

* The scale bar and the north arrow are handed to the label layout as points
  that carry no label, so a name is no longer drawn across either. ggspatial
  places both from the panel's own corners and ggrepel lays out names in the
  map's coordinates, and neither knew about the other. The footprints were
  measured by drawing the map twice, once with the annotations and once
  without, and reading the bounding box of every pixel that changed.
* `map_panel_inches()` measures the drawn panel, which is what turns a corner
  given in centimeters into ground meters. `site_map()` and `site_report()`
  each measure their own, because the map fills the figure in one and sits in
  a slot of it in the other.
* **Changed.** Place names and the names of the other MAPPPD sites are laid
  out in one pass. A ggrepel layer cannot see the labels of any other, so as
  two layers the sets had nothing stopping them landing on each other. The one
  visible consequence is that a place name's leader is now the same faint
  slate as a site name's.
* **Changed.** The names are repelled with `point.padding` of 0.06 inches, a
  force of 4 and a pull of 0.8, where all three were ggrepel's defaults. The
  default padding is a millionth of a line, which is small enough that a
  barrier point moves nothing at all, and the eased pull lets a name whose own
  position falls under the bar leave it and draw a leader back.
* The site and neighbor markers are barriers too, as filled discs rather than
  rings, because a label well inside a ring touches nothing.
* `check_map_labels()` draws a map on a throwaway device and reads every label
  back out of it, reporting boxes that meet and labels sitting on a barrier
  point. The second is the only way to catch a name on the bar, the arrow or a
  marker, because none of those reaches the layout as a box of its own.
* `site_map()` gained `overwrite`, to match `site_report()`.

## What the map looks like

* **Changed.** The hypsometric ramp is stretched to each window's own relief,
  from the sea to the 99.5th percentile of the land inside the drawn frame,
  and the caption names that top. Held to the old fixed scale running to
  2400 m, a 25 km window round a landing site was drawn in two of the eight
  colors and read as flat. A color now means a height within one map and not
  between two, which is why the caption carries the number. The pad is left
  out of the percentile, because ground in the pad is never drawn.
* **Changed.** The ramp runs continuously between its eight anchors, mixed in
  Lab. Eight flat steps over a kilometer of relief printed as hard-edged bands
  that read as contours nobody drew.
* **Changed.** The scale bar is a filled alternating bar rather than tick
  marks, which sit at about 1.6 to 1 against the ocean and disappear into the
  relief. This is the treatment the Antarctic Sound sheet uses.
* **Changed.** The north arrow is a glyph inside a ring with the letter above
  it, on a backing in the paper color, in place of ggspatial's bare line and
  triangle. `north_arrow_circle()` takes ggspatial's own style arguments, so
  it can be used in any map, and `site_map()` and `site_report()` take the
  glyph by name through `arrow`.
* **Changed.** The glyph is `"blade"` by default, a five-cornered sail with a
  hollow luff and a broken leech, drawn with straight edges, so site maps now
  draw that where they drew the plain arrowhead. `arrow = "dart"` restores the
  old look and `arrow = "sastrugi"` gives the curved alternative, a
  wind-carved snow ridge with a sharp crest over a long leeward drift. Each
  glyph is scaled so its furthest point from the ring's center reaches the
  same radius, rather than to its own height, so all three sit inside the ring
  with identical clearance.
* **Removed.** The swept delta is gone and asking for it by name is an error
  rather than a silent substitution. It was the Star Trek Starfleet insignia,
  a registered Paramount trademark, and it was retired for that reason and not
  on taste. Every variant tried on the way to the replacements is in the
  design-library repository under `cartography/symbols/north-arrows`, that one
  included, as SVG with its own measurements.
* **Changed.** The sea floor is tinted by depth from the IBCSO v2 bed model
  when `landfall_paths()$bathymetry` points at one, and keeps the flat ocean
  color when it does not, so a window with no soundings in it looks like one.
  The ramp is stretched to the window the way the land ramp is, to the 99.5th
  percentile of the depth inside the drawn frame, and the square root of the
  depth puts more of it in the shallows, which is the water a landing party
  is in. The caption names the depth it runs to. IBCSO is a 500 m grid, about
  a tenth of an inch on a 25 km map, so it is cropped in its own coordinates
  and resampled onto the relief's grid; the crop's margin is counted in the
  bed model's own cells, because a margin shorter than one cell leaves a rim
  of nothing around the window.
* `site_map()` wraps its caption. It never did, and the caption had grown long
  enough to run off the page.
* **Changed.** Every leader is drawn before the marker it points at, on the
  map and in the chart, so a line passes under its own circle instead of
  across it and the marker's white halo cuts it off at the circle's edge.
  The names are unaffected: the markers are barrier points in the layout, so
  drawing them last cannot cover one, and `check_map_labels()` reports any
  name that lands on one.
* **Changed.** The new count's accuracy range is drawn over the marker's
  white halo and under its circle, so it reads as one line passing behind
  the marker. Under the halo as well, the halo erased it either side of the
  circle and all that showed was two dashes with nothing joining them. The
  range is a quantity and not a leader, which is why it is the one line on
  these figures that is not drawn under everything.
* The two annotations are built in `map_furniture()`, so the rectangles
  reserved for them and the things actually drawn cannot drift apart.

## Returned objects

* **Changed.** `site_map()` and `site_report()` return the map layers in an
  attribute called `map_layers`, not `layers`. ggplot2 4.0 keeps a plot's own
  properties in its attributes, so an attribute called `layers` replaced the
  plot's layers and the returned figure could not be drawn or added to. The
  files written were never affected, because they are written first.

## Files and sources

* **Changed.** A report's default file name carries the site, the species, the
  count type and the date, so two observations at one site in one season no
  longer write to the same files. `save_figure()` refuses to replace an
  existing file unless `overwrite = TRUE`, and `site_report()` returns the
  paths it wrote in the `files` attribute.
* The caption names the Antarctic Penguin Biogeography Project as the source of
  the counts and the MAPPPD application separately, reports the installed
  `mapppdr` version whole, states what the heights are measured from and gives
  the contour interval. It had cut the version to two parts and cited only the
  application.
* `load_coast()` reads the coastline version from the file that was loaded. It
  had written v7.10 into every caption whatever was on disk.
* **Changed.** `landfall_paths()` clears a setting when it is passed `NA` and
  clears every setting with `reset = TRUE`. A `NULL` could not be told from an
  omitted argument, so a setting could only be replaced.
* A vector layer is filtered in its own coordinate system. The filter had been
  written in EPSG:3031 whatever the file held.
* **Changed.** The ADD layer defaults are the first of a short list of
  candidate names that is on disk, so the high-resolution coastline and the
  rock outcrops follow a v7.11 or v7.12 release where one is there. The ADD
  renames these files at every release and a default pinned to one of them
  goes quietly missing. Setting the path by hand still pins a release.
* The contour and rock layers are left out with a message when the file is
  there but cannot be read, rather than stopping the map. A file can report
  its full size and still throw when anything asks for its bytes, which is
  what a layer looks like once cloud storage has turned it back into a
  placeholder.
* `landfall_paths()` gained `bathymetry`, which defaults to
  `ibcso_v2/IBCSO_v2_bed.nc` under `spatial`.
* **Changed.** `check_offline()` reports whether each layer can be opened,
  not whether a file sits at the path, and covers the bed model as well. The
  two answers differ on exactly the layer that matters: a placeholder left by
  cloud storage exists, reports its full size and throws when anything reads
  it, so the check said a layer was packed for a trip that could not be drawn.
  Only each layer's header is read, whatever the file's size. `pack_sites()`
  raises the same test on the coastline and says which of the optional layers
  cannot be read.
* `rema_fetch_tiles()` checks whether the mosaic serves a tile before
  downloading, returns a `status` for each one, and checks that the elevation
  file was saved. It had marked a tile fetched without reading the result of
  the copy, and treated a tile the mosaic does not carry as a failed transfer.

# landfall 0.1.0

First release.

* `site_report()` renders, for a four-letter MAPPPD site code and a new
  colony count, a figure with the relief map of the site's surroundings, the
  long-term record of counts at the site, and how the new count compares with
  earlier seasons.
* `site_map()` draws the relief map alone, for any MAPPPD site or any named
  point, at any radius.
* `pack_sites()` fetches and caches every input a list of sites needs, so the
  figures render offline afterwards.
