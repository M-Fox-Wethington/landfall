# What the wall-map work found

`landfall` shares its stack with the Antarctic Program's printed map series:
REMA elevation under a four-direction hillshade, the ADD layers, `ggrepel` for
the labels and `ggspatial` for the scale bar and the north arrow. Four faults
were found in that series during November 2026 and three of them were present
in this package. All three are fixed here. Each is written out below with what
was done about it. The fixes themselves are in [NEWS.md](NEWS.md).

## REMA measures from the ellipsoid, not from the sea

REMA v2.0 heights are above the WGS84 ellipsoid, which on the Antarctic
Peninsula stands well below the sea surface. Measured from four windows' own
water, cell by cell off the drawn coastline, the separation is 15.05 m at the
Lemaire Channel, 18.24 m in the Gerlache Strait, 22.14 m in the Antarctic Sound
and 22.55 m at the Danger Islands, rising steadily to the northeast. Each figure
rests on tens of millions of cells and in each window most of them sit within a
meter of the median, so these are readings of the sea surface and not of noise.

`HYPSO_BREAKS` starts at 0 m and `terrain.R` feeds it REMA's own values, so on
any Peninsula site the ramp's first step begins at the sea surface plus that
separation. Every beach and every low landing is tinted as though it stood 15 to
22 m higher than it does, which on the low gentoo sites this package is built
for is the whole of the ground a party walks on. The same offset means the cut
at zero in `terrain.R` never fires at the shore: a cell REMA reads below the sea
surface still reads above zero on the ellipsoid, so it is drawn as the palest
ice instead of being dropped.

The repair used in the map series is to measure the separation from the window's
own water, take it off the merged model, cut the model to the drawn land with a
collar so the hillshade keeps its neighbors at every shore, and floor it just
above zero so that land REMA cannot height sits at the bottom of the ramp rather
than vanishing. The measurement is cheap, because the window's own sea is
already in the grid.

**Fixed.** `build_site_terrain()` measures the sea surface from the window's own
water, takes it off the model, shades a copy cut to a 60 m collar beyond the
drawn land, and floors the land at 0.25 m. At Neko Harbor the separation
measures 16.83 m, between the 15.05 m of the Lemaire Channel and the 18.24 m of
the Gerlache Strait, which is where Andvord Bay should sit. The same pass
separates two masks that had been one: water takes the ocean color, and land
the coastline puts above the sea for which REMA holds no height takes a flat
land tint and is counted and reported. At Neko Harbor that is 2,921 cells of
439,558, which had been drawn as open water on ground.

## The relief cache is keyed on less than it is built from

`window_stem()` names the cached relief from the site code, the radius and the
central meridian. The grid resolution, the exaggeration, the shadow floor, the
hypsometric ramp and the version of the coastline the model is masked to are all
absent from that name, so changing any of them returns the image built before
the change with no warning. The map series hit this four times in a week, on the
coastline version, the contour and sea-floor resolutions, the REMA tile set and
the elevation datum, and settled on writing a signature of every setting beside
the cached raster and rebuilding on a mismatch.

**Fixed.** `terrain_signature()` writes the window's own coordinates, the
projection, the grid, the exaggeration, the shadow floor, the collar, the
elevation source and the coastline file with their sizes and timestamps, and
the ramp and its breaks, into a file beside the raster. The cache is reused
only on an exact match. This also closes a worse case than a stale style: two
different places can share a cache file name, because the name carries only the
site code, the radius and the meridian, and a point of your own labelled NEKO
at the same radius writes where Neko Harbor wrote. `clear_terrain_cache()`
removes them by site or all at once.

## The scale bar and the north arrow are invisible to the labels

`map.R` draws site names through `ggrepel` and the bar and the arrow through
`ggspatial`, which places them from the panel's own corners. The repel pass
cannot see either, so a site name can be laid straight across one. On the
Lemaire Channel sheet the bar cut a name in half and it went unnoticed for days
because an inset happened to be covering both. The repair is to hand the bar's
and the arrow's footprints back to the layout as a grid of points carrying no
label, spaced closer than the shortest label box is tall, because a name is
pushed only by a point that falls inside its own box.

**Fixed.** The bar's and the arrow's footprints were measured by drawing the
map twice, once with the two annotations and once without, and reading the
bounding box of every pixel that changed. Each rectangle, plus a margin, is now
laid down as a lattice of points that carry no label and so draw nothing, and
`check_map_labels()` reads every label back out of the finished drawing and
reports any that still meets another or sits on one of those points.

Turning a corner measured in centimeters into ground meters needs the drawn
panel's size, which is the figure less its margins, its title, its caption and
its edge labels. `map_panel_inches()` measures it by drawing a plot that
carries the same frame and text but no data layers; the skeleton and the full
map gave the same panel to five decimal places at three figure sizes.

Two things had to change before any of it moved a name. ggrepel pushes a label
away from a point only within `point.padding` of it, and the default is a
millionth of a line, so the first barrier was laid down correctly and did
nothing. And `max.overlaps` counts every barrier point, so a ceiling there
drops the very names the barrier is meant to move. On a test map with a place
name planted in the middle of each corner, both names sat on the furniture
before the repair and neither did after.

## Measure the picture, not the settings

The fourth fault does not arise here, because this package prints no ratio
scale, but the habit it taught is worth keeping. A setting that says what a
figure should be is not evidence of what it is: a terrain raster can be enlarged
to fill a frame, a scale bar can be drawn and then covered, and a label layout
can stop on a clock with its labels still moving. Each of those shipped in the
map series and each was found only by reading the exported image back and
measuring it. The checks that caught them are cheap: the share of horizontally
adjacent pixels that are identical tells you whether a raster was enlarged, the
longest unbroken run across a scale bar's own band tells you whether something
is drawn over it, and redrawing the label layer on its own under the same seed
lets every label box be read back and compared.
