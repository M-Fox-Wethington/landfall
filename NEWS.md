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
