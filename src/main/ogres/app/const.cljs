(ns ogres.app.const)

(goog-define VERSION "latest")
(goog-define PATH "/release")
(goog-define SOCKET-URL "ws://localhost:5000/ws")
(goog-define THUMBNAIL-URL "http://localhost:5000/thumbnail")

(def ^:const grid-size
  "The length, in pixels, of a single square in the scene grid. This
   correlates to 5 feet in this spatial system."
  70)

(def ^:const half-size
  "Half the length, in pixels, of a single square in the scene grid."
  35)

(def ^:const hex-radius
  "The distance, in pixels, from the center of a hexagon in the scene grid
   to one of its vertices. Hexagons are pointy-top, matching the layout of
   real Gloomhaven/Frosthaven board tiles. Derived from `grid-size` so that
   a hex grid occupies the same normalized space as the square grid."
  grid-size)

(def ^:const hex-width
  "The width, in pixels, of a single pointy-top hexagon in the scene grid."
  (* (js/Math.sqrt 3) hex-radius))

(def ^:const hex-row
  "The vertical distance, in pixels, between the centers of two adjacent
   rows of hexagons in the scene grid."
  (* 1.5 hex-radius))

(def ^:const token-radius
  "The rendered radius, in pixels, of a default (Medium, 5ft) token.

   A hexagon's apothem (half of `hex-width`) naturally gives a token more
   breathing room from the grid edge than a square's half-width does at
   the same nominal radius -- that's just hexagon geometry, not a
   deliberate size difference. This value is chosen so a token's
   clearance-to-edge ratio on a square grid matches the ratio it already
   has on a hex grid, keeping the two grid types visually consistent
   without changing grid-size, snapping, or measurement math at all."
  (/ (* half-size (- half-size 2)) (/ hex-width 2)))
