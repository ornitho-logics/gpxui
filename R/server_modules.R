#' Create the Base Leaflet Map
#'
#' @return A `leaflet` map object.
#' @export
basemap <- function() {
  leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.PositronNoLabels,
      group = "Print Map"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$OpenStreetMap,
      group = "Street Map"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Print Map", "Street Map", "Satellite"),
      overlayGroups = c("Tracks", "Points"),
      position = "topleft",
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::setView(sample(-150:150, 1), sample(-60:60, 1), zoom = 2)
}

#' Add GPX Layers to a Leaflet Map
#'
#' @param MAP A `leaflet` map or proxy object.
#' @param bbox Bounding box used to fit the map.
#' @param pts Optional `sf` waypoints.
#' @param trk Optional `sf` track lines.
#' @param zoom When `TRUE`, fit the map to `bbox` after drawing the GPX layers.
#'
#' @return A `leaflet` map or proxy object.
#' @export
gpxmap <- function(MAP, bbox, pts, trk, zoom = TRUE) {
  bbox <- .leaflet_bbox(bbox) |> unname()

  MAP <- MAP |>
    leaflet::clearShapes() |>
    leaflet::clearMarkers()

  if (!is.null(pts) && nrow(pts) > 0) {
    MAP <- MAP |>
      leaflet::addCircleMarkers(
        data = pts,
        group = "Points",
        fillOpacity = 0.5,
        opacity = 0.5,
        radius = 3,
        label = ~gps_point,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "auto",
          background = "transparent",
          offset = c(2, 0)
        )
      )
  }

  if (!is.null(trk) && nrow(trk) > 0) {
    MAP <- MAP |>
      leaflet::addPolylines(
        data = trk,
        group = "Tracks",
        color = "#da3503"
      )
  }

  if (isTRUE(zoom) && !is.null(bbox)) {
    MAP <- MAP |>
      leaflet::fitBounds(
        lng1 = bbox[1],
        lat1 = bbox[2],
        lng2 = bbox[3],
        lat2 = bbox[4]
      )
  }

  MAP
}
