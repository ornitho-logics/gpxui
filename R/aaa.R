

#' @import methods glue sf shiny leaflet data.table
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar sidebarMenu dashboardBody dashboardControlbar toast
#' @importFrom dplyr group_by summarise n
#' @importFrom fs dir_exists
#' @importFrom rlang .data
#' @importFrom RMariaDB MariaDB
#' @importFrom shinycssloaders withSpinner
#' @importFrom stringr str_detect str_extract
#' @importFrom stats na.omit weighted.mean
#' @importFrom xml2 read_xml xml_add_child xml_new_root xml_set_attr write_xml
NULL

utils::globalVariables(c(
  ":=", ".", ".SD", "datapath", "datetime_", "deltat", "dist", "ele", "gpx",
  "gps_id", "gps_point", "i", "last_entry_before_update", "max_dt",
  "max_ele", "mean_ele", "min_dt", "min_ele", "n", "name", "seg_id", "tab",
  "time", "track_seg_id", "track_seg_point_id"
))
