
library(raadfiles)
library(basf)
fline <- raadfiles::thelist_files(pattern = "hydline")$fullname

fun <- function(.x) {
  layer <- vapour::vapour_layer_names(.x)[1]
  att <- vapour::vapour_read_attributes(.x, sql = glue::glue("SELECT NAME FROM \"{layer}\""))
  tibble::tibble(fullname = .x, NAME = att$NAME)
}
att <- purrr::map_dfr(fline, fun)
att %>% dplyr::filter(stringr::str_detect(NAME, "Esk")) %>% dplyr::distinct(NAME)
# # A tibble: 2 x 1
# NAME
# <chr>
#   1 South Esk River
# 2 North Esk River

(esk <- att %>% dplyr::filter(stringr::str_detect(NAME, "Esk")) %>%
  dplyr::filter(NAME == "South Esk River") %>%
  dplyr::distinct(NAME, fullname))
# # A tibble: 6 x 2
# file                               NAME
# <chr>                              <chr>
#   1 list_hydline_break_o_day.gdb       South Esk River
# 2 list_hydline_dorset.gdb            South Esk River
# 3 list_hydline_launceston.gdb        South Esk River
# 4 list_hydline_meander_valley.gdb    South Esk River
# 5 list_hydline_northern_midlands.gdb South Esk River
# 6 list_hydline_west_tamar.gdb        South Esk River
#

l <- purrr::map(esk$fullname, function(x) {
  layer <- vapour::vapour_layer_names(x)[1]
  # Bingo, just the Major River line
  vapour::vapour_read_geometry(x, sql = glue::glue("SELECT * FROM \"{layer}\" WHERE NAME = 'South Esk River' AND HYD_CLASS = 'Major River'"))
})


south_esk <- do.call(c, lapply(l, sf::st_as_sfc))
#plot(south_esk, col = hcl.colors(length(south_esk)))
#plot(st_cast(st_line_merge( st_union(south_esk)), "LINESTRING"), col = hcl.colors(5))
xx <- st_cast(st_line_merge( st_union(south_esk)), "LINESTRING")
len <- unclass(st_length(xx))
xx <- xx[order(len, decreasing = TRUE)[1:2]]
xx <- st_cast(st_line_merge(st_union(xx)), "LINESTRING")
south_esk <- sf::st_set_crs(xx, sf::st_crs(read_sf(esk$fullname[1])))

## can't merge em so leave it for now
#plot(st_cast(st_line_merge(st_union(xx)), "LINESTRING"), col = hcl.colors(2))
usethis::use_data(south_esk, overwrite = TRUE)

