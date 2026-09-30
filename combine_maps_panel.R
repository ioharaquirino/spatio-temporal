# ============================================================
# COMBINE 4 TIFF MAPS INTO A COMPACT 2 x 2 FIGURE
#
# Left column:
#   keeps Latitude
#   removes excess RIGHT margin
#
# Right column:
#   removes repeated Latitude labels/ticks
#   removes excess LEFT margin
#
# Original TIFFs are NOT modified.
# No resizing is performed.
# ============================================================


# ============================================================
# PACKAGE
# ============================================================

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}

library(magick)


# ============================================================
# FOLDER
# ============================================================

folder <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "novas_KDE/grid_proxy_paired_autumn_winter/",
  "mapas_pontos_maiorque1"
)


# ============================================================
# INPUT FILES
#
# Final arrangement:
#
#   1 | 2
#   -----
#   3 | 4
#
# ============================================================

file1 <- file.path(
  folder,
  "GRID_TEST2_PAIRED_wide_A1999_W2000_cell5km_allcells_20260921_132824.tiff"
)

file2 <- file.path(
  folder,
  "GRID_TEST2_PAIRED_wide_A2009_W2010_cell5km_allcells_20260921_132824.tiff"
)

file3 <- file.path(
  folder,
  "GRID_TEST2_PAIRED_wide_A2016_W2017_cell5km_allcells_20260921_132824.tiff"
)

file4 <- file.path(
  folder,
  "GRID_TEST2_PAIRED_wide_A2022_W2023_cell5km_allcells_20260921_132824.tiff"
)


# ============================================================
# CHECK THAT ALL FILES EXIST
# ============================================================

files <- c(file1, file2, file3, file4)

if (!all(file.exists(files))) {
  
  missing_files <- files[!file.exists(files)]
  
  stop(
    paste(
      "The following file(s) could not be found:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    )
  )
}

message("✓ All four TIFF files were found.")


# ============================================================
# READ TIFF FILES
# ============================================================

img1 <- image_read(file1)
img2 <- image_read(file2)
img3 <- image_read(file3)
img4 <- image_read(file4)


# ============================================================
# CHECK ORIGINAL DIMENSIONS
# ============================================================

info1 <- image_info(img1)
info2 <- image_info(img2)
info3 <- image_info(img3)
info4 <- image_info(img4)

dimensions <- data.frame(
  Figure = c(
    "1999 Autumn + 2000 Winter",
    "2009 Autumn + 2010 Winter",
    "2016 Autumn + 2017 Winter",
    "2022 Autumn + 2023 Winter"
  ),
  Width_px = c(
    info1$width,
    info2$width,
    info3$width,
    info4$width
  ),
  Height_px = c(
    info1$height,
    info2$height,
    info3$height,
    info4$height
  )
)

message("\n===== ORIGINAL IMAGE DIMENSIONS =====")
print(dimensions, row.names = FALSE)


# ============================================================
# CHECK WHETHER DIMENSIONS MATCH
# ============================================================

same_width <- length(unique(dimensions$Width_px)) == 1
same_height <- length(unique(dimensions$Height_px)) == 1

if (!same_width || !same_height) {
  
  stop(
    paste0(
      "The four TIFFs do not have identical dimensions.\n",
      "The script stopped BEFORE modifying anything."
    )
  )
}

message("\n✓ All four maps have identical dimensions.")
message("✓ No resizing will be performed.")


# ============================================================
# ORIGINAL DIMENSIONS
# ============================================================

W <- dimensions$Width_px[1]
H <- dimensions$Height_px[1]

message(
  "\nOriginal dimensions: ",
  W, " x ", H, " px"
)


# ============================================================
# CROPPING SETTINGS
# ============================================================
#
# LEFT COLUMN:
# Maps 1 and 3 keep their Latitude axis.
# Only excess space on the RIGHT is removed.
#
# RIGHT COLUMN:
# Maps 2 and 4 lose the repeated Latitude axis
# by cropping the LEFT side.
#
# Updated slightly stronger crop:
# ============================================================

crop_right_leftcolumn <- 220
crop_left_rightcolumn <- 380


# ============================================================
# CROP MAPS 1 AND 3
#
# Keep Latitude.
# Remove 220 px from RIGHT.
# ============================================================

img1_crop <- image_crop(
  img1,
  geometry = geometry_area(
    width  = W - crop_right_leftcolumn,
    height = H,
    x_off  = 0,
    y_off  = 0
  ),
  repage = TRUE
)

img3_crop <- image_crop(
  img3,
  geometry = geometry_area(
    width  = W - crop_right_leftcolumn,
    height = H,
    x_off  = 0,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# CROP MAPS 2 AND 4
#
# Remove 380 px from LEFT.
# This removes the repeated Latitude axis.
# ============================================================

img2_crop <- image_crop(
  img2,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)

img4_crop <- image_crop(
  img4,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# SMALL CONTROLLED GAP BETWEEN COLUMNS
# ============================================================

gap_px <- 20

gap <- image_blank(
  width = gap_px,
  height = H,
  color = "white"
)


# ============================================================
# CREATE TOP ROW
#
# 1999/2000 | 2009/2010
# ============================================================

row_top <- image_append(
  c(
    img1_crop,
    gap,
    img2_crop
  ),
  stack = FALSE
)


# ============================================================
# CREATE BOTTOM ROW
#
# 2016/2017 | 2022/2023
# ============================================================

row_bottom <- image_append(
  c(
    img3_crop,
    gap,
    img4_crop
  ),
  stack = FALSE
)


# ============================================================
# COMBINE BOTH ROWS
# ============================================================

figure_final_compact <- image_append(
  c(
    row_top,
    row_bottom
  ),
  stack = TRUE
)


# ============================================================
# CHECK FINAL DIMENSIONS
# ============================================================

final_info <- image_info(figure_final_compact)

message("\n===== FINAL FIGURE =====")

message(
  "Dimensions: ",
  final_info$width,
  " x ",
  final_info$height,
  " px"
)


# ============================================================
# OUTPUT FILE
# ============================================================

output_file <- file.path(
  folder,
  "GRID_PAIRED_4PANELS_COMPACT_V2_1999_2009_2016_2022.tiff"
)


# ============================================================
# SAVE FINAL TIFF
# ============================================================

image_write(
  figure_final_compact,
  path = output_file,
  format = "tiff",
  density = "300x300",
  compression = "lzw"
)


# ============================================================
# CONFIRM OUTPUT
# ============================================================

message("\n✓ FINAL COMPACT 2 x 2 FIGURE SAVED:")
message(output_file)

message("\n✓ Original TIFFs were NOT modified.")
message("✓ No resizing was performed.")
message("✓ Latitude retained on left-hand panels.")
message("✓ Repeated Latitude removed from right-hand panels.")
message("✓ Central whitespace further reduced.")
message("✓ Output written at 300 dpi with LZW compression.")


# ============================================================
# FINAL CHECK
# ============================================================

if (file.exists(output_file)) {
  
  size_mb <- file.info(output_file)$size / 1024^2
  
  message(
    "\n✓ File successfully created (",
    round(size_mb, 2),
    " MB)."
  )
  
} else {
  
  warning("The output file could not be found after saving.")
  
}

#
#
#

# Outra figura #

# ============================================================
# AUTUMN OVERLAP MAPS
# COMBINE 4 TIFF MAPS INTO A COMPACT 2 x 2 FIGURE
#
# Arrangement:
#
#   2020 | 2021
#   -----------
#   2022 | 2023
#
# Maps 1 and 3:
#   COMPLETELY preserved
#   Latitude retained
#   NO crop on the right
#
# Maps 2 and 4:
#   LEFT side cropped to remove repeated Latitude
#
# Original TIFFs are NOT modified.
# No resizing is performed.
# ============================================================


# ============================================================
# PACKAGE
# ============================================================

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}

library(magick)


# ============================================================
# FOLDER
# ============================================================

folder <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "grid_overlap_5km_plotter_ALL_cells/autumn"
)


# ============================================================
# INPUT FILES
#
# Final arrangement:
#
#   1 | 2
#   -----
#   3 | 4
#
# ============================================================

file1 <- file.path(
  folder,
  "GRID_overlap_autumn_2020_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file2 <- file.path(
  folder,
  "GRID_overlap_autumn_2021_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file3 <- file.path(
  folder,
  "GRID_overlap_autumn_2022_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file4 <- file.path(
  folder,
  "GRID_overlap_autumn_2023_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)


# ============================================================
# CHECK THAT ALL FILES EXIST
# ============================================================

files <- c(file1, file2, file3, file4)

if (!all(file.exists(files))) {
  
  missing_files <- files[!file.exists(files)]
  
  stop(
    paste(
      "The following file(s) could not be found:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    )
  )
}

message("✓ All four TIFF files were found.")


# ============================================================
# READ TIFF FILES
# ============================================================

img1 <- image_read(file1)
img2 <- image_read(file2)
img3 <- image_read(file3)
img4 <- image_read(file4)


# ============================================================
# CHECK ORIGINAL DIMENSIONS
# ============================================================

info1 <- image_info(img1)
info2 <- image_info(img2)
info3 <- image_info(img3)
info4 <- image_info(img4)

dimensions <- data.frame(
  Figure = c(
    "Autumn 2020",
    "Autumn 2021",
    "Autumn 2022",
    "Autumn 2023"
  ),
  Width_px = c(
    info1$width,
    info2$width,
    info3$width,
    info4$width
  ),
  Height_px = c(
    info1$height,
    info2$height,
    info3$height,
    info4$height
  )
)

message("\n===== ORIGINAL IMAGE DIMENSIONS =====")
print(dimensions, row.names = FALSE)


# ============================================================
# CHECK WHETHER DIMENSIONS MATCH
# ============================================================

same_width <- length(unique(dimensions$Width_px)) == 1
same_height <- length(unique(dimensions$Height_px)) == 1

if (!same_width || !same_height) {
  
  stop(
    paste0(
      "The four TIFFs do not have identical dimensions.\n",
      "The script stopped BEFORE modifying anything."
    )
  )
}

message("\n✓ All four maps have identical dimensions.")
message("✓ No resizing will be performed.")


# ============================================================
# ORIGINAL DIMENSIONS
# ============================================================

W <- dimensions$Width_px[1]
H <- dimensions$Height_px[1]

message(
  "\nOriginal dimensions: ",
  W, " x ", H, " px"
)


# ============================================================
# CROPPING SETTINGS
#
# IMPORTANT:
#
# Maps 1 and 3:
#   NO CROP
#
# Maps 2 and 4:
#   remove 380 px from LEFT
#   to eliminate repeated Latitude
#
# Small gap between columns:
#   20 px
# ============================================================

crop_left_rightcolumn <- 380
gap_px <- 20


# ============================================================
# MAPS 1 AND 3
#
# DO NOT CROP
#
# Autumn 2020 and Autumn 2022 remain exactly
# as they were in the original TIFF files.
# ============================================================

img1_final <- img1
img3_final <- img3


# ============================================================
# MAP 2
#
# Autumn 2021
# Remove LEFT side containing repeated Latitude
# ============================================================

img2_final <- image_crop(
  img2,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# MAP 4
#
# Autumn 2023
# Remove LEFT side containing repeated Latitude
# ============================================================

img4_final <- image_crop(
  img4,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# CREATE SMALL GAP BETWEEN COLUMNS
# ============================================================

gap <- image_blank(
  width = gap_px,
  height = H,
  color = "white"
)


# ============================================================
# CREATE TOP ROW
#
# Autumn 2020 | Autumn 2021
# ============================================================

row_top <- image_append(
  c(
    img1_final,
    gap,
    img2_final
  ),
  stack = FALSE
)


# ============================================================
# CREATE BOTTOM ROW
#
# Autumn 2022 | Autumn 2023
# ============================================================

row_bottom <- image_append(
  c(
    img3_final,
    gap,
    img4_final
  ),
  stack = FALSE
)


# ============================================================
# COMBINE BOTH ROWS
# ============================================================

figure_final_autumn <- image_append(
  c(
    row_top,
    row_bottom
  ),
  stack = TRUE
)


# ============================================================
# CHECK FINAL DIMENSIONS
# ============================================================

final_info <- image_info(figure_final_autumn)

message("\n===== FINAL AUTUMN FIGURE =====")

message(
  "Dimensions: ",
  final_info$width,
  " x ",
  final_info$height,
  " px"
)


# ============================================================
# OUTPUT FILE
# ============================================================

output_file <- file.path(
  folder,
  "GRID_OVERLAP_AUTUMN_2020_2023_4PANELS_COMPACT_V2.tiff"
)


# ============================================================
# SAVE FINAL TIFF
# ============================================================

image_write(
  figure_final_autumn,
  path = output_file,
  format = "tiff",
  density = "300x300",
  compression = "lzw"
)


# ============================================================
# CONFIRM OUTPUT
# ============================================================

message("\n✓ FINAL AUTUMN 2 x 2 FIGURE SAVED:")
message(output_file)

message("\n✓ Original TIFFs were NOT modified.")
message("✓ No resizing was performed.")
message("✓ 2020 and 2022 were NOT cropped.")
message("✓ Latitude retained on 2020 and 2022.")
message("✓ Repeated Latitude removed from 2021 and 2023.")
message("✓ Output written at 300 dpi with LZW compression.")


# ============================================================
# FINAL FILE CHECK
# ============================================================

if (file.exists(output_file)) {
  
  size_mb <- file.info(output_file)$size / 1024^2
  
  message(
    "\n✓ File successfully created (",
    round(size_mb, 2),
    " MB)."
  )
  
} else {
  
  warning("The output file could not be found after saving.")
  
}


#
#
#

# Figura inverno #

# ============================================================
# WINTER OVERLAP MAPS
# COMBINE 4 TIFF MAPS INTO A COMPACT 2 x 2 FIGURE
#
# Arrangement:
#
#   2020 | 2021
#   -----------
#   2022 | 2023
#
# Maps 1 and 3:
#   COMPLETELY preserved
#   Latitude retained
#   NO crop on the right
#
# Maps 2 and 4:
#   LEFT side cropped to remove repeated Latitude
#
# Original TIFFs are NOT modified.
# No resizing is performed.
# ============================================================


# ============================================================
# PACKAGE
# ============================================================

if (!requireNamespace("magick", quietly = TRUE)) {
  install.packages("magick")
}

library(magick)


# ============================================================
# FOLDER
# ============================================================

folder <- paste0(
  "C:/Users/SH01IQ/OneDrive - UHI/Desktop/",
  "grid_overlap_5km_plotter_ALL_cells/winter"
)


# ============================================================
# INPUT FILES
#
# Final arrangement:
#
#   1 | 2
#   -----
#   3 | 4
#
# ============================================================

file1 <- file.path(
  folder,
  "GRID_overlap_winter_2020_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file2 <- file.path(
  folder,
  "GRID_overlap_winter_2021_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file3 <- file.path(
  folder,
  "GRID_overlap_winter_2022_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)

file4 <- file.path(
  folder,
  "GRID_overlap_winter_2023_cell5km_plotter_ALL_self_ALL_20260922_112741.tiff"
)


# ============================================================
# CHECK THAT ALL FILES EXIST
# ============================================================

files <- c(file1, file2, file3, file4)

if (!all(file.exists(files))) {
  
  missing_files <- files[!file.exists(files)]
  
  stop(
    paste(
      "The following file(s) could not be found:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    )
  )
}

message("✓ All four TIFF files were found.")


# ============================================================
# READ TIFF FILES
# ============================================================

img1 <- image_read(file1)
img2 <- image_read(file2)
img3 <- image_read(file3)
img4 <- image_read(file4)


# ============================================================
# CHECK ORIGINAL DIMENSIONS
# ============================================================

info1 <- image_info(img1)
info2 <- image_info(img2)
info3 <- image_info(img3)
info4 <- image_info(img4)

dimensions <- data.frame(
  Figure = c(
    "Winter 2020",
    "Winter 2021",
    "Winter 2022",
    "Winter 2023"
  ),
  Width_px = c(
    info1$width,
    info2$width,
    info3$width,
    info4$width
  ),
  Height_px = c(
    info1$height,
    info2$height,
    info3$height,
    info4$height
  )
)

message("\n===== ORIGINAL IMAGE DIMENSIONS =====")
print(dimensions, row.names = FALSE)


# ============================================================
# CHECK WHETHER DIMENSIONS MATCH
# ============================================================

same_width <- length(unique(dimensions$Width_px)) == 1
same_height <- length(unique(dimensions$Height_px)) == 1

if (!same_width || !same_height) {
  
  stop(
    paste0(
      "The four TIFFs do not have identical dimensions.\n",
      "The script stopped BEFORE modifying anything."
    )
  )
}

message("\n✓ All four maps have identical dimensions.")
message("✓ No resizing will be performed.")


# ============================================================
# ORIGINAL DIMENSIONS
# ============================================================

W <- dimensions$Width_px[1]
H <- dimensions$Height_px[1]

message(
  "\nOriginal dimensions: ",
  W, " x ", H, " px"
)


# ============================================================
# CROPPING SETTINGS
#
# Maps 1 and 3:
#   NO CROP
#
# Maps 2 and 4:
#   remove 380 px from LEFT
#   to eliminate repeated Latitude
#
# Small gap between columns:
#   20 px
# ============================================================

crop_left_rightcolumn <- 380
gap_px <- 20


# ============================================================
# MAPS 1 AND 3
#
# DO NOT CROP
#
# Winter 2020 and Winter 2022 remain exactly
# as they were in the original TIFF files.
# ============================================================

img1_final <- img1
img3_final <- img3


# ============================================================
# MAP 2
#
# Winter 2021
# Remove LEFT side containing repeated Latitude
# ============================================================

img2_final <- image_crop(
  img2,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# MAP 4
#
# Winter 2023
# Remove LEFT side containing repeated Latitude
# ============================================================

img4_final <- image_crop(
  img4,
  geometry = geometry_area(
    width  = W - crop_left_rightcolumn,
    height = H,
    x_off  = crop_left_rightcolumn,
    y_off  = 0
  ),
  repage = TRUE
)


# ============================================================
# CREATE SMALL GAP BETWEEN COLUMNS
# ============================================================

gap <- image_blank(
  width = gap_px,
  height = H,
  color = "white"
)


# ============================================================
# CREATE TOP ROW
#
# Winter 2020 | Winter 2021
# ============================================================

row_top <- image_append(
  c(
    img1_final,
    gap,
    img2_final
  ),
  stack = FALSE
)


# ============================================================
# CREATE BOTTOM ROW
#
# Winter 2022 | Winter 2023
# ============================================================

row_bottom <- image_append(
  c(
    img3_final,
    gap,
    img4_final
  ),
  stack = FALSE
)


# ============================================================
# COMBINE BOTH ROWS
# ============================================================

figure_final_winter <- image_append(
  c(
    row_top,
    row_bottom
  ),
  stack = TRUE
)


# ============================================================
# CHECK FINAL DIMENSIONS
# ============================================================

final_info <- image_info(figure_final_winter)

message("\n===== FINAL WINTER FIGURE =====")

message(
  "Dimensions: ",
  final_info$width,
  " x ",
  final_info$height,
  " px"
)


# ============================================================
# OUTPUT FILE
# ============================================================

output_file <- file.path(
  folder,
  "GRID_OVERLAP_WINTER_2020_2023_4PANELS_COMPACT.tiff"
)


# ============================================================
# SAVE FINAL TIFF
# ============================================================

image_write(
  figure_final_winter,
  path = output_file,
  format = "tiff",
  density = "300x300",
  compression = "lzw"
)


# ============================================================
# CONFIRM OUTPUT
# ============================================================

message("\n✓ FINAL WINTER 2 x 2 FIGURE SAVED:")
message(output_file)

message("\n✓ Original TIFFs were NOT modified.")
message("✓ No resizing was performed.")
message("✓ 2020 and 2022 were NOT cropped.")
message("✓ Latitude retained on 2020 and 2022.")
message("✓ Repeated Latitude removed from 2021 and 2023.")
message("✓ Output written at 300 dpi with LZW compression.")


# ============================================================
# FINAL FILE CHECK
# ============================================================

if (file.exists(output_file)) {
  
  size_mb <- file.info(output_file)$size / 1024^2
  
  message(
    "\n✓ File successfully created (",
    round(size_mb, 2),
    " MB)."
  )
  
} else {
  
  warning("The output file could not be found after saving.")
  
}

