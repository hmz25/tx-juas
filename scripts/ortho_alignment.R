# ============================================================================
# UAV Orthomosaic Co-registration at Sonora Site  --  TRANSLATION ONLY
# With interactive zoom (draw() + click()) for precise feature picking
# yuxia.liu@cornell.edu
# ----------------------------------------------------------------------------
# Workflow for each target image:
#   1. Full-mosaic view of reference
#   2. draw() a small box around a stable feature (rocks, fence corner, etc.)
#   3. Zoomed view -> click() the feature precisely on REFERENCE
#   4. Same zoom box on TARGET -> click() the same feature
#   5. Repeat for 8 points spread across the image
#   6. Compute median (dx, dy), snap to pixel grid, shift + resample target
#
# Key precision trick: each zoom window is 5-10 m wide, so you can click
# within 1-2 cm of the true feature location.
# ============================================================================

library(terra)
library(tidyverse)

# ----------------------------------------------------------------------------
# 1. Paths
# ----------------------------------------------------------------------------

#set working dir

#lab desktop
# setwd("C:/Users/hmz25/Box/Katz lab/")

#hz laptop
# setwd('/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas')

#mo comp
setwd("C:/Users/HMZ/Box/texas")

img_dir <- "01_data/orthos" 
# out_dir <- "03_output/aligned_orthos" #directory for aligned images
out_dir <- "F:/aligned_orthos"
# dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ref_path <- file.path(img_dir, "gun_20260111_transparent_mosaic_group1_corrected.tif") #replace with your ref image name
ref_path <- "F:/aligned_orthos/kimble_20260114_transparent_mosaic_group1_corrected.tif"

target_paths <- file.path(img_dir, "kimble_20240117_transparent_mosaic_group1.tif") #replace with image you want to align

ref <- rast(ref_path)
# plotRGB(ref)
cat("Reference:", ref_path, "\n")
cat("  CRS:", crs(ref, describe = TRUE)$name, "\n")
cat("  Resolution (m):", res(ref), "\n\n")


# ----------------------------------------------------------------------------
# 2. Interactive picker with zoom (MAIN FUNCTION)
# ----------------------------------------------------------------------------
# For each of n points:
#   - Shows full reference, you draw() a zoom box
#   - Zooms in on reference, you click() the feature
#   - Zooms in on target (SAME box), you click() the same feature
# Press ESC in any click() step to abort.
#
# Tip: pick features spread across the image (corners + center).
# GOOD: bare rock clusters, road Y-junctions, fence posts, tanks, boulders
# BAD:  juniper canopies (change between dates), shadows, vegetation

pick_matching_points_zoom <- function(ref_img, tgt_img, n = 8,
                                      rgb_bands = c(1, 2, 3)) {
  
  pts <- data.frame(ref_x = numeric(n), ref_y = numeric(n),
                    tgt_x = numeric(n), tgt_y = numeric(n))
  
  for (i in seq_len(n)) {
    cat(sprintf("\n========== Point %d of %d ==========\n", i, n))
    
    # --- Step A: Full reference view, draw zoom box --------------------
    cat("  A) Click TWO corners on REFERENCE to draw a small zoom box\n")
    cat("     around a stable feature (aim for 5-10 m wide)\n")
    plotRGB(ref_img, r = rgb_bands[1], g = rgb_bands[2], b = rgb_bands[3],
            stretch = "lin",
            main = sprintf("Point %d/%d: draw zoom box on REFERENCE", i, n))
    e <- draw(x = "extent", col = "yellow")     # two clicks -> extent
    cat(sprintf("     zoom window: %.1f m wide x %.1f m tall\n",
                e[2] - e[1], e[4] - e[3]))
    
    # --- Step B: Zoomed reference, click feature -----------------------
    cat("  B) Click the feature on zoomed REFERENCE\n")
    plotRGB(crop(ref_img, e), r = rgb_bands[1], g = rgb_bands[2],
            b = rgb_bands[3], stretch = "lin",
            main = sprintf("Point %d/%d - REFERENCE: click feature", i, n))
    p_ref <- click(ref_img, n = 1, xy = TRUE, show = TRUE)
    
    # --- Step C: Zoomed target (same box), click same feature ----------
    cat("  C) Click the SAME feature on zoomed TARGET\n")
    plotRGB(crop(tgt_img, e), r = rgb_bands[1], g = rgb_bands[2],
            b = rgb_bands[3], stretch = "lin",
            main = sprintf("Point %d/%d - TARGET: click same feature", i, n))
    p_tgt <- click(tgt_img, n = 1, xy = TRUE, show = TRUE)
    
    pts[i, ] <- c(p_ref[, "x"], p_ref[, "y"],
                  p_tgt[, "x"], p_tgt[, "y"])
    
    # Quick feedback: per-point shift
    dx_i <- p_ref[, "x"] - p_tgt[, "x"]
    dy_i <- p_ref[, "y"] - p_tgt[, "y"]
    cat(sprintf("     -> this point's shift: dx=%+.3f m, dy=%+.3f m\n",
                dx_i, dy_i))
  }
  
  cat("\nAll", n, "points collected.\n")
  pts
}


# ----------------------------------------------------------------------------
# 3. Estimate translation (median, robust to bad clicks)
# ----------------------------------------------------------------------------
estimate_shift <- function(pts, ref_img, snap_to_pixel = TRUE) {
  
  dx_i <- pts$ref_x - pts$tgt_x
  dy_i <- pts$ref_y - pts$tgt_y
  
  dx <- median(dx_i);  dy <- median(dy_i)
  
  cat("\nPer-point shifts (m):\n")
  print(data.frame(dx = round(dx_i, 3), dy = round(dy_i, 3)))
  
  cat(sprintf("\n  median dx = %+7.3f m   mean dx = %+7.3f m\n", dx, mean(dx_i)))
  cat(sprintf("  median dy = %+7.3f m   mean dy = %+7.3f m\n", dy, mean(dy_i)))
  cat(sprintf("  sd(dx)    = %7.3f m   sd(dy)  = %7.3f m\n",
              sd(dx_i), sd(dy_i)))
  
  # Flag outliers (>2 sd from median shift)
  bad <- which(abs(dx_i - dx) > 2 * sd(dx_i) |
                 abs(dy_i - dy) > 2 * sd(dy_i))
  if (length(bad)) {
    cat("\n  Suspicious points (>2sd from median):", bad,
        "\n  -> consider dropping them and re-running estimate_shift().\n")
    cat("     e.g.  estimate_shift(pts[-c(", paste(bad, collapse = ","),
        "), ], ref)\n")
  }
  
  # Snap to pixel grid -> integer-pixel shift, clean downstream resampling
  if (snap_to_pixel) {
    px <- res(ref_img)
    dx_snap <- round(dx / px[1]) * px[1]
    dy_snap <- round(dy / px[2]) * px[2]
    cat(sprintf("\n  Snapped to pixel grid (res = %.4f, %.4f m):\n",
                px[1], px[2]))
    cat(sprintf("    dx = %+7.4f m  (%d pixels)\n", dx_snap,
                round(dx / px[1])))
    cat(sprintf("    dy = %+7.4f m  (%d pixels)\n", dy_snap,
                round(dy / px[2])))
    return(c(dx = dx_snap, dy = dy_snap))
  }
  c(dx = dx, dy = dy)
}


# ----------------------------------------------------------------------------
# 4. Apply shift + resample to reference grid
# ----------------------------------------------------------------------------
shift_raster <- function(tgt_img, shift_vec, ref_img = NULL,
                         resample_to_ref = TRUE, out_path = NULL) {
  
  # shift() is metadata-only (no resampling): moves origin by (dx, dy)
  shifted <- shift(tgt_img, dx = shift_vec["dx"], dy = shift_vec["dy"])
  
  if (resample_to_ref && !is.null(ref_img)) {
    # Resample onto reference grid so pixel centers align exactly,
    # enabling clean pixel-wise multi-date spectral index comparison
    out <- resample(shifted, ref_img, method = "bilinear",
                    filename = if (!is.null(out_path)) out_path else "",
                    overwrite = TRUE)
  } else if (!is.null(out_path)) {
    out <- writeRaster(shifted, out_path, overwrite = TRUE)
  } else {
    out <- shifted
  }
  out
}


# ----------------------------------------------------------------------------
# 5. RUN ONE TARGET (interactive) 
# ----------------------------------------------------------------------------
tgt <- rast(target_paths)
cat("\n--- Target:", target_paths, "---\n")

# Get the target filename (without directory, without .tif extension)
# e.g. "sonora_20240116_transparent_mosaic_group1"
tgt_base <- tools::file_path_sans_ext(basename(target_paths))

if (!same.crs(ref, tgt)) {
  cat("CRS differ -- reprojecting target to reference CRS.\n")
  tgt <- project(tgt, crs(ref))
}

# 5a. Pick matching points with zoom
pts_i <- pick_matching_points_zoom(ref, tgt, n = 8)

# Save so you don't lose the work if something crashes
# Output CSV name: gcps_<target_filename>.csv
write.csv(pts_i,
          file.path(out_dir, paste0("gcps_", tgt_base, ".csv")),
          row.names = FALSE)

kimb_gcps <- read_csv("3_output/aligned_orthos/gcps_kimble_20240117_transparent_mosaic_group1.csv")
kimb_gcps <- read_csv("C:/Users/HMZ/Box/texas/03_output/aligned_orthos/gcps_kimble_20240117_transparent_mosaic_group1.csv")

pts_i <- kimb_gcps

# 5b. Estimate shift
shift_i <- estimate_shift(pts_i, ref, snap_to_pixel = TRUE)

# 5c. Apply shift + resample
# Output TIFF name: <target_filename>_aligned.tif
out_i <- file.path(out_dir, paste0(tgt_base, "_aligned.tif"))
aligned_i <- shift_raster(tgt, shift_i, ref_img = ref,
                          resample_to_ref = TRUE, out_path = out_i)

cat("\nDone. Outputs:\n")
cat("  GCPs:    ", file.path(out_dir, paste0("gcps_", tgt_base, ".csv")), "\n")
cat("  Aligned: ", out_i, "\n")

#manually remove large objects (generic R memory cleanup)
rm(aligned_i, ref, tgt)

#force garbage collection 
gc()

#remove all temporary files from the current session
tmpFiles(current = TRUE, remove = TRUE)

#remove orphaned temporary files (files no longer attached to an active SpatRaster object)
tmpFiles(orphan = TRUE, remove = TRUE)
