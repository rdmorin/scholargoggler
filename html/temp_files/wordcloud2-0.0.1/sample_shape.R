#!/usr/bin/env Rscript
# sample_shape.R: convert an image mask into a wordcloud2.js shape case block.
# Usage:
#   Rscript sample_shape.R /path/to/image.[png|jpg] shape_name [threshold 0-1]
# Steps:
#   - load the image (prefers alpha if present)
#   - build a binary mask (invert grayscale; optional threshold override)
#   - keep the largest connected component (drops watermarks/noise)
#   - sample 360 radii (farthest in mask along each ray from mask centroid)
#   - normalize radii and print a JS case block for wordcloud2-all.js

suppressWarnings({
  if (!requireNamespace("png", quietly = TRUE)) stop("Please install 'png'")
  if (!requireNamespace("jpeg", quietly = TRUE)) stop("Please install 'jpeg'")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'")
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript sample_shape.R /path/to/image.[png|jpg] shape_name [threshold 0-1]")
img_path <- args[1]
shape_name <- ifelse(length(args) >= 2, args[2], "myshape")
user_thresh <- ifelse(length(args) >= 3, as.numeric(args[3]), NA_real_)

is_png <- grepl("\\.png$", img_path, ignore.case = TRUE)
img <- if (is_png) png::readPNG(img_path) else jpeg::readJPEG(img_path)

# Build binary mask
if (length(dim(img)) == 3) {
  alpha <- if (dim(img)[3] == 4) img[, , 4] else NULL
  gray <- img[, , 1] * 0.299 + img[, , 2] * 0.587 + img[, , 3] * 0.114
} else {
  gray <- img
  alpha <- NULL
}

if (!is.null(alpha)) {
  mask <- alpha > 0.5
} else {
  # Treat darker pixels as foreground
  fg <- 1 - gray
  th <- if (is.na(user_thresh)) stats::quantile(fg, 0.8) else user_thresh
  mask <- fg > th
}

nr <- nrow(mask); nc <- ncol(mask)
if (!any(mask)) stop("Mask is empty; adjust threshold")

# Keep largest 4-neighbor component (drops watermarks)
visited <- matrix(FALSE, nr, nc)
comp_id <- matrix(0, nr, nc)
comp_sizes <- integer(0)
comp <- 0
drs <- c(-1, 1, 0, 0)
dcs <- c(0, 0, -1, 1)
for (r in seq_len(nr)) for (c in seq_len(nc)) {
  if (!mask[r, c] || visited[r, c]) next
  comp <- comp + 1
  q_r <- r
  q_c <- c
  head <- 1
  tail <- 1
  visited[r, c] <- TRUE
  comp_id[r, c] <- comp
  size <- 1
  while (head <= tail) {
    cr <- q_r[head]
    cc <- q_c[head]
    head <- head + 1
    for (k in 1:4) {
      nr2 <- cr + drs[k]
      nc2 <- cc + dcs[k]
      if (nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc) next
      if (!mask[nr2, nc2] || visited[nr2, nc2]) next
      tail <- tail + 1
      q_r[tail] <- nr2
      q_c[tail] <- nc2
      visited[nr2, nc2] <- TRUE
      comp_id[nr2, nc2] <- comp
      size <- size + 1
    }
  }
  comp_sizes[comp] <- size
}

keep <- which(comp_id == which.max(comp_sizes))
mask[,] <- FALSE
mask[keep] <- TRUE

coords <- which(mask, arr.ind = TRUE)
cy <- mean(coords[, 1])
cx <- mean(coords[, 2])

# Sample radii (farthest in mask along each ray)
samples <- 360
radii <- numeric(samples)
max_dim <- ceiling(sqrt(nr^2 + nc^2))
for (i in seq_len(samples)) {
  theta <- 2 * pi * (i - 1) / samples
  dx <- cos(theta)
  dy <- sin(theta)
  r <- 0
  best <- 0
  while (r <= max_dim) {
    x <- round(cx + r * dx)
    y <- round(cy + r * dy)
    if (x < 1 || y < 1 || x > nc || y > nr) break
    if (mask[y, x]) {
      best <- r
    } else if (best > 0) {
      break
    }
    r <- r + 1
  }
  radii[i] <- best
}
max_r <- max(radii)
radii <- as.integer(radii)

# Emit JS block
cat(sprintf("// %s sampled from %s\ncase '%s':\n", shape_name, img_path, shape_name))
cat("  var ", shape_name, "Radii = ", jsonlite::toJSON(radii, auto_unbox = TRUE), ";\n", sep = "")
cat("  var ", shape_name, "Max = ", max_r, ";\n", sep = "")
cat("  settings.shape = function ", shape_name, "(theta) {\n")
cat("    var len = ", shape_name, "Radii.length;\n", sep = "")
cat("    var idx = ((theta / (2 * Math.PI)) * len) | 0;\n")
cat("    if (settings.ellipticity === 0.65) { settings.ellipticity = 1; }\n")
cat("    return (", shape_name, "Radii[idx % len] / ", shape_name, "Max) * 0.9;\n", sep = "")
cat("  };\n")
cat("  break;\n")
