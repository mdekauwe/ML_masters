# set file path
df <- read.csv("your_file.csv", stringsAsFactors = FALSE)

urls <- df$download_link
urls <- urls[!is.na(urls) & urls != ""]

# Output folder?
outdir <- "downloads"
dir.create(outdir, showWarnings = FALSE)

for (url in urls) {

  clean_url <- strsplit(url, "\\?")[[1]][1]
  filename <- basename(clean_url)
  destfile <- file.path(outdir, filename)

  tryCatch({
    download.file(url, destfile = destfile, mode = "wb")
    cat("Downloaded:", filename, "\n")
  }, error = function(e) {
    cat("Failed:", url, "\n")
  })
}
