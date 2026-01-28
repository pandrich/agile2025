# Reconstruct Quarto .qmd from RStudio notebook recovery files
# Dependencies: jsonlite, readr, stringr
# Adjust base_dir to the folder you found (the one containing folder "1")
library(jsonlite)
library(readr)
library(stringr)

root <- rprojroot::has_file("agile2025.Rproj")$make_fix_file()
base_dir <- root(".Rproj.user/shared/notebooks/8E833D23-build_climate_malnourishment_violence_uganda_dataset/1/75CAFB78d3bf8e8d")

# Helper: read a file robustly and return a string
read_chunk_file <- function(f) {
  if (!file.exists(f)) return(NA_character_)
  ext <- tolower(tools::file_ext(f))
  # If it's csv or tsv, try to extract textual column(s)
  if (ext %in% c("csv","tsv")) {
    # try to read, then collapse all columns (fallback)
    tryCatch({
      df <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
      # heuristics: prefer a column named "text" or "code" or "chunk"
      for (cn in c("text","code","chunk","content")) {
        if (cn %in% tolower(names(df))) {
          col <- df[[which(tolower(names(df))==cn)[1]]]
          return(paste(col, collapse = "\n"))
        }
      }
      # fallback: paste all columns row-wise
      paste(apply(df, 1, function(r) paste(r, collapse = " ")), collapse = "\n")
    }, error = function(e) {
      # fallback: raw read
      tryCatch(read_file(f), error = function(e2) NA_character_)
    })
  } else {
    # text-like file: read raw
    tryCatch(read_file(f), error = function(e) NA_character_)
  }
}

# Locate all chunks.json files beneath base_dir
chunks_jsons <- list.files(base_dir, pattern = "chunks\\.json$", recursive = TRUE, full.names = TRUE)

if (length(chunks_jsons) == 0) {
  stop("No chunks.json files found under: ", base_dir)
}

recovered_chunks <- list()

for (cj in chunks_jsons) {
  js <- tryCatch(fromJSON(cj, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(js)) next
  
  # Try a few common structures: a list named "chunks", or top-level array
  entries <- NULL
  if (!is.null(js$chunks)) {
    entries <- js$chunks
  } else if (is.list(js) && length(js) > 0 && all(sapply(js, function(x) is.list(x)))) {
    # might already be an array of chunk objects
    entries <- js
  } else {
    # fallback: store raw JSON text (for manual inspection)
    recovered_chunks[[length(recovered_chunks)+1]] <- list(type="raw_json", content=toJSON(js, pretty=TRUE), order=NA, source=cj)
    next
  }
  
  # iterate chunk entries
  for (i in seq_along(entries)) {
    ent <- entries[[i]]
    # Try to discover content file path from common fields
    file_candidate <- NULL
    if (!is.null(ent$file)) file_candidate <- file.path(dirname(cj), ent$file)
    if (is.null(file_candidate) && !is.null(ent$path)) file_candidate <- file.path(dirname(cj), ent$path)
    if (is.null(file_candidate) && !is.null(ent$id)) {
      # sometimes files are named by id in sibling folder
      possible <- list.files(dirname(cj), pattern = ent$id, recursive = TRUE, full.names = TRUE)
      if (length(possible)>0) file_candidate <- possible[1]
    }
    # if still NULL, look for small text-like files in the same folder
    if (is.null(file_candidate)) {
      siblings <- list.files(dirname(cj), full.names = TRUE)
      # prefer small files under 200KB
      fl <- siblings[file.info(siblings)$size < 200000]
      # choose first .csv or .txt
      if (length(fl)>0) {
        extpref <- fl[grepl("\\.(csv|txt|r|py|md)$", fl, ignore.case = TRUE)]
        if (length(extpref)>0) file_candidate <- extpref[1] else file_candidate <- fl[1]
      }
    }
    
    content_text <- if (!is.null(file_candidate) && file.exists(file_candidate)) {
      read_chunk_file(file_candidate)
    } else if (!is.null(ent$text) && nzchar(as.character(ent$text))) {
      as.character(ent$text)
    } else {
      NA_character_
    }
    
    # chunk language/type heuristics
    chunk_type <- NA_character_
    if (!is.null(ent$lang)) chunk_type <- ent$lang
    if (is.null(chunk_type) && !is.null(ent$type)) chunk_type <- ent$type
    if (is.null(chunk_type) && !is.na(file_candidate)) {
      ext <- tolower(tools::file_ext(file_candidate))
      if (ext %in% c("r")) chunk_type <- "r"
      if (ext %in% c("py","python")) chunk_type <- "python"
      if (ext %in% c("md","qmd","txt")) chunk_type <- "markdown"
    }
    
    recovered_chunks[[length(recovered_chunks)+1]] <- list(
      type = ifelse(is.null(chunk_type),"unknown",chunk_type),
      content = ifelse(is.na(content_text),"",content_text),
      order = ifelse(!is.null(ent$order), ent$order, i),
      source = file_candidate
    )
  }
}

# Sort by order (if available)
ord <- sapply(recovered_chunks, function(x) ifelse(is.null(x$order) || is.na(x$order), Inf, as.numeric(x$order)))
recovered_chunks <- recovered_chunks[order(ord)]

# Compose .qmd
out_lines <- c("---",
               "title: \"Recovered notebook\"",
               "format: html", 
               "---",
               "")
for (rc in recovered_chunks) {
  ct <- trimws(rc$content)
  if (nchar(ct)==0) next
  if (tolower(rc$type) %in% c("r","rscript")) {
    out_lines <- c(out_lines, "```{r}", ct, "```", "")
  } else if (tolower(rc$type) %in% c("python","py")) {
    out_lines <- c(out_lines, "```{python}", ct, "```", "")
  } else if (tolower(rc$type) %in% c("markdown","md","text")) {
    out_lines <- c(out_lines, ct, "")
  } else {
    # default: put into an R fenced chunk if unknown
    out_lines <- c(out_lines, "```{r}", ct, "```", "")
  }
}

out_file <- file.path(root("notebooks"), "recovered-notebook.qmd")
writeLines(out_lines, out_file, useBytes = TRUE)
message("Wrote recovered Quarto file to: ", out_file)
