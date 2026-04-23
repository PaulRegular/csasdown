extract_docx_document_xml <- function(docx_path) {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  utils::unzip(docx_path, exdir = tmp_dir)
  xml_path <- file.path(tmp_dir, "word", "document.xml")
  xml <- paste(readLines(xml_path, warn = FALSE), collapse = "")
  list(xml = xml, tmp_dir = tmp_dir)
}

write_docx_document_xml <- function(docx_path, xml, tmp_dir) {
  xml_path <- file.path(tmp_dir, "word", "document.xml")
  writeLines(xml, xml_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp_dir)
  files <- list.files(
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  utils::zip(zipfile = docx_path, files = files, flags = "-q")
}

split_docx_paragraphs <- function(xml) {
  hits <- gregexpr("<w:p\\b.*?</w:p>", xml, perl = TRUE)[[1]]
  if (identical(hits[[1]], -1L)) {
    return(list(paragraphs = character(), starts = integer(), lengths = integer()))
  }
  lengths <- attr(hits, "match.length")
  paragraphs <- substring(xml, hits, hits + lengths - 1L)
  list(paragraphs = paragraphs, starts = hits, lengths = lengths)
}

paragraph_plain_text <- function(paragraph) {
  text_bits <- regmatches(paragraph, gregexpr("<w:t[^>]*>.*?</w:t>", paragraph, perl = TRUE))[[1]]
  if (!length(text_bits)) {
    return("")
  }
  text <- gsub("<[^>]+>", "", text_bits)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  paste(text, collapse = "")
}

extract_tag_block <- function(source_docx, tag) {
  source_xml <- extract_docx_document_xml(source_docx)
  on.exit(unlink(source_xml$tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  split <- split_docx_paragraphs(source_xml$xml)
  texts <- trimws(vapply(split$paragraphs, paragraph_plain_text, character(1)))

  start_matches <- which(texts == paste0("START:", tag))
  end_matches <- which(texts == paste0("END:", tag))

  if (!length(start_matches) || !length(end_matches)) {
    warning(sprintf("Tag '%s' not found in source frontmatter document.", tag), call. = FALSE)
    return(NULL)
  }

  if (length(start_matches) > 1L || length(end_matches) > 1L) {
    stop(sprintf("Duplicate tag block for '%s'.", tag), call. = FALSE)
  }

  if (start_matches[[1]] >= end_matches[[1]]) {
    stop(sprintf("Malformed tag block for '%s'.", tag), call. = FALSE)
  }

  split$paragraphs[(start_matches[[1]] + 1L):(end_matches[[1]] - 1L)]
}

replace_bookmark_paragraph <- function(target_xml, bookmark, replacement_paragraphs) {
  split <- split_docx_paragraphs(target_xml)
  if (!length(split$paragraphs)) {
    warning(sprintf("Bookmark '%s' not found in target document.", bookmark), call. = FALSE)
    return(target_xml)
  }

  bkm_pattern <- sprintf('w:bookmarkStart[^>]*w:name="%s"', bookmark)
  idx <- which(grepl(bkm_pattern, split$paragraphs, perl = TRUE))

  if (!length(idx)) {
    warning(sprintf("Bookmark '%s' not found in target document.", bookmark), call. = FALSE)
    return(target_xml)
  }

  if (length(idx) > 1L) {
    warning(sprintf("Bookmark '%s' appears multiple times; using first occurrence.", bookmark), call. = FALSE)
  }

  i <- idx[[1]]
  start <- split$starts[[i]]
  end <- split$starts[[i]] + split$lengths[[i]] - 1L

  before <- if (start > 1L) substr(target_xml, 1L, start - 1L) else ""
  after <- substr(target_xml, end + 1L, nchar(target_xml))

  paste0(before, paste(replacement_paragraphs, collapse = ""), after)
}

remove_marker_sections <- function(doc, tags) {
  docx_path <- tempfile(fileext = ".docx")
  print(doc, target = docx_path)

  xml_obj <- extract_docx_document_xml(docx_path)
  on.exit(unlink(c(xml_obj$tmp_dir, docx_path), recursive = TRUE, force = TRUE), add = TRUE)

  split <- split_docx_paragraphs(xml_obj$xml)
  texts <- trimws(vapply(split$paragraphs, paragraph_plain_text, character(1)))

  start_pat <- paste0("^START:(", paste(tags, collapse = "|"), ")$")
  end_pat <- paste0("^END:(", paste(tags, collapse = "|"), ")$")

  starts <- which(grepl(start_pat, texts, perl = TRUE))
  ends <- which(grepl(end_pat, texts, perl = TRUE))

  if (length(starts) && length(ends)) {
    remove_start <- split$starts[[min(starts)]]
    remove_end <- split$starts[[max(ends)]] + split$lengths[[max(ends)]] - 1L
    before <- if (remove_start > 1L) substr(xml_obj$xml, 1L, remove_start - 1L) else ""
    after <- substr(xml_obj$xml, remove_end + 1L, nchar(xml_obj$xml))
    xml_obj$xml <- paste0(before, after)

    write_docx_document_xml(docx_path, xml_obj$xml, xml_obj$tmp_dir)
  }

  officer::read_docx(docx_path)
}

#' Move one marker-wrapped text block into a bookmark
#'
#' @param doc Target `officer::rdocx` object.
#' @param source_docx Source rendered frontmatter `.docx` path.
#' @param target_bookmark Bookmark in target document.
#' @param source_tag Marker tag in source document.
#'
#' @noRd
move_text <- function(doc, source_docx, target_bookmark, source_tag) {
  move_text_blocks(doc, source_docx, stats::setNames(source_tag, target_bookmark))
}

#' Move multiple marker-wrapped text blocks into bookmarks
#'
#' @param doc Target `officer::rdocx` object.
#' @param source_docx Source rendered frontmatter `.docx` path.
#' @param tag_map Named character vector of `bookmark = tag`.
#'
#' @noRd
move_text_blocks <- function(doc, source_docx, tag_map) {
  if (!length(tag_map)) {
    return(doc)
  }

  if (is.null(names(tag_map)) || any(!nzchar(names(tag_map)))) {
    stop("tag_map must be a named character vector of bookmark = tag values.", call. = FALSE)
  }

  target_docx <- tempfile(fileext = ".docx")
  print(doc, target = target_docx)

  target_xml <- extract_docx_document_xml(target_docx)
  on.exit(unlink(c(target_docx, target_xml$tmp_dir), recursive = TRUE, force = TRUE), add = TRUE)

  xml <- target_xml$xml

  for (bookmark in names(tag_map)) {
    tag <- unname(tag_map[[bookmark]])
    block <- extract_tag_block(source_docx, tag)
    if (is.null(block)) {
      next
    }
    xml <- replace_bookmark_paragraph(xml, bookmark, block)
  }

  write_docx_document_xml(target_docx, xml, target_xml$tmp_dir)

  officer::read_docx(target_docx)
}
