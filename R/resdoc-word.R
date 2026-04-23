#' @import bookdown
#' @rdname csas_docx
#' @export

resdoc_docx <- function(...) {
  .csasdown_docx_base(
    reference_docx = "resdoc-content-2026.docx",
    link_citations = TRUE,
    template_dir = "csas-docx",
    use_pandoc_highlight = TRUE,
    ...
  )
}

#' Fix missing namespaces in merged document
#'
#' @param docx_path Path to the .docx file to fix
#' @keywords internal
#' @noRd
fix_missing_namespaces <- function(docx_path) {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  utils::unzip(docx_path, exdir = temp_dir)

  doc_xml_path <- file.path(temp_dir, "word", "document.xml")
  doc_content <- readLines(doc_xml_path, warn = FALSE)

  # Add missing namespace declarations to the root element if not present
  if (!any(grepl('xmlns:a=', doc_content[1:5]))) {
    doc_content[2] <- gsub(
      '<w:document ',
      '<w:document xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" ',
      doc_content[2]
    )
  }

  writeLines(doc_content, doc_xml_path)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(temp_dir)
  files <- list.files(recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
  utils::zip(zipfile = file.path(old_wd, docx_path), files = files, flags = "-q")

  unlink(temp_dir, recursive = TRUE)

  invisible()
}


add_resdoc_word_frontmatter2 <- function(index_fn, yaml_fn = "_bookdown.yml", verbose = FALSE, keep_files = FALSE) {
  if (verbose) cli_inform("Adding frontmatter to the Research Document using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)

  french <- isTRUE(x$output[[1]]$french)

  front_filename <- if (french) "resdoc-frontmatter-french2.docx" else "resdoc-frontmatter-english2.docx"
  ref_front_file <- system.file("csas-docx", front_filename, package = "csasdown")
  toc_keyword <- if (french) "TABLE DES MATIÈRES" else "TABLE OF CONTENTS"
  region <- if (french) x$french_region else x$english_region
  book_filename <- paste0("_book/", get_book_filename(yaml_fn), ".docx")

  frontmatter <- officer::read_docx(ref_front_file) |>
    officer::headers_replace_text_at_bkm("region", region) |>
    officer::headers_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::cursor_reach(keyword = toc_keyword) |>
    officer::body_add_toc()

  print(frontmatter, target = "tmp-frontmatter.docx")
  fix_missing_namespaces("tmp-frontmatter.docx")

  content <- officer::read_docx(book_filename) |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(content, target = "tmp-content.docx")

  full_doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::cursor_end() |>
    officer::body_import_docx("tmp-content.docx") |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(full_doc, target = book_filename)

  move_text(
    book_filename,
    c(
      title = "title",
      authors = "authors",
      address = "address",
      english_authors_list = "english_authors_list",
      year_english_reference1 = "year_english_reference1",
      year_english_reference = "year_english_reference",
      english_title = "english_title",
      french_authors_list = "french_authors_list",
      year_french_reference1 = "year_french_reference1",
      year_french_reference = "year_french_reference",
      french_title = "french_title",
      abstract = "abstract"
    )
  )

  fix_table_caption_alignment(book_filename, reference_docx = "resdoc-content-2026.docx")

  if (!keep_files) {
    unlink(c("tmp-frontmatter.docx", "tmp-content.docx"))
  }

  invisible()
}

inject_resdoc_frontmatter_text <- function(index_fn = "index.Rmd", yaml_fn = "_bookdown.yml") {
  x <- rmarkdown::yaml_front_matter(index_fn)
  parsed <- parse_author_field(x$author)
  x$english_author <- parsed$english_author
  x$french_author <- parsed$french_author
  x$english_author_list <- parsed$english_author_list
  x$french_author_list <- parsed$french_author_list

  french <- isTRUE(x$output[[1]]$french)

  title <- if (french) x$french_title else x$english_title
  authors <- if (french) x$french_author else x$english_author
  address <- if (french) x$french_address else x$english_address

  y <- yaml::read_yaml(yaml_fn)
  rmd_files <- unlist(y$rmd_files, use.names = FALSE)
  rmd_files <- rmd_files[!is.na(rmd_files)]
  if (length(rmd_files) == 0) {
    stop(sprintf("No rmd_files found in '%s'.", yaml_fn))
  }
  is_index <- tolower(basename(rmd_files)) == "index.rmd"
  target_candidates <- rmd_files[!is_index]
  first_rmd <- if (length(target_candidates) > 0) target_candidates[[1]] else rmd_files[[1]]

  original_lines <- readLines(first_rmd, warn = FALSE)
  if (length(original_lines) == 0) {
    stop(sprintf("No content found in '%s'.", first_rmd))
  }

  text_or_empty <- function(value) {
    if (is.null(value)) "" else as.character(value)
  }

  build_tagged_block <- function(tag, value) {
    c(paste0("START:", tag, "\n"), text_or_empty(value), paste0("\nEND:", tag), "\n")
  }

  frontmatter_lines <- c(
    build_tagged_block("title", title),
    build_tagged_block("authors", authors),
    build_tagged_block("address", address),
    build_tagged_block("english_authors_list", x$english_author_list),
    build_tagged_block("year_english_reference1", x$year),
    build_tagged_block("year_english_reference", x$year),
    build_tagged_block("english_title", x$english_title),
    build_tagged_block("french_authors_list", x$french_author_list),
    build_tagged_block("year_french_reference1", x$year),
    build_tagged_block("year_french_reference", x$year),
    build_tagged_block("french_title", x$french_title)
  )

  heading_pat <- "^#\\s*(ABSTRACT|R\u00c9SUM\u00c9|RESUME)\\s*$"
  abstract_heading_i <- which(grepl(heading_pat, original_lines, ignore.case = TRUE))

  processed_lines <- original_lines
  if (length(abstract_heading_i) > 0) {
    heading_i <- abstract_heading_i[[1]]
    remaining <- if (heading_i < length(processed_lines)) {
      processed_lines[(heading_i + 1):length(processed_lines)]
    } else {
      character()
    }
    next_h1_rel <- which(grepl("^#\\s+", remaining))
    next_h1_i <- if (length(next_h1_rel) > 0) heading_i + next_h1_rel[[1]] else length(processed_lines) + 1
    abstract_body <- if (heading_i + 1 <= next_h1_i - 1) processed_lines[(heading_i + 1):(next_h1_i - 1)] else character()
    abstract_tagged <- c("START:abstract\n", abstract_body, "END:abstract\n")
    suffix <- if (next_h1_i <= length(processed_lines)) processed_lines[next_h1_i:length(processed_lines)] else character()
    processed_lines <- c(
      processed_lines[seq_len(heading_i - 1)],
      processed_lines[[heading_i]],
      abstract_tagged,
      suffix
    )
  }

  writeLines(c(frontmatter_lines, processed_lines), con = first_rmd)

  list(file = first_rmd, original_lines = original_lines)
}

restore_injected_resdoc_frontmatter_text <- function(state) {
  if (is.null(state$file) || is.null(state$original_lines)) {
    return(invisible())
  }
  writeLines(state$original_lines, con = state$file)
  invisible()
}
