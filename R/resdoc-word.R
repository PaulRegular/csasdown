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

RESDOC_FRONTMATTER_TAG_MAP <- c(
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
  french_title = "french_title"
)

restore_resdoc_sources <- function(manifest) {
  if (!length(manifest)) {
    return(invisible())
  }

  for (path in names(manifest)) {
    backup <- manifest[[path]]
    if (file.exists(backup)) {
      file.copy(backup, path, overwrite = TRUE)
      unlink(backup)
    }
  }

  invisible()
}

extract_and_strip_abstract <- function(lines) {
  heading_idx <- grep(
    "^#\\s+(ABSTRACT|R[ÉE]SUM[ÉE])\\s*(\\{[^}]*\\})?\\s*$",
    lines,
    perl = TRUE
  )

  if (!length(heading_idx)) {
    return(list(abstract_lines = character(), content_lines = lines))
  }

  start <- heading_idx[[1]]
  later_h1 <- grep("^#\\s+", lines)
  end <- later_h1[later_h1 > start][[1]]
  if (is.na(end)) {
    end <- length(lines) + 1L
  }

  abstract_block <- lines[start:(end - 1L)]
  abstract_lines <- if (length(abstract_block) > 1L) abstract_block[-1] else character()
  abstract_lines <- abstract_lines[!grepl("^\\s*$", abstract_lines) | cumsum(!grepl("^\\s*$", abstract_lines)) > 0]

  before_lines <- if (start > 1L) lines[seq_len(start - 1L)] else character()
  after_lines <- if (end <= length(lines)) lines[end:length(lines)] else character()

  list(
    abstract_lines = abstract_lines,
    content_lines = c(before_lines, after_lines)
  )
}

get_first_resdoc_content_file <- function(yaml_fn) {
  yml <- yaml::read_yaml(yaml_fn)
  rmd_files <- yml$rmd_files
  if (is.null(rmd_files) || !length(rmd_files)) {
    cli::cli_abort("No rmd_files found in {.file {yaml_fn}}.")
  }

  index_idx <- which(basename(rmd_files) == "index.Rmd")
  if (!length(index_idx)) {
    cli::cli_abort("index.Rmd must appear in rmd_files in {.file {yaml_fn}}.")
  }

  if (index_idx[[1]] >= length(rmd_files)) {
    cli::cli_abort("No content file found after index.Rmd in {.file {yaml_fn}}.")
  }

  rmd_files[[index_idx[[1]] + 1L]]
}

build_frontmatter_generated_rmd <- function(meta, abstract_lines, path) {
  parsed <- parse_author_field(meta$author)
  meta$english_author <- parsed$english_author
  meta$french_author <- parsed$french_author
  meta$english_author_list <- parsed$english_author_list
  meta$french_author_list <- parsed$french_author_list

  french <- isTRUE(meta$output[[1]]$french)

  section <- function(tag, value) {
    val <- as.character(unlist(value, recursive = TRUE, use.names = FALSE))
    val <- val[!is.na(val)]
    if (!length(val)) val <- ""
    c(
      paste0("START:", tag),
      val,
      paste0("END:", tag),
      ""
    )
  }

  lines <- c(
    section("title", if (french) meta$french_title else meta$english_title),
    section("authors", if (french) meta$french_author else meta$english_author),
    section("address", if (french) meta$french_address else meta$english_address),
    section("english_authors_list", meta$english_author_list),
    section("year_english_reference1", as.character(meta$year)),
    section("year_english_reference", as.character(meta$year)),
    section("english_title", meta$english_title),
    section("french_authors_list", meta$french_author_list),
    section("year_french_reference1", as.character(meta$year)),
    section("year_french_reference", as.character(meta$year)),
    section("french_title", meta$french_title),
    section("abstract", abstract_lines)
  )

  writeLines(lines, path)

  invisible(path)
}

render_generated_frontmatter_docx <- function(generated_rmd, output_docx) {
  reference_doc <- system.file("csas-docx", "resdoc-content-2026.docx", package = "csasdown")

  rmarkdown::pandoc_convert(
    input = generated_rmd,
    to = "docx",
    output = output_docx,
    options = c(sprintf("--reference-doc=%s", reference_doc))
  )

  invisible(output_docx)
}

extract_abstract_paragraphs <- function(abstract_lines) {
  lines <- trimws(abstract_lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return(character())
  }

  paste(lines, collapse = " ")
}

#' Build, assemble, and merge resdoc frontmatter
#'
#' @param index_fn Index Rmd file.
#' @param yaml_fn _bookdown.yml file.
#' @param verbose Verbose output.
#' @param ... Arguments passed to [bookdown::render_book()].
#'
#' @noRd
build_resdoc_frontmatter_and_merge <- function(index_fn = "index.Rmd", yaml_fn = "_bookdown.yml", verbose = FALSE, ...) {
  if (verbose) cli::cli_inform("Building Research Document frontmatter...")

  meta <- rmarkdown::yaml_front_matter(index_fn)
  first_content_file <- get_first_resdoc_content_file(yaml_fn)

  manifest <- list()
  on.exit(restore_resdoc_sources(manifest), add = TRUE)

  content_lines <- readLines(first_content_file, warn = FALSE)
  abstract_result <- extract_and_strip_abstract(content_lines)

  backup_file <- tempfile(fileext = ".Rmd")
  file.copy(first_content_file, backup_file, overwrite = TRUE)
  manifest[[first_content_file]] <- backup_file
  writeLines(abstract_result$content_lines, first_content_file)

  generated_frontmatter_rmd <- "tmp-frontmatter-generated.Rmd"
  rendered_frontmatter_docx <- "tmp-frontmatter-rendered.docx"

  cleanup_files <- c(generated_frontmatter_rmd, rendered_frontmatter_docx, "tmp-frontmatter.docx", "tmp-content.docx")
  on.exit(unlink(cleanup_files[file.exists(cleanup_files)], force = TRUE), add = TRUE)

  cli::cli_inform("Stage 1/4: generating marker-wrapped frontmatter Rmd")
  build_frontmatter_generated_rmd(meta, abstract_result$abstract_lines, generated_frontmatter_rmd)

  cli::cli_inform("Stage 2/4: rendering frontmatter source with pandoc")
  render_generated_frontmatter_docx(generated_frontmatter_rmd, rendered_frontmatter_docx)

  output_options <- list(pandoc_args = c("--metadata=title:", "--metadata=abstract:"))

  cli::cli_inform("Stage 3/4: rendering main content with bookdown")
  bookdown::render_book(
    index_fn,
    config_file = yaml_fn,
    envir = parent.frame(n = 2L),
    output_options = output_options,
    ...
  )

  book_filename <- paste0(get_book_filename(yaml_fn), ".docx")
  output_docx <- file.path("_book", book_filename)
  file.rename(book_filename, output_docx)

  french <- isTRUE(meta$output[[1]]$french)
  front_template <- if (french) "resdoc-frontmatter-french2.docx" else "resdoc-frontmatter-english2.docx"
  abstract_keyword <- if (french) "R\u00c9SUM\u00c9" else "ABSTRACT"

  cli::cli_inform("Stage 4/4: assembling frontmatter and merging final document")
  frontmatter <- officer::read_docx(system.file("csas-docx", front_template, package = "csasdown")) |>
    officer::cursor_end() |>
    officer::body_add_docx(rendered_frontmatter_docx, pos = "on")

  frontmatter <- move_text_blocks(
    doc = frontmatter,
    source_docx = rendered_frontmatter_docx,
    tag_map = RESDOC_FRONTMATTER_TAG_MAP
  )

  frontmatter <- remove_marker_sections(
    doc = frontmatter,
    tags = c(unname(RESDOC_FRONTMATTER_TAG_MAP), "abstract")
  )

  abstract_paragraph <- extract_abstract_paragraphs(abstract_result$abstract_lines)
  if (length(abstract_paragraph)) {
    frontmatter <- frontmatter |>
      officer::cursor_reach(keyword = abstract_keyword) |>
      officer::body_add_par(abstract_paragraph, style = "Body Text")
  }

  frontmatter <- frontmatter |>
    officer::headers_replace_text_at_bkm("region", if (french) meta$french_region else meta$english_region) |>
    officer::headers_replace_text_at_bkm("year", as.character(meta$year)) |>
    officer::cursor_reach(keyword = if (french) "TABLE DES MATI\u00c8RES" else "TABLE OF CONTENTS") |>
    officer::body_add_toc()

  print(frontmatter, target = "tmp-frontmatter.docx")

  content <- officer::read_docx(output_docx) |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)
  print(content, target = "tmp-content.docx")

  full_doc <- officer::read_docx("tmp-frontmatter.docx") |>
    officer::cursor_end() |>
    officer::body_add_docx("tmp-content.docx", pos = "on") |>
    officer::docx_set_settings(even_and_odd_headers = FALSE)

  print(full_doc, target = output_docx)
  fix_table_caption_alignment(output_docx, reference_docx = "resdoc-content-2026.docx")

  invisible(output_docx)
}
