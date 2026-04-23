test_that("extract_and_strip_abstract handles English and French headings", {
  english <- c(
    "# ABSTRACT",
    "English abstract line.",
    "",
    "# INTRODUCTION",
    "Body"
  )

  out_en <- extract_and_strip_abstract(english)
  expect_equal(out_en$abstract_lines, c("English abstract line.", ""))
  expect_false(any(grepl("# ABSTRACT", out_en$content_lines, fixed = TRUE)))
  expect_true(any(grepl("# INTRODUCTION", out_en$content_lines, fixed = TRUE)))

  french <- c(
    "# RÉSUMÉ",
    "Résumé en français.",
    "# INTRODUCTION",
    "Body"
  )

  out_fr <- extract_and_strip_abstract(french)
  expect_equal(out_fr$abstract_lines, "Résumé en français.")
  expect_false(any(grepl("# RÉSUMÉ", out_fr$content_lines, fixed = TRUE)))
})

test_that("build_frontmatter_generated_rmd writes marker wrapped sections", {
  tmp <- tempfile(fileext = ".Rmd")
  meta <- list(
    author = "First M. Last and Alex B. Smith",
    output = list(list(french = FALSE)),
    english_title = "English title",
    french_title = "Titre français",
    english_address = "English address",
    french_address = "Adresse française",
    year = 2031
  )

  build_frontmatter_generated_rmd(meta, c("Abstract line 1", "Abstract line 2"), tmp)
  txt <- readLines(tmp, warn = FALSE)

  expect_true(any(txt == "START:title"))
  expect_true(any(txt == "END:title"))
  expect_true(any(txt == "START:abstract"))
  expect_true(any(txt == "END:abstract"))
  expect_true(any(grepl("English title", txt, fixed = TRUE)))
})

test_that("move_text moves marker content and warns/errors on failures", {
  skip_on_cran()

  source_rmd <- tempfile(fileext = ".Rmd")
  source_docx <- tempfile(fileext = ".docx")

  writeLines(c(
    "START:title",
    "Population of *Sebastes alutus*",
    "END:title"
  ), source_rmd)

  render_generated_frontmatter_docx(source_rmd, source_docx)

  target <- officer::read_docx(system.file("csas-docx", "resdoc-frontmatter-english2.docx", package = "csasdown"))
  out <- move_text(target, source_docx, "title", "title")

  out_summary <- officer::docx_summary(out)
  expect_true(any(grepl("Population of Sebastes alutus", out_summary$text, fixed = TRUE)))

  expect_warning(
    move_text(target, source_docx, "not_a_bookmark", "title"),
    "Bookmark 'not_a_bookmark' not found"
  )

  expect_warning(
    move_text(target, source_docx, "title", "not_a_tag"),
    "Tag 'not_a_tag' not found"
  )

  dup_rmd <- tempfile(fileext = ".Rmd")
  dup_docx <- tempfile(fileext = ".docx")
  writeLines(c(
    "START:title",
    "one",
    "END:title",
    "START:title",
    "two",
    "END:title"
  ), dup_rmd)
  render_generated_frontmatter_docx(dup_rmd, dup_docx)

  expect_error(
    move_text(target, dup_docx, "title", "title"),
    "Duplicate tag block"
  )
})

test_that("resdoc source files are restored when orchestration errors", {
  skip_on_cran()

  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc_restore")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  on.exit(setwd(wd), add = TRUE)

  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))

  first_content <- get_first_resdoc_content_file("_bookdown.yml")
  original <- readLines(first_content, warn = FALSE)

  writeLines(c(original, "```{r}", "stop('forced error')", "```"), first_content)
  before <- readLines(first_content, warn = FALSE)

  expect_error(
    build_resdoc_frontmatter_and_merge(index_fn = "index.Rmd", yaml_fn = "_bookdown.yml"),
    "forced error"
  )

  after <- readLines(first_content, warn = FALSE)
  expect_equal(after, before)
})

test_that("resdoc end-to-end puts abstract in frontmatter only", {
  skip_on_cran()

  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc_abstract_flow")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  on.exit(setwd(wd), add = TRUE)

  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))
  render()

  expect_true(file.exists("_book/resdoc.docx"))

  xml_dir <- tempfile()
  dir.create(xml_dir)
  utils::unzip("_book/resdoc.docx", exdir = xml_dir)
  doc_xml <- paste(readLines(file.path(xml_dir, "word", "document.xml"), warn = FALSE), collapse = "")

  abstract_sentence <- "The Abstract is mandatory. This section will be posted in HTML format"
  hits <- gregexpr(abstract_sentence, doc_xml, fixed = TRUE)[[1]]
  expect_equal(sum(hits > 0), 1)

  unlink(xml_dir, recursive = TRUE, force = TRUE)
})
