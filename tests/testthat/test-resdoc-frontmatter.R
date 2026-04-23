test_that("resdoc frontmatter replaces header region and year bookmarks", {
  front_file <- system.file("csas-docx", "resdoc-frontmatter-english2.docx", package = "csasdown")

  frontmatter <- officer::read_docx(front_file) |>
    officer::headers_replace_text_at_bkm("region", "Test Region") |>
    officer::headers_replace_text_at_bkm("year", "2030")

  out <- tempfile(fileext = ".docx")
  print(frontmatter, target = out)

  xml_dir <- tempfile()
  dir.create(xml_dir)
  utils::unzip(out, exdir = xml_dir)
  header_xml <- paste(readLines(file.path(xml_dir, "word", "header1.xml"), warn = FALSE), collapse = "")

  expect_match(header_xml, "Test Region", fixed = TRUE)
  expect_match(header_xml, "2030", fixed = TRUE)
  expect_false(grepl("Name of the region", header_xml, fixed = TRUE))

  unlink(c(out, xml_dir), recursive = TRUE, force = TRUE)
})

test_that("frontmatter Lua filter injects English resdoc metadata and styles by default", {
  skip_on_cran()

  filter_path <- system.file("rmarkdown", "lua", "frontmatter-inject.lua", package = "csasdown")
  expect_true(file.exists(filter_path))

  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".md")

  writeLines(c(
    "---",
    "english_title: Population trends",
    "french_title: Tendances de la population",
    "authors:",
    "  - Alice A.^1^",
    "  - Bob B.^2^",
    "english_address: Pacific Region\\\\Fisheries and Oceans Canada",
    "french_address: Région du Pacifique\\\\Pêches et Océans Canada",
    "english_citations: \"DFO. *Population trends*. 2026.\"",
    "french_citations: \"MPO. *Tendances de la population*. 2026.\"",
    "english_abstract: Summary *text*.",
    "french_abstract: Résumé *texte*.",
    "---",
    "",
    "# Intro",
    "Body"
  ), input)

  rmarkdown::pandoc_convert(
    input = input,
    to = "markdown",
    from = "markdown",
    output = output,
    options = c("--lua-filter", filter_path)
  )

  out <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(out, "START:title", fixed = TRUE)
  expect_match(out, "END:title", fixed = TRUE)
  expect_match(out, "START:authors", fixed = TRUE)
  expect_match(out, "END:authors", fixed = TRUE)
  expect_match(out, "START:address", fixed = TRUE)
  expect_match(out, "END:address", fixed = TRUE)
  expect_match(out, "START:citations", fixed = TRUE)
  expect_match(out, "END:citations", fixed = TRUE)
  expect_match(out, "START:abstract", fixed = TRUE)
  expect_match(out, "END:abstract", fixed = TRUE)

  expect_match(out, 'custom-style="Cover: Document title"', fixed = TRUE)
  expect_match(out, 'custom-style="Cover: Author"', fixed = TRUE)
  expect_match(out, 'custom-style="Cover: Address"', fixed = TRUE)
  expect_match(out, 'custom-style="citation"', fixed = TRUE)
  expect_match(out, "Population trends", fixed = TRUE)
  expect_false(grepl("Tendances de la population", out, fixed = TRUE))
  expect_false(grepl("Région du Pacifique", out, fixed = TRUE))
  expect_false(grepl("Résumé", out, fixed = TRUE))

  unlink(c(input, output), force = TRUE)
})

test_that("frontmatter Lua filter injects French resdoc metadata and cover styles when french is true", {
  skip_on_cran()

  filter_path <- system.file("rmarkdown", "lua", "frontmatter-inject.lua", package = "csasdown")
  expect_true(file.exists(filter_path))

  input <- tempfile(fileext = ".md")
  output <- tempfile(fileext = ".md")

  writeLines(c(
    "---",
    "english_title: Population trends",
    "french_title: Tendances de la population",
    "author: Alice A.^1^",
    "english_address: Pacific Region\\\\Fisheries and Oceans Canada",
    "french_address: Région du Pacifique\\\\Pêches et Océans Canada",
    "english_citations: \"DFO. *Population trends*. 2026.\"",
    "french_citations: \"MPO. *Tendances de la population*. 2026.\"",
    "english_abstract: Summary *text*.",
    "french_abstract: Résumé *texte*.",
    "french: true",
    "---",
    "",
    "Body"
  ), input)

  rmarkdown::pandoc_convert(
    input = input,
    to = "markdown",
    from = "markdown",
    output = output,
    options = c("--lua-filter", filter_path)
  )

  out <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(out, 'custom-style="Couverture : titre du document"', fixed = TRUE)
  expect_match(out, 'custom-style="Couverture : auteurs"', fixed = TRUE)
  expect_match(out, 'custom-style="Couverture : adresse"', fixed = TRUE)
  expect_match(out, 'custom-style="citation"', fixed = TRUE)
  expect_match(out, 'custom-style="Body Text"', fixed = TRUE)

  expect_match(out, "Tendances de la population", fixed = TRUE)
  expect_match(out, "Région du Pacifique", fixed = TRUE)
  expect_match(out, "Résumé", fixed = TRUE)
  expect_false(grepl("Population trends", out, fixed = TRUE))
  expect_false(grepl("Pacific Region", out, fixed = TRUE))
  expect_false(grepl("Summary", out, fixed = TRUE))

  unlink(c(input, output), force = TRUE)
})

set_frontmatter_fields <- function(index_file = "index.Rmd") {
  lines <- readLines(index_file, warn = FALSE)
  insert_at <- which(grepl("^year:", lines))[1]
  extra <- c(
    "english_citations: \"DFO. 2026. Population trends.\"",
    "french_citations: \"MPO. 2026. Tendances de la population.\""
  )
  out <- append(lines, extra, after = insert_at)
  writeLines(out, index_file)
}

set_french <- function(index_file = "index.Rmd") {
  lines <- readLines(index_file, warn = FALSE)
  lines <- gsub("french: false", "french: true", lines, fixed = TRUE)
  writeLines(lines, index_file)
}

read_docx_document_xml <- function(docx_path) {
  xml_dir <- tempfile("docx-xml-")
  dir.create(xml_dir)
  utils::unzip(docx_path, exdir = xml_dir)
  on.exit(unlink(xml_dir, recursive = TRUE, force = TRUE), add = TRUE)
  paste(readLines(file.path(xml_dir, "word", "document.xml"), warn = FALSE), collapse = "")
}

test_that("resdoc content build injects frontmatter metadata (English)", {
  skip_on_cran()

  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc-core-content-en")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  on.exit(setwd(wd), add = TRUE)

  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))
  set_frontmatter_fields("index.Rmd")
  bookdown::render_book("index.Rmd", config_file = "_bookdown.yml")

  content_docx <- "_book/resdoc.docx"
  expect_true(file.exists(content_docx))
  xml <- read_docx_document_xml(content_docx)

  expect_match(xml, "START:title", fixed = TRUE)
  expect_match(xml, "START:authors", fixed = TRUE)
  expect_match(xml, "START:address", fixed = TRUE)
  expect_match(xml, "START:citations", fixed = TRUE)
  expect_match(xml, "Title Here", fixed = TRUE)
  expect_match(xml, "First M. Last", fixed = TRUE)
  expect_match(xml, "Pacific Biological Station", fixed = TRUE)
  expect_match(xml, "DFO. 2026. Population trends.", fixed = TRUE)
})

test_that("resdoc content build injects frontmatter metadata (French)", {
  skip_on_cran()

  wd <- getwd()
  testing_path <- file.path(tempdir(), "resdoc-core-content-fr")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  on.exit(setwd(wd), add = TRUE)

  suppressMessages(draft("resdoc", create_dir = FALSE, edit = FALSE))
  set_frontmatter_fields("index.Rmd")
  set_french("index.Rmd")
  bookdown::render_book("index.Rmd", config_file = "_bookdown.yml")

  content_docx <- "_book/resdoc.docx"
  expect_true(file.exists(content_docx))
  xml <- read_docx_document_xml(content_docx)

  expect_match(xml, "START:title", fixed = TRUE)
  expect_match(xml, "START:authors", fixed = TRUE)
  expect_match(xml, "START:address", fixed = TRUE)
  expect_match(xml, "START:citations", fixed = TRUE)
  expect_match(xml, "Titre ici", fixed = TRUE)
  expect_match(xml, "Station biologique du Pacifique", fixed = TRUE)
  expect_match(xml, "MPO. 2026. Tendances de la population.", fixed = TRUE)
})

test_that("resdoc output format wires frontmatter injection filter only for resdoc", {
  resdoc_format <- resdoc_docx()
  fsar_format <- fsar_docx()

  resdoc_args <- resdoc_format$pandoc$args
  fsar_args <- fsar_format$pandoc$args

  expect_true(any(grepl("frontmatter-inject\\.lua$", resdoc_args)))
  expect_false(any(grepl("frontmatter-inject\\.lua$", fsar_args)))
})
