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

  expect_match(out, "\\[\\[CSAS-FM-START:title\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-END:title\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-START:authors\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-END:authors\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-START:address\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-END:address\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-START:citations\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-END:citations\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-START:abstract\\]\\]")
  expect_match(out, "\\[\\[CSAS-FM-END:abstract\\]\\]")

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
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: true",
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

test_that("resdoc output format wires frontmatter injection filter only for resdoc", {
  resdoc_format <- resdoc_docx()
  fsar_format <- fsar_docx()

  resdoc_args <- resdoc_format$pandoc$args
  fsar_args <- fsar_format$pandoc$args

  expect_true(any(grepl("frontmatter-inject\\.lua$", resdoc_args)))
  expect_false(any(grepl("frontmatter-inject\\.lua$", fsar_args)))
})
