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

test_that("frontmatter Lua filter wraps expected frontmatter keys with deterministic markers", {
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
    "year: 2026",
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

  keys <- c(
    "title",
    "authors",
    "address",
    "english_authors_list",
    "year_english_reference1",
    "year_english_reference",
    "english_title",
    "french_authors_list",
    "year_french_reference1",
    "year_french_reference",
    "french_title"
  )

  for (key in keys) {
    expect_match(out, paste0("START:", key), fixed = TRUE)
    expect_match(out, paste0("END:", key), fixed = TRUE)
  }

  expect_match(out, "Population trends", fixed = TRUE)
  expect_match(out, "Alice A.", fixed = TRUE)
  expect_match(out, "Pacific Region", fixed = TRUE)
  expect_match(out, "2026", fixed = TRUE)

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
