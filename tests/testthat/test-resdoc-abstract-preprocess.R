test_that("preprocess_resdoc_abstract extracts abstract and removes it from source", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)

  path <- tempfile("resdoc_preprocess_")
  dir.create(path)
  setwd(path)

  writeLines(c(
    'book_filename: "resdoc"',
    'rmd_files: ["index.Rmd", "01-intro.Rmd"]'
  ), "_bookdown.yml")
  writeLines("---\ntitle: test\n---", "index.Rmd")
  writeLines(c(
    "# Abstract",
    "",
    "First abstract paragraph.",
    "",
    "Second abstract paragraph.",
    "",
    "# Introduction",
    "",
    "Main body."
  ), "01-intro.Rmd")

  state <- preprocess_resdoc_abstract("_bookdown.yml")

  expect_equal(state$source_file, "01-intro.Rmd")
  expect_match(paste(readLines("tmp-abstract.Rmd"), collapse = "\n"), "# Abstract", fixed = TRUE)
  expect_match(paste(readLines("tmp-abstract.Rmd"), collapse = "\n"), "Second abstract paragraph.", fixed = TRUE)
  expect_false(any(grepl("# Abstract", readLines("01-intro.Rmd"), fixed = TRUE)))
  expect_true(any(grepl("# Introduction", readLines("01-intro.Rmd"), fixed = TRUE)))
})

test_that("preprocess_resdoc_abstract returns NULL with fewer than two headings", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)

  path <- tempfile("resdoc_preprocess_no_abstract_")
  dir.create(path)
  setwd(path)

  writeLines(c(
    'book_filename: "resdoc"',
    'rmd_files: ["index.Rmd", "01-intro.Rmd"]'
  ), "_bookdown.yml")
  writeLines("---\ntitle: test\n---", "index.Rmd")
  writeLines(c(
    "# Introduction",
    "",
    "Main body only."
  ), "01-intro.Rmd")

  state <- preprocess_resdoc_abstract("_bookdown.yml")
  expect_null(state)
  expect_false(file.exists("tmp-abstract.Rmd"))
})
