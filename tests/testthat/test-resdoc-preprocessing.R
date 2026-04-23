test_that("prepare_resdoc_frontmatter_inputs writes tmp-frontmatter and strips abstract", {
  wd <- getwd()
  path <- file.path(tempdir(), "resdoc_preprocess_test")
  unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  setwd(path)
  on.exit(setwd(wd), add = TRUE)

  writeLines(c(
    'book_filename: "resdoc"',
    'rmd_files: ["01-main.Rmd"]'
  ), "_bookdown.yml")

  writeLines(c(
    "---",
    "english_title: English Title",
    "french_title: Titre français",
    "author: First A. Last and Second B. Person",
    "english_address: English Address",
    "french_address: Adresse française",
    "year: 2026",
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: false",
    "---"
  ), "index.Rmd")

  original <- c("# ABSTRACT", "Paragraph one.", "Paragraph two.", "", "# INTRODUCTION", "Main text.")
  writeLines(original, "01-main.Rmd")

  state <- prepare_resdoc_frontmatter_inputs("index.Rmd", "_bookdown.yml")
  frontmatter <- readLines(state$tmp_frontmatter_md, warn = FALSE)
  updated <- readLines("01-main.Rmd", warn = FALSE)

  expect_true(any(frontmatter == "START:title"))
  expect_true(any(frontmatter == "END:title"))
  expect_true(any(frontmatter == "START:abstract"))
  expect_true(any(frontmatter == "Paragraph one."))
  expect_false(any(updated == "# ABSTRACT"))
  expect_false(any(updated == "Paragraph one."))
  expect_true(any(updated == "# INTRODUCTION"))
})

test_that("prepare_resdoc_frontmatter_inputs skips index.Rmd when selecting content file", {
  wd <- getwd()
  path <- file.path(tempdir(), "resdoc_preprocess_index_target_test")
  unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  setwd(path)
  on.exit(setwd(wd), add = TRUE)

  writeLines(c(
    'book_filename: "resdoc"',
    'rmd_files: ["index.Rmd", "01-main.Rmd"]'
  ), "_bookdown.yml")

  index_original <- c(
    "---",
    "english_title: English Title",
    "french_title: Titre français",
    "author: First A. Last and Second B. Person",
    "english_address: English Address",
    "french_address: Adresse française",
    "year: 2026",
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: false",
    "---"
  )
  writeLines(index_original, "index.Rmd")
  writeLines(c("# ABSTRACT", "Abstract body.", "# INTRODUCTION", "Body."), "01-main.Rmd")

  state <- prepare_resdoc_frontmatter_inputs("index.Rmd", "_bookdown.yml")

  expect_identical(state$file, "01-main.Rmd")
  expect_identical(readLines("index.Rmd", warn = FALSE), index_original)
})

test_that("restore_resdoc_frontmatter_inputs restores content and removes temp frontmatter file", {
  wd <- getwd()
  path <- file.path(tempdir(), "resdoc_preprocess_restore_test")
  unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  setwd(path)
  on.exit(setwd(wd), add = TRUE)

  writeLines(c(
    'book_filename: "resdoc"',
    'rmd_files: ["01-main.Rmd"]'
  ), "_bookdown.yml")

  writeLines(c(
    "---",
    "english_title: English Title",
    "french_title: Titre français",
    "author: First A. Last and Second B. Person",
    "english_address: English Address",
    "french_address: Adresse française",
    "year: 2026",
    "output:",
    "  csasdown::resdoc_docx:",
    "    french: false",
    "---"
  ), "index.Rmd")

  original <- c("# ABSTRACT", "Some abstract text.", "# INTRODUCTION", "Body.")
  writeLines(original, "01-main.Rmd")

  state <- prepare_resdoc_frontmatter_inputs("index.Rmd", "_bookdown.yml")
  expect_true(file.exists(state$tmp_frontmatter_md))
  restore_resdoc_frontmatter_inputs(state)

  expect_identical(readLines("01-main.Rmd", warn = FALSE), original)
  expect_false(file.exists(state$tmp_frontmatter_md))
})
