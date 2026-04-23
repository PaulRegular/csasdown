test_that("inject_resdoc_frontmatter_text adds tagged frontmatter and tags abstract", {
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

  original <- c(
    "# ABSTRACT",
    "Paragraph one.",
    "Paragraph two.",
    "",
    "# INTRODUCTION",
    "Main text."
  )
  writeLines(original, "01-main.Rmd")

  state <- inject_resdoc_frontmatter_text("index.Rmd", "_bookdown.yml")
  updated <- readLines("01-main.Rmd", warn = FALSE)

  expect_identical(state$file, "01-main.Rmd")
  expect_match(paste(updated[1:4], collapse = "\n"), "START:title\nEnglish Title\nEND:title", perl = TRUE)
  expect_true(any(updated == "START:authors"))
  expect_true(any(updated == "START:abstract"))
  expect_true(any(updated == "END:abstract"))
  expect_true(any(updated == "# ABSTRACT"))
  expect_true(any(updated == "Paragraph one."))
  expect_true(any(updated == "# INTRODUCTION"))
})

test_that("inject_resdoc_frontmatter_text skips index.Rmd when selecting injection file", {
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

  state <- inject_resdoc_frontmatter_text("index.Rmd", "_bookdown.yml")

  expect_identical(state$file, "01-main.Rmd")
  expect_identical(readLines("index.Rmd", warn = FALSE), index_original)
  expect_true(any(readLines("01-main.Rmd", warn = FALSE) == "START:title"))
})

test_that("restore_injected_resdoc_frontmatter_text restores original file content", {
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

  state <- inject_resdoc_frontmatter_text("index.Rmd", "_bookdown.yml")
  restore_injected_resdoc_frontmatter_text(state)

  expect_identical(readLines("01-main.Rmd", warn = FALSE), original)
})
