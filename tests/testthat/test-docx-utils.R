count_fixed_matches <- function(x, pattern) {
  matches <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (identical(matches[1], -1L)) {
    return(0L)
  }
  length(matches)
}

test_that("fix_table_cell_styles_xml applies expected table cell styles", {
  xml <- paste0(
    '<w:document><w:body><w:tbl>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr><w:sz w:val="20"/><w:szCs w:val="20"/></w:pPr>',
    '<w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/>',
    '</w:rPr><w:t>Header</w:t></w:r></w:p></w:tc></w:tr>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr><w:sz w:val="20"/><w:szCs w:val="20"/></w:pPr>',
    '<w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" w:eastAsia="Helvetica" w:cs="Helvetica"/>',
    '</w:rPr><w:t>Body</w:t></w:r></w:p></w:tc></w:tr>',
    '</w:tbl></w:body></w:document>'
  )

  out <- csasdown:::fix_table_cell_styles_xml(xml)
  rows <- regmatches(out, gregexpr("<w:tr[^>]*>.*?</w:tr>", out, perl = TRUE))[[1]]

  expect_length(rows, 2L)
  expect_true(grepl('<w:pStyle w:val="Caption-Table"/>', rows[1], fixed = TRUE))
  expect_true(grepl('<w:pStyle w:val="BodyText"/>', rows[2], fixed = TRUE))
  expect_false(grepl('<w:sz w:val="20"/>', rows[1], fixed = TRUE))
  expect_false(grepl('<w:szCs w:val="20"/>', rows[1], fixed = TRUE))
  expect_true(grepl('<w:sz w:val="20"/>', rows[2], fixed = TRUE))
  expect_true(grepl('<w:szCs w:val="20"/>', rows[2], fixed = TRUE))
  expect_false(grepl('<w:rFonts w:ascii="Helvetica"', out, fixed = TRUE))
  expect_equal(count_fixed_matches(out, "<w:rFonts/>"), 2L)
  expect_false(grepl("\\\\1<w:pStyle", out, fixed = TRUE))
})

test_that("fix_table_cell_styles_xml is idempotent and de-duplicates styles", {
  xml <- paste0(
    '<w:document><w:body><w:tbl>',
    '<w:tr><w:tc><w:tcPr></w:tcPr><w:p><w:pPr>',
    '<w:pStyle w:val="Caption-Table"/><w:pStyle w:val="Caption-Table"/>',
    '</w:pPr><w:r><w:rPr><w:rFonts w:ascii="Helvetica" w:hAnsi="Helvetica" ',
    'w:eastAsia="Helvetica" w:cs="Helvetica"/></w:rPr><w:t>Header</w:t></w:r></w:p></w:tc></w:tr>',
    '</w:tbl></w:body></w:document>'
  )

  once <- csasdown:::fix_table_cell_styles_xml(xml)
  twice <- csasdown:::fix_table_cell_styles_xml(once)

  expect_identical(once, twice)
  expect_equal(count_fixed_matches(once, '<w:pStyle w:val="Caption-Table"/>'), 1L)
})

read_document_xml <- function(path) {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = tmp)
  paste(readLines(file.path(tmp, "word", "document.xml"), warn = FALSE), collapse = "")
}

test_that("move_text moves multi-paragraph content within a document", {
  docx <- tempfile(fileext = ".docx")

  doc <- officer::read_docx() |>
    officer::body_add_par("Placeholder", style = "heading 1") |>
    officer::cursor_backward() |>
    officer::body_bookmark("title") |>
    officer::cursor_end() |>
    officer::body_add_par("before", style = "Normal") |>
    officer::body_add_par("START:title", style = "Normal") |>
    officer::body_add_par("First moved paragraph", style = "Normal") |>
    officer::body_add_par("Second moved paragraph", style = "Normal") |>
    officer::body_add_par("END:title", style = "Normal") |>
    officer::body_add_par("after", style = "Normal")
  print(doc, target = docx)

  csasdown:::move_text(docx, c(title = "title"))

  doc_xml <- read_document_xml(docx)

  expect_false(grepl("START:title", doc_xml, fixed = TRUE))
  expect_false(grepl("END:title", doc_xml, fixed = TRUE))
  expect_false(grepl("Placeholder", doc_xml, fixed = TRUE))
  expect_true(grepl("First moved paragraph", doc_xml, fixed = TRUE))
  expect_true(grepl("Second moved paragraph", doc_xml, fixed = TRUE))
  expect_true(grepl("before", doc_xml, fixed = TRUE))
  expect_true(grepl("after", doc_xml, fixed = TRUE))
  expect_true(grepl('<w:pStyle w:val="heading1"/>', doc_xml, fixed = TRUE))
})

test_that("move_text errors when markers are missing", {
  docx <- tempfile(fileext = ".docx")

  doc <- officer::read_docx() |>
    officer::body_add_par("Placeholder", style = "Normal") |>
    officer::cursor_backward() |>
    officer::body_bookmark("title") |>
    officer::cursor_end() |>
    officer::body_add_par("START:title", style = "Normal") |>
    officer::body_add_par("No end marker", style = "Normal")
  print(doc, target = docx)

  expect_error(
    csasdown:::move_text(docx, c(title = "title")),
    "Could not find both START:title and END:title markers."
  )
})
