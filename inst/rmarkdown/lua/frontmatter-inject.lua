local START_PREFIX = "[[CSAS-FM-START:"
local END_PREFIX = "[[CSAS-FM-END:"

local STYLE_MAP = {
  title = "Cover: Document title",
  authors = "Cover: Author",
  address = "Cover: Address",
  citations = "citation",
  abstract = "Body Text"
}

local function marker_block(kind, phase)
  local prefix = START_PREFIX
  if phase == "end" then
    prefix = END_PREFIX
  end
  return pandoc.Para({ pandoc.Str(prefix .. kind .. "]]" ) })
end

local function meta_to_markdown(value)
  if value == nil then
    return ""
  end

  local t = pandoc.utils.type(value)

  if t == "Inlines" then
    return pandoc.write(pandoc.Pandoc({ pandoc.Para(value) }), "markdown")
  end

  if t == "Blocks" then
    return pandoc.write(pandoc.Pandoc(value), "markdown")
  end

  if t == "MetaInlines" then
    return pandoc.write(pandoc.Pandoc({ pandoc.Para(value) }), "markdown")
  end

  if t == "MetaBlocks" then
    return pandoc.write(pandoc.Pandoc(value), "markdown")
  end

  if t == "MetaString" then
    return tostring(value)
  end

  if t == "MetaBool" then
    return tostring(value)
  end

  if t == "MetaList" then
    local parts = {}
    for _, item in ipairs(value) do
      table.insert(parts, meta_to_markdown(item))
    end
    return table.concat(parts, "\\\\\n")
  end

  return pandoc.utils.stringify(value)
end

local function styled_block(kind, markdown)
  if markdown == nil or markdown == "" then
    return {}
  end

  local parsed = pandoc.read(markdown, "markdown")
  local style = STYLE_MAP[kind]
  local out = { marker_block(kind, "start") }

  if #parsed.blocks > 0 then
    table.insert(out, pandoc.Div(parsed.blocks, pandoc.Attr("", {}, { ["custom-style"] = style })))
  end

  table.insert(out, marker_block(kind, "end"))
  return out
end

function Pandoc(doc)
  local meta = doc.meta
  local authors_value = meta.authors
  if authors_value == nil then
    authors_value = meta.author
  end

  local fields = {
    { key = "title", value = meta.title },
    { key = "authors", value = authors_value },
    { key = "address", value = meta.address },
    { key = "citations", value = meta.citations },
    { key = "abstract", value = meta.abstract }
  }

  local injected = {}
  for _, field in ipairs(fields) do
    local markdown = meta_to_markdown(field.value)
    local blocks = styled_block(field.key, markdown)
    for _, block in ipairs(blocks) do
      table.insert(injected, block)
    end
  end

  if #injected == 0 then
    return doc
  end

  local merged = {}
  for _, block in ipairs(injected) do
    table.insert(merged, block)
  end
  for _, block in ipairs(doc.blocks) do
    table.insert(merged, block)
  end

  return pandoc.Pandoc(merged, doc.meta)
end
