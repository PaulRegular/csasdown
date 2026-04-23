local START_PREFIX = "START:"
local END_PREFIX = "END:"

local STYLE_MAP_EN = {
  title = "Cover: Document title",
  authors = "Cover: Author",
  address = "Cover: Address",
  citations = "citation",
  abstract = "Body Text"
}

local STYLE_MAP_FR = {
  title = "Couverture : titre du document",
  authors = "Couverture : auteurs",
  address = "Couverture : adresse",
  citations = "citation",
  abstract = "Body Text"
}

local function marker_block(kind, phase)
  local prefix = START_PREFIX
  if phase == "end" then
    prefix = END_PREFIX
  end
  return pandoc.Para({ pandoc.Str(prefix .. kind) })
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

local function first_non_nil(meta, keys)
  for _, key in ipairs(keys) do
    local value = meta[key]
    if value ~= nil then
      return value
    end
  end
  return nil
end

local function meta_bool(value)
  if value == nil then
    return false
  end

  local t = pandoc.utils.type(value)
  if t == "MetaBool" then
    return value
  end

  if t == "MetaString" then
    local lowered = tostring(value):lower()
    return lowered == "true"
  end

  return false
end

local function is_french(meta)
  if meta_bool(meta.french) then
    return true
  end

  local output = meta.output
  if output == nil or pandoc.utils.type(output) ~= "MetaMap" then
    return false
  end

  local output_map = output
  for key, value in pairs(output_map) do
    if tostring(key):find("resdoc_docx", 1, true) and pandoc.utils.type(value) == "MetaMap" then
      if meta_bool(value.french) then
        return true
      end
    end
  end

  return false
end

local function styled_block(kind, markdown, style_map)
  if markdown == nil or markdown == "" then
    return {}
  end

  local parsed = pandoc.read(markdown, "markdown")
  local style = style_map[kind]
  local out = { marker_block(kind, "start") }

  if #parsed.blocks > 0 then
    table.insert(out, pandoc.Div(parsed.blocks, pandoc.Attr("", {}, { ["custom-style"] = style })))
  end

  table.insert(out, marker_block(kind, "end"))
  return out
end

function Pandoc(doc)
  local meta = doc.meta
  local french = is_french(meta)
  local style_map = french and STYLE_MAP_FR or STYLE_MAP_EN
  local authors_value = first_non_nil(meta, { "authors", "author" })
  local title_keys = french and { "french_title", "title", "english_title" } or { "english_title", "title", "french_title" }
  local address_keys = french and { "french_address", "address", "english_address" } or { "english_address", "address", "french_address" }
  local citation_keys = french and { "french_citations", "citations", "english_citations" } or { "english_citations", "citations", "french_citations" }
  local abstract_keys = french and { "french_abstract", "abstract", "english_abstract" } or { "english_abstract", "abstract", "french_abstract" }

  local fields = {
    { key = "title", value = first_non_nil(meta, title_keys) },
    { key = "authors", value = authors_value },
    { key = "address", value = first_non_nil(meta, address_keys) },
    { key = "citations", value = first_non_nil(meta, citation_keys) },
    { key = "abstract", value = first_non_nil(meta, abstract_keys) }
  }

  local injected = {}
  for _, field in ipairs(fields) do
    local markdown = meta_to_markdown(field.value)
    local blocks = styled_block(field.key, markdown, style_map)
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
