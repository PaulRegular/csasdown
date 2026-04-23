local START_PREFIX = "START:"
local END_PREFIX = "END:"

local FRONTMATTER_KEYS = {
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
    if meta[key] ~= nil then
      return meta[key]
    end
  end
  return nil
end

local function frontmatter_value(meta, key)
  if key == "title" then
    return first_non_nil(meta, { "title", "english_title", "french_title" })
  end

  if key == "authors" then
    return first_non_nil(meta, { "authors", "author" })
  end

  if key == "address" then
    return first_non_nil(meta, { "address", "english_address", "french_address" })
  end

  if key == "year_english_reference1" or key == "year_english_reference" or key == "year_french_reference1" or key == "year_french_reference" then
    return meta.year
  end

  return meta[key]
end

local function wrapped_blocks(key, value)
  local out = { marker_block(key, "start") }
  local markdown = meta_to_markdown(value)

  if markdown ~= "" then
    local parsed = pandoc.read(markdown, "markdown")
    for _, block in ipairs(parsed.blocks) do
      table.insert(out, block)
    end
  end

  table.insert(out, marker_block(key, "end"))
  return out
end

function Pandoc(doc)
  local injected = {}

  for _, key in ipairs(FRONTMATTER_KEYS) do
    local value = frontmatter_value(doc.meta, key)
    local blocks = wrapped_blocks(key, value)
    for _, block in ipairs(blocks) do
      table.insert(injected, block)
    end
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
