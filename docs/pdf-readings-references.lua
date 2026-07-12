function Pandoc(document)
  if FORMAT:match("latex") then
    table.insert(document.blocks, pandoc.Div({}, pandoc.Attr("refs")))
  end

  return document
end
