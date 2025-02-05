return {
  ['alert'] = function(args, kwargs, meta)
    -- Parse the argument (alert name)
    local alertName = pandoc.utils.stringify(args[1])
    
    -- Fetch alerts_list from metadata
    local alerts_list = meta["alerts_list"]
    if not alerts_list then
      return pandoc.Str("Error: alerts_list not found in metadata.")
    end

    local alertData = alerts_list[alertName]
    if not alertData then
      return pandoc.Str("Error: Alert '" .. alertName .. "' not found.")
    end

    -- Basic alert fields
    local title   = pandoc.utils.stringify(alertData["title"] or "Untitled")
    local type    = pandoc.utils.stringify(alertData["type"] or "note")
    local content = pandoc.utils.stringify(alertData["content"] or "No content provided.")
    local showIcon     = alertData["icon"] ~= false         -- defaults to true
    local collapsible  = alertData["collapse"] == true        -- defaults to false

    -- New extra fields
    local date_created  = pandoc.utils.stringify(alertData["date_created"] or "")
    local resolved      = pandoc.utils.stringify(alertData["resolved"] or false)
    local date_resolved = pandoc.utils.stringify(alertData["date_resolved"] or "")
    local resolution    = pandoc.utils.stringify(alertData["resolution"] or "")
    
    -- Build extra info (each item will be its own paragraph)
    local include_extras = pandoc.utils.stringify(alertData["include_extras"] or false)
    
    local extra_info = {}
    if include_extras:lower() == "yes" then
      if date_created ~= "" then
        table.insert(extra_info, "Date Created: " .. date_created)
      end
      if resolved:lower() == "yes" then
        table.insert(extra_info, "Resolved: Yes")
        if date_resolved ~= "" then
          table.insert(extra_info, "Date Resolved: " .. date_resolved)
        end
        if resolution ~= "" then
          table.insert(extra_info, "Resolution: " .. resolution)
        end
      else
        table.insert(extra_info, "Resolved: No")
      end
    end

    -- Convert each extra info item into a separate paragraph block
    local extra_blocks = {}
    for _, item in ipairs(extra_info) do
      table.insert(extra_blocks, pandoc.Para({pandoc.Str(item)}))
    end

    -- Assemble the main content block followed by the extra info blocks
    local blocks = {}
    table.insert(blocks, pandoc.Para({pandoc.Str(content)}))
    for _, para in ipairs(extra_blocks) do
      table.insert(blocks, para)
    end

    -- Create the callout structure (title, type, content, etc.)
    local calloutDiv = {
      type = type,
      icon = showIcon,
      title = title,
      content = blocks,
      collapse = collapsible
    }

    -- Return the rendered callout via Quarto's Callout function
    return quarto.Callout(calloutDiv)
  end


,



  -- Alert summary function
['alert_summary'] = function(args, kwargs, meta)
  local alerts_list = meta["alerts_list"]
  if not alerts_list then
    return pandoc.Str("Error: alerts_list not found in metadata.")
  end

  local alertNames = {}

  -- Extract alert names
  for key, _ in pairs(alerts_list) do
    table.insert(alertNames, key)
  end

  -- Sort alert names based on the number in their name
  table.sort(alertNames, function(a, b)
    local numA = tonumber(a:match("%d+")) or math.huge
    local numB = tonumber(b:match("%d+")) or math.huge
    return numA < numB
  end)

  return pandoc.Para({pandoc.Str(table.concat(alertNames, ", "))})
end
}
