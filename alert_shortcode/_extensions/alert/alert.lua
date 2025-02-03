return {

 ['alert'] = function(args, kwargs, meta, raw_args, context)
    -- Parse the argument (alert name)
    local alertName = pandoc.utils.stringify(args[1])

    -- Fetch alerts_list from metadata
    local alerts_list = meta["alerts_list"]

    -- Ensure alerts_list exists and contains the specified alert
    if not alerts_list then
      return pandoc.Str("Error: alerts_list not found in metadata.")
    end

    local alertData = alerts_list[alertName]
    if not alertData then
      return pandoc.Str("Error: Alert '" .. alertName .. "' not found.")
    end

    -- Extract values safely
    local title = pandoc.utils.stringify(alertData["title"] or "Untitled")
    local type = pandoc.utils.stringify(alertData["type"] or "note")
    local content = pandoc.utils.stringify(alertData["content"] or "No content provided.")
    local showIcon = alertData["icon"] ~= false  -- Defaults to true unless explicitly set to false
    local collapsible = alertData["collapse"] == true  -- Defaults to false unless explicitly set to true
    local alertId = type .. "-" .. alertName  -- Construct the ID
    
        print("Alert ID: " .. alertId)  -- Debugging alert ID

    -- Handle context: block, inline, or text
    if context == "block" then
      -- In block context, create a full callout with ID
      local calloutDiv = {
        type = type,
        icon = showIcon,
        title = title,
        content = pandoc.Para({pandoc.Str(content)}),
        collapse = collapsible,
        attr = pandoc.Attr("", "", {["id"] = alertId})
      }

      
      return quarto.Callout(calloutDiv)
      
      
    elseif context == "inline" then
      -- In inline context, create a cross-reference with quarto-xref class
      local xref = pandoc.Link("@" .. alertId, "#" .. alertId)
      xref.attributes["class"] = "quarto-xref"  -- Add the quarto-xref class
      return xref
    elseif context == "text" then
      -- In text context, just return the ID within some context (e.g., code block)
      return pandoc.Str("Alert ID: " .. alertId)
    end
    
  end,



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
