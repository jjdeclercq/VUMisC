-- alerts.lua
return {
  ['alert'] = function(args, kwargs, meta)
    -- Parse the argument (alert name)
    local alertName = args[1]

    -- Load metadata from _alerts.yml
    local alerts = meta["alerts"] or {}

    -- Fetch alert data from metadata
    local alertData = alerts[alertName]
    if not alertData then
      return pandoc.Str("Alert not found: " .. alertName)
    end

    -- Extract values (fallback to default if not present in metadata)
    local title = alertData["title"] or "Untitled"
    local type = alertData["type"] or "note"
    local content = alertData["content"] or "No content provided."

    -- Create the callout structure
    local calloutDiv = {}
    calloutDiv["type"] = type
    calloutDiv["icon"] = false  -- Customize as needed (false hides icon)
    calloutDiv["title"] = title
    calloutDiv["content"] = pandoc.Str(content)

    -- Return the callout output
    return quarto.Callout(calloutDiv)
  end
}
