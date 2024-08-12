-- BASE REFERENCE: https://alexplescan.com/posts/2024/08/10/wezterm/

-- Import the wezterm module and others
local wezterm = require 'wezterm'
local appearance = require 'appearance'

-- Creates a config object which we will be adding our config to
local config = wezterm.config_builder()

-- Use the is_dark function to determine which color scheme to use 
config.color_scheme = 'Everforest Dark (Gogh)'
-- if appearance.is_dark() then
-- else
  -- config.color_scheme = 'Everforest Light (Gogh)'
-- end

-- Fonts etc
config.font = wezterm.font({ family = "JetBrains Mono" })
config.font_size = 13

-- Slightly transparent and blurred background
config.window_background_opacity = 0.9
config.macos_window_background_blur = 30
-- Removes the title bar, leaving only the tab bar. Keeps
-- the ability to resize by dragging the window's edges.
-- On macOS, 'RESIZE|INTEGRATED_BUTTONS' also looks nice if
-- you want to keep the window controls visible and integrate
-- them into the tab bar.
config.window_decorations = 'RESIZE'
-- Sets the font for the window frame (tab bar)
config.window_frame = {
  -- Berkeley Mono for me again, though an idea could be to try a
  -- serif font here instead of monospace for a nicer look?
  font = wezterm.font({ family = 'JetBrains Mono', weight = 'Bold' }),
  font_size = 11,
}

-- "Now, let’s do something a little kitsch. See that empty space to the right of our terminal’s tab bar? 
-- Let’s fill it with a powerline looking status bar. We’ll add an update-status callback:"
local function segments_for_right_status(window)
  return {
    window:active_workspace(),
    wezterm.strftime('%a %b %-d %H:%M'),
    wezterm.hostname(),
  }
end

wezterm.on('update-status', function(window, _)
  local SOLID_LEFT_ARROW = utf8.char(0xe0b2)
  local segments = segments_for_right_status(window)

  local color_scheme = window:effective_config().resolved_palette
  -- Note the use of wezterm.color.parse here, this returns
  -- a Color object, which comes with functionality for lightening
  -- or darkening the colour (amongst other things).
  local bg = wezterm.color.parse(color_scheme.background)
  local fg = color_scheme.foreground

  -- Each powerline segment is going to be coloured progressively
  -- darker/lighter depending on whether we're on a dark/light colour
  -- scheme. Let's establish the "from" and "to" bounds of our gradient.
  local gradient_to, gradient_from = bg
  if appearance.is_dark() then
    gradient_from = gradient_to:lighten(0.2)
  else
    gradient_from = gradient_to:darken(0.2)
  end

  -- Yes, WezTerm supports creating gradients, because why not?! Although
  -- they'd usually be used for setting high fidelity gradients on your terminal's
  -- background, we'll use them here to give us a sample of the powerline segment
  -- colours we need.
  local gradient = wezterm.color.gradient(
    {
      orientation = 'Horizontal',
      colors = { gradient_from, gradient_to },
    },
    #segments -- only gives us as many colours as we have segments.
  )

  -- We'll build up the elements to send to wezterm.format in this table.
  local elements = {}

  for i, seg in ipairs(segments) do
    local is_first = i == 1

    if is_first then
      table.insert(elements, { Background = { Color = 'none' } })
    end
    table.insert(elements, { Foreground = { Color = gradient[i] } })
    table.insert(elements, { Text = SOLID_LEFT_ARROW })

    table.insert(elements, { Foreground = { Color = fg } })
    table.insert(elements, { Background = { Color = gradient[i] } })
    table.insert(elements, { Text = ' ' .. seg .. ' ' })
  end

  window:set_right_status(wezterm.format(elements))
end)

-- Returns our config to be evaluated. We must always do this at the bottom of this file
return config
