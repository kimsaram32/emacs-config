local wezterm = require 'wezterm' --[[@as Wezterm]]
local act = wezterm.action
local config = wezterm.config_builder()

local function read_json(file)
  local f = assert(io.open(wezterm.home_dir .. '/me/dotfiles' .. file, 'rb'))
  local content = f:read '*all'
  f:close()
  return wezterm.json_parse(content)
end

-- Appearance
config.colors = read_json '/colors.json'
config.force_reverse_video_cursor = true
-- config.font = wezterm.font_with_fallback { 'IosevkaCustom Nerd Font', 'Sarasa Term K' }
config.font_size = 15
config.adjust_window_size_when_changing_font_size = false
config.front_end = 'WebGpu'
config.max_fps = 120
config.ssh_domains = {}
config.enable_tab_bar = false
config.window_close_confirmation = 'NeverPrompt'
config.window_decorations = 'RESIZE'
local padding = '4pt'
config.window_padding = { left = padding, right = padding, top = padding, bottom = 0 }
---@diagnostic disable-next-line: inject-field
config.window_content_alignment = { horizontal = 'Center', vertical = 'Center' }
wezterm.on('toggle-ligature', function(window)
  local overrides = window:get_config_overrides() or {}
  if not overrides.harfbuzz_features then
    overrides.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }
  else
    overrides.harfbuzz_features = nil
  end
  window:set_config_overrides(overrides)
end)
-- maximize window on startup
wezterm.on('gui-startup', function(cmd)
  local _, _, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

-- Key
config.disable_default_key_bindings = true
-- stylua: ignore
local function to_action(lhs, action) return { mods = lhs.mods, key = lhs.key, action = action } end
-- stylua: ignore
local function to_key(lhs, rhs) return to_action(lhs, act.SendKey(rhs)) end
-- stylua: ignore
local function tmux_key(key) return act.Multiple { act.SendKey { mods = 'CTRL', key = 'x' }, act.SendKey(key) } end
-- stylua: ignore
local function to_tmux(lhs, rhs) return to_action(lhs, tmux_key(rhs)) end

config.keys = {
  -- WezTerm
  to_action({ mods = 'CMD', key = 'q' }, act.QuitApplication),
  to_action({ mods = 'CMD', key = 'h' }, act.HideApplication),
  to_action({ mods = 'CMD|SHIFT', key = 'N' }, act.SpawnWindow),
  to_action({ mods = 'CMD', key = 'w' }, act.CloseCurrentTab { confirm = false }),
  to_action({ mods = 'CMD|SHIFT', key = '|' }, act.ShowDebugOverlay),
  to_action({ mods = 'CMD|CTRL', key = '`' }, act.ReloadConfiguration),
  to_action({ mods = 'CMD|CTRL', key = 'f' }, act.ToggleFullScreen),
  to_action({ mods = 'CMD', key = 'v' }, act.PasteFrom 'Clipboard'),
  to_action({ mods = 'CMD', key = '0' }, act.Multiple { act.ResetFontSize, act.EmitEvent 'window-resized' }),
  to_action({ mods = 'CMD', key = '-' }, act.Multiple { act.DecreaseFontSize, act.EmitEvent 'window-resized' }),
  to_action({ mods = 'CMD', key = '=' }, act.Multiple { act.IncreaseFontSize, act.EmitEvent 'window-resized' }),
  to_action({ mods = 'CMD', key = '\\' }, act.EmitEvent 'toggle-ligature'),

  -- shift enter
  to_key({ mods = 'SHIFT', key = 'Enter' }, { mods = 'ALT', key = 'm' }),
  to_key({ mods = 'CMD', key = 'Enter' }, { mods = 'CTRL|ALT', key = 'm' }),

  -- arrow
  to_key({ mods = 'OPT', key = 'LeftArrow' }, { mods = 'ALT', key = 'b' }),
  to_key({ mods = 'OPT', key = 'RightArrow' }, { mods = 'ALT', key = 'f' }),
  to_key({ mods = 'OPT', key = 'Backspace' }, { mods = 'CTRL', key = 'w' }),
  to_key({ mods = 'CMD', key = 'LeftArrow' }, { mods = '', key = 'Home' }),
  to_key({ mods = 'CMD', key = 'RightArrow' }, { mods = '', key = 'End' }),
  to_key({ mods = 'CMD', key = 'Backspace' }, { mods = 'CTRL', key = 'u' }),
}

for _, k in ipairs(read_json '/keys.json') do
  if k.type == 'key' then
    table.insert(config.keys, to_key(k.lhs, k.rhs))
  elseif k.type == 'tmux' then
    table.insert(config.keys, to_tmux(k.lhs, k.rhs))
  end
end

return config
