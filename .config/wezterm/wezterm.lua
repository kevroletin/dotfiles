-- Pull in the wezterm API
local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local act = wezterm.action

function get_appearance()
    if wezterm.gui then
        return wezterm.gui.get_appearance()
    end
    return 'Dark'
end

function scheme_for_appearance(appearance)
    if appearance:find 'Dark' then
        return 'Solarized (dark) (terminal.sexy)'
    else
        return 'Solarized (light) (terminal.sexy)'
    end
end

local function is_night_time()
    local current_hour = wezterm.time.now():format("%H") -- Gets hour (00-23)
    local hour = tonumber(current_hour)
    return hour >= 18 or hour < 6 -- True if between 6 PM (18) and 6 AM (06)
end

local function scheme_for_time()
    if is_night_time () then
        return 'Solarized (dark) (terminal.sexy)'
    else
        return 'Solarized (light) (terminal.sexy)'
    end
end

config.enable_kitty_keyboard = true

config.color_scheme = scheme_for_appearance(get_appearance())
config.colors = {
    cursor_bg = "#657b83",
    cursor_border = "#657b83",
}

config.enable_scroll_bar = false
config.enable_scroll_bar = false
config.enable_tab_bar = false
config.font = wezterm.font 'UbuntuMono Nerd Font Propo'
config.font_size = 20
config.window_padding = {
    left = "0.4cell",
    right = 0,
    top = "0.2cell",
    bottom = 0,
}

config.default_prog = { '/usr/bin/zsh' }

config.keys = {
    { key = ' ', mods = 'CTRL', action = wezterm.action.ActivateCopyMode }, -- Selection mode
    -- { key = 'v', mods = 'CTRL', action = wezterm.action.PasteFrom('Clipboard') },
    -- Clear selection using C-c
    {
        key = 'c',
        mods = 'CTRL',
        action = wezterm.action_callback(function(window, pane)
                local has_selection = window:get_selection_text_for_pane(pane) ~= ''
                if has_selection then
                    window:perform_action(act.CopyTo 'ClipboardAndPrimarySelection', pane)

                    window:perform_action(act.ClearSelection, pane)
                else
                    window:perform_action(act.SendKey { key = 'c', mods = 'CTRL' }, pane)
                end
        end),
    },
    {
        key = 'Escape',
        mods = 'NONE',
        -- clear the previous search string by entering the search mode and using it's api, not sure how to do it better
        action = wezterm.action_callback(function(window, pane)
                window:perform_action(act.ActivateCopyMode, pane)
                window:perform_action(act.CopyMode 'ClearPattern', pane)
                window:perform_action(act.CopyMode 'ClearSelectionMode', pane)
                window:perform_action(act.CopyMode 'Close', pane)
                window:perform_action(act.SendKey { key = 'Escape', mods = '' }, pane)
        end),
    },
    {
        key = '/',
        mods = 'CTRL',
        action = act.Search("CurrentSelectionOrEmptyString"),
    },
}

config.key_tables = {
    copy_mode = {
      { key = 'Tab', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
      { key = 'Tab', mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
      -- Ret copies the selection and exits the copy mode
      { key = 'Enter', mods = 'NONE', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { Multiple = { 'ScrollToBottom', { CopyMode =  'Close' } } } } },
      --
      { key = 'Escape', mods = 'NONE', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
      { key = 'Space', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
      { key = '$', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = ',', mods = 'NONE', action = act.CopyMode 'JumpReverse' },
      { key = '0', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
      { key = ';', mods = 'NONE', action = act.CopyMode 'JumpAgain' },
      { key = 'F', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = false } } },
      { key = 'F', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = false } } },
      { key = 'G', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackBottom' },
      { key = 'G', mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
      { key = 'H', mods = 'NONE', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'H', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
      { key = 'L', mods = 'NONE', action = act.CopyMode 'MoveToViewportBottom' },
      { key = 'L', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
      { key = 'M', mods = 'NONE', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'M', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
      { key = 'O', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
      { key = 'O', mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
      { key = 'T', mods = 'NONE', action = act.CopyMode{ JumpBackward = { prev_char = true } } },
      { key = 'T', mods = 'SHIFT', action = act.CopyMode{ JumpBackward = { prev_char = true } } },
      { key = 'V', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Line' } },
      { key = 'V', mods = 'SHIFT', action = act.CopyMode{ SetSelectionMode =  'Line' } },
      { key = '^', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = '^', mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'b', mods = 'CTRL', action = act.CopyMode 'PageUp' },
      -- C-c copies the selection and exits the copy mode
      { key = 'c', mods = 'CTRL', action = act.Multiple{ { CopyTo =  'ClipboardAndPrimarySelection' }, { Multiple = { 'ScrollToBottom', { CopyMode =  'Close' } } } } },
      --
      { key = 'd', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (0.5) } },
      { key = 'e', mods = 'NONE', action = act.CopyMode 'MoveForwardWordEnd' },
      { key = 'f', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = false } } },
      { key = 'f', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
      { key = 'f', mods = 'CTRL', action = act.CopyMode 'PageDown' },
      { key = 'g', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop' },
      { key = 'g', mods = 'CTRL', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
      { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
      { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },
      { key = 'm', mods = 'ALT', action = act.CopyMode 'MoveToStartOfLineContent' },

  { key = 'o', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEnd' },
      { key = 'q', mods = 'NONE', action = act.Multiple{ 'ScrollToBottom', { CopyMode =  'Close' } } },
      { key = 't', mods = 'NONE', action = act.CopyMode{ JumpForward = { prev_char = true } } },
      { key = 'u', mods = 'CTRL', action = act.CopyMode{ MoveByPage = (-0.5) } },
      { key = 'v', mods = 'NONE', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
      { key = 'v', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Block' } },
      { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
      -- y just copies selection
      { key = 'y', mods = 'NONE', action = act.CopyTo 'ClipboardAndPrimarySelection' },
      --
      { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
      { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
      { key = 'End', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = 'Home', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
      { key = 'LeftArrow', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
      { key = 'LeftArrow', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
      { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
      { key = 'RightArrow', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
      { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'MoveUp' },
      { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'MoveDown' },
    },
    search_mode = {
        -- Start visual selection around the current match
        { key = 'Enter', mods = 'NONE', action = act.ActivateCopyMode },
        --
        { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
        { key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
        { key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
        { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
        { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
        { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
        { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
        { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
        { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
    }
}

-- for numen infor panel
config.initial_cols = 20
config.initial_rows = 20

return config
