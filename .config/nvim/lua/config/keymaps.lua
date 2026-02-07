-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>fs", "<cmd>w<cr>", { desc = "Save File" })

-- Remap buffer search from <Space>f to <Space>b prefix
vim.keymap.set("n", "<leader>bB", function() Snacks.picker.buffers({ hidden = true, nofile = true }) end, { desc = "Buffers (all)" })
vim.keymap.set("n", "<leader>bb", function() Snacks.picker.buffers() end, { desc = "Buffers" })

-- Buffer navigation
vim.keymap.set("n", "<leader>bp", "<cmd>bprevious<cr>", { desc = "Switch to previous buffer" })
vim.keymap.set("n", "<leader>bn", "<cmd>bnext<cr>", { desc = "Switch to next buffer" })

-- Start LSP for current filetype
vim.keymap.set("n", "<leader>cL", function()
  local filetype = vim.bo.filetype

  -- Map filetypes to their LSP server names
  local ft_to_lsp = {
    rust = "rust_analyzer",
    python = "pyright",
    lua = "lua_ls",
    javascript = "tsserver",
    typescript = "tsserver",
    go = "gopls",
    -- Add more mappings as needed
  }

  local lsp_name = ft_to_lsp[filetype]

  if lsp_name then
    -- Check if already running
    local clients = vim.lsp.get_active_clients({ bufnr = 0, name = lsp_name })
    if #clients == 0 then
      vim.cmd("LspStart " .. lsp_name)
      vim.notify("Started " .. lsp_name, vim.log.levels.INFO)
    else
      vim.notify(lsp_name .. " already running", vim.log.levels.INFO)
    end
  else
    vim.notify("No LSP mapping found for filetype: " .. filetype, vim.log.levels.WARN)
  end
end, { desc = "Start LSP for filetype" })
