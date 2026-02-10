-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.cursorline = false
vim.opt.wrap = false
vim.o.title = true
vim.o.titlestring = "NVIM: %t"

vim.diagnostic.config({
  float = {
    border = "rounded",
  },
})
