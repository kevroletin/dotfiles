return {
  -- Disable snacks.nvim indent scope animations
  {
    "folke/snacks.nvim",
    opts = {
      indent = { enabled = false },
      scope = { enabled = false },
    },
  },
  -- Disable mini.indentscope if it's loaded
  {
    "nvim-mini/mini.indentscope",
    enabled = false,
  },
}
