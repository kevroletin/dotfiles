return {
  {
    "craftzdog/solarized-osaka.nvim",
    branch = "osaka",
    lazy = true,
    priority = 1000,
    opts = function()
      return {
        transparent = false,
        day_brightness = 0.2,
        styles = {
          floats = "light",
          sidebars = "light",
        },
      }
    end,
  },
}
