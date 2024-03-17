return {
  "VonHeikemen/fine-cmdline.nvim",
  lazy = false,
  priority = 1000,
  dependencies = {
    "MunifTanjim/nui.nvim",
  },
  config = function()
    require("fine-cmdline").setup({
      cmdlin = {
        enable_keymaps = true,
        smart_history = true,
        prompt = ": ",
      },
      popup = {
        position = {
          row = "10%",
          col = "50%",
        },
        size = {
          width = "60%",
        },
        border = {
          style = "rounded",
        },
        win_options = {
          winhighlight = "Normal:Normal,FloatBorder:FloatBorder",
        },
      },
    })
  end,
}
