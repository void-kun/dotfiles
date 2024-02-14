-- noice.lua --------------------------------

  require("noice").setup({
    cmdline = {
      view = "cmdline",
      format = {
          cmdline = { pattern = "^:", icon = "󰘳  :", lang = "vim" },
          search_down = { kind = "search", pattern = "^/", icon = "󰩊  search: ", lang = "regex" },
          search_up = { kind = "search", pattern = "^%?", icon = "󰩊  search: ", lang = "regex" },
          filter = { pattern = "^:%s*!", icon = "󰻿  filter: ", lang = "bash" },
          lua = { pattern = { "^:%s*lua%s+", "^:%s*lua%s*=%s*", "^:%s*=%s*" }, icon = " run lua: ", lang = "lua" },
          help = { pattern = "^:%s*he?l?p?%s+", icon = "󰞋  help: " },
      }
    },
  })
-- noice.lua ends here
