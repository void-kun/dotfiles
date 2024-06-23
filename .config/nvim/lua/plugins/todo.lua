--- todo.lua --- Zrik's neovim setup.
--- Code:

return {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
        signs = true,
        keywords = {
            FIX = { icon = "󰁨 " },
            TODO = { icon = " " },
            HACK = { icon = "󰬏 " },
            WARN = { icon = " " },
            PERF = { icon = " " },
            NOTE = { icon = " " },
            TEST = { icon = "󰙨 " },
        }
    }
}

--- todo.lua ends here
