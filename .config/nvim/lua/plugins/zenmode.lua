--- zenmode.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

return {
    "folke/zen-mode.nvim",
    opts = {
        window = {
            options = {
                signcolumn = "yes",
                number = true,
                cursorline = true,
                list = true
            }
        }
    }
}

--- zenmode.lua ends here
