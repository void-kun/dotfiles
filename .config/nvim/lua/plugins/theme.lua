--- theme.lua --- Zrik's neovim setup.
--- Code:

return {
    "miikanissi/modus-themes.nvim",
    priority = 1000,
    name = "modus",
    config = function()
        require("modus-themes").setup({
            style = "modus_operandi",
            variant = "deuteranopia",
            transparent = false,  -- Transparent background (as supported by the terminal)
            dim_inactive = false, -- "non-current" windows are dimmed
            styles = {
                comments = { italic = true },
                keywords = { italic = true },
                functions = {},
                variables = {},
            },

        })
        -- vim.cmd.colorscheme("modus")
    end
}

--- theme.lua ends here
