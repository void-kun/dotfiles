--- theme.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

-- vim.o.background = "light"
return {
    {
        "sainnhe/gruvbox-material",
        lazy = false,
        priority = 1000,
        config = function()
            vim.g.gruvbox_material_enable_italic = true
            vim.cmd.colorscheme('gruvbox-material')
        end
    }
}
--- theme.lua ends here
