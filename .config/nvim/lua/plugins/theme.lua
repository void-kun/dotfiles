--- theme.lua --- Zrik's neovim setup.
--- Code:

return {
    {
        "loctvl842/monokai-pro.nvim",
        priority = 1000,
        name = "monokai-pro",
        config = function()
            require("monokai-pro").setup({
                transparent_background = true,
                terminal_colors = true,
                devicons = true, -- highlight the icons of `nvim-web-devicons`
                styles = {
                    comment = { italic = true },
                    keyword = { italic = true },       -- any other keyword
                    type = { italic = true },          -- (preferred) int, long, char, etc
                    storageclass = { italic = true },  -- static, register, volatile, etc
                    structure = { italic = true },     -- struct, union, enum, etc
                    parameter = { italic = true },     -- parameter pass in function
                    annotation = { italic = true },
                    tag_attribute = { italic = true }, -- attribute of tag in reactjs
                },
                filter = "ristretto",                  -- classic | octagon | pro | machine | ristretto | spectrum
                inc_search = "background",             -- underline | background
                background_clear = {
                    "float_win",
                    "toggleterm",
                    "telescope",
                    "renamer",
                    "notify",
                    "nvim-tree",
                },
                plugins = {
                    bufferline = {
                        underline_selected = false,
                        underline_visible = false,
                    },
                    indent_blankline = {
                        context_highlight = "default", -- default | pro
                        context_start_underline = false,
                    },
                },
                override = function()
                    return {
                        WinSeparator = { fg = "#cacaca" }
                    }
                end
            })
            vim.cmd.colorscheme("monokai-pro")
        end
    },
}

--- theme.lua ends here
