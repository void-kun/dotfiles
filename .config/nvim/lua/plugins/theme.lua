--- theme.lua --- Zrik's neovim setup.
--- Code:

local vim = vim


-- return {
--     "rose-pine/neovim",
--     name = "rose-pine",
--     config = function()
--         require("rose-pine").setup({
--             variant = "dawn",
--             -- disable_background = true
--         })
--
--         vim.cmd [[ colorscheme rose-pine ]]
--     end
-- }


vim.o.background = "light"
return {
    {
        "rebelot/kanagawa.nvim",
        config = function()
            require('kanagawa').setup({
                theme = "lotus",     -- Load "wave" theme when 'background' option is not set
                background = {       -- map the value of 'background' option to a theme
                    dark = "dragon", -- try "dragon" !
                    light = "lotus"
                },
            })
            -- vim.cmd("colorscheme kanagawa")
        end
    },
    {
        "miikanissi/modus-themes.nvim",
        priority = 1000,
        config = function()
            require("modus-themes").setup({
                -- `default`, `tinted`, `deuteranopia`, and `tritanopia`
                variant = "tritanopia"
            })

            vim.cmd("colorscheme modus")
        end
    } }
--- theme.lua ends here
