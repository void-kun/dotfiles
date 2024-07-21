--- theme.lua --- Zrik's neovim setup.
--- Code:

local vim = vim


-- return {
--     "loctvl842/monokai-pro.nvim",
--     config = function()
--         require("monokai-pro").setup({
--             -- transparent_background = true,
--             filter = "ristretto",
--         })
--         vim.cmd [[ colorscheme monokai-pro ]]
--     end
-- }

return {
    "rose-pine/neovim",
    name = "rose-pine",
    config = function()
        require("rose-pine").setup({
            variant = "dawn",
            -- disable_background = true
        })

        vim.cmd [[ colorscheme rose-pine ]]
    end
}

--- theme.lua ends here
