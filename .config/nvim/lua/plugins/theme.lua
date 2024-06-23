--- theme.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

vim.opt.background = "light"

return {
    "loctvl842/monokai-pro.nvim",
    config = function()
        require("monokai-pro").setup({
            transparent_background = true,
            filter = "ristretto",
        })
    end
}
--- theme.lua ends here
