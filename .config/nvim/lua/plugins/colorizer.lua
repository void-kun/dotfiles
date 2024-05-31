--- colorizer.lua --- Zrik's neovim setup.
--- Code:

return {
    "norcalli/nvim-colorizer.lua",
    config = function()
        require("colorizer").setup({
            "css",
            "javascript",
            html = {
                mode = "foreground",
            },
        })
    end,
}

--- colorizer.lua ends here
