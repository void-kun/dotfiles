--- notify.lua --- Zrik's neovim setup.
--- Code:

return {
    {
        "rcarriga/nvim-notify",
        config = function()
            require("notify").setup({})
        end
    },
}
--- notify.lua ends here
