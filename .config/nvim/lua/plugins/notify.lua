--- notify.lua --- Zrik's neovim setup.
--- Code:

return {
    {
        "rcarriga/nvim-notify",
        config = function()
            require("notify").setup({})
        end
    },
    {
        'mrded/nvim-lsp-notify',
        config = function()
            require('lsp-notify').setup({
                notify = require("notify"),
                icons = {
                    spinner = { '|', '/', '-', '\\' },
                    done = '!'
                }
            })
        end
    }
}
--- notify.lua ends here
