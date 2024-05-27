--- windline.lua --- Zrik's neovim setup.
--- Code:

return {
    "windwp/windline.nvim",
    config = function()
        require('wlsample.vscode')
        require('windline').add_status(
            require('spectre.state_utils').status_line()
        )
    end,
}

--- windline.lua ends here
