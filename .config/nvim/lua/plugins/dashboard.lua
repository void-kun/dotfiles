--- dashboard.lua --- Zrik's neovim setup.
--- Code:

local vim = vim
return {
    'nvimdev/dashboard-nvim',
    event = 'VimEnter',
    config = function()
        require('dashboard').setup {

        }
    end,
    dependencies = { { 'nvim-tree/nvim-web-devicons' } }
}

--- dashboard.lua ends here
