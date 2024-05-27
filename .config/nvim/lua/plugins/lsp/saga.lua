--- saga.lua --- Zrik's neovim setup.
--- Code:

local sagaconfig = {
    finder = {
        max_height = 0.6,
        keys = {
            vsplit = 'v'
        },
        methods = {
            tyd = 'textDocument/typeDefinition'
        },
        ui = {
            code_action = ''
        }
    }
}

require("lspsaga").setup(sagaconfig)

--- saga.lua ends here
