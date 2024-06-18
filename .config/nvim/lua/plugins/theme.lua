--- theme.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

return {
    "arzg/vim-colors-xcode",
    config = function()
        -- vim.cmd.colorscheme("xcodelight")
        -- vim.cmd.colorscheme("xcodelighthc")
        -- vim.cmd.colorscheme("xcodewwdc")
        -- vim.cmd.colorscheme("xcodedark")
        vim.cmd.colorscheme("xcodedarkhc")
    end
}

--- theme.lua ends here
