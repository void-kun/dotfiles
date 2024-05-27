--- commands.lua --- Zrik's neovim setup.
--- Code:

local vim = vim
local autocmd = vim.api.nvim_create_autocmd

-- Rewrite all augroups using tis function
local function augroup(name)

    return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })
end

--- commands.lua ends here
