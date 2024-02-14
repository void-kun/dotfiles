-- init.lua ---------------------------------------------

vim.defer_fn(function()
    pcall(require, "impatient")
end, 0)

-- Import core (options, keymapping and ui)
require("core")

-- Install the lazy Plugin Manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "--single-branch",
        "https://github.com/folke/lazy.nvim.git",
        lazypath,
    })
end

vim.opt.runtimepath:prepend(lazypath)

-- Import custom plugins
require("plugins")

-- init.lua ends here
