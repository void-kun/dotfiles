--- init.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

local function safeRequire(module)
    local success, loadedModule = pcall(require, module)
    if success then
        return loadedModule
    end
    print("Error loading " .. module)
end

-- vim.o.background = 'light'

safeRequire("core.options")
safeRequire("core.keymaps")
safeRequire("core.commands")
safeRequire("core.neovide")

-- Install lazy.nvim automatically
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

local opts = {
    git = { log = { "--since=3 days ago" } },
    ui = { custom_keys = { false } },
    install = { colorscheme = { "rose-pine" } },
    performance = {
        rtp = {
            disabled_plugins = {
                "gzip",
                "netrwPlugin",
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
                "rplugin",
                "editorconfig",
                "matchparen",
                "matchit",
            },
        },
    },
    checker = { enabled = false },
}

-- Load the plugins and options
require("lazy").setup("plugins", opts)

--- init.lua ends here
