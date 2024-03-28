local vim = vim
-- load env
vim.env.PATH = "/usr/local/go/bin:" .. vim.env.PATH
vim.env.PATH = "/home/zrik/.go/bin:" .. vim.env.PATH

vim.fn.setenv("TERM", "tmux-256color")

local function safeRequire(module)
	local success, loadedModule = pcall(require, module)
	if success then
		return loadedModule
	end
	print("Error loading " .. module)
end

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
	install = { colorscheme = { "onedarkpro" } },
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
