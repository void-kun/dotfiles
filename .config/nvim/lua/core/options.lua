-- config
local vim = vim
local opt = vim.opt
local g = vim.g

vim.g.matchparen_timeout = 20
vim.g.matchparen_insert_timeout = 20

local options = {
	equalalways = true,
	winbar = "%=%m %F",
	nrformats = { "alpha", "octal", "hex" },
	virtualedit = "block",
	modelines = 5,
	modelineexpr = false,
	modeline = true,
	cursorline = true,
	cursorcolumn = false,
	splitright = true,
	splitbelow = true,
	smartcase = true,
	hlsearch = true,
	ignorecase = true,
	incsearch = true,
	inccommand = "nosplit",
	hidden = true,
	autoindent = true,
	termguicolors = true,
	showmode = false,
	showmatch = true,
	matchtime = 4,
	wildmode = "longest:full,full",
	number = true,
	linebreak = true,
	joinspaces = false,
	path = vim.opt.path + "**",
	isfname = vim.opt.isfname:append("@-@"),
	autochdir = true,
	relativenumber = true,
	numberwidth = 8,
	shada = "!,'50,<50,s10,h,r/tmp",
	expandtab = true,
	smarttab = true,
	smartindent = true,
	shiftround = true,
	shiftwidth = 4,
	tabstop = 4,
	foldenable = false,
	foldlevel = 99,
	foldlevelstart = 99,
	foldcolumn = "1",
	foldmethod = "expr",
	foldexpr = "nvim_treesitter#foldexpr()",
	undodir = os.getenv("HOME") .. "/.config/nvim/undodir",
	undofile = true,
	showtabline = 0,
	mouse = "a",
	mousescroll = "ver:2,hor:6",
	scrolloff = 3,
	sidescrolloff = 3,
	wrap = true,
	list = true,
	lazyredraw = true,
	updatetime = 250,
	laststatus = 3,
	confirm = false,
	conceallevel = 3,
	cmdheight = 1,
}

for k, v in pairs(options) do
	vim.opt[k] = v
end
vim.opt.listchars:append("space:⋅")
vim.opt.listchars:append("tab:- ")

vim.cmd("set clipboard+=unnamedplus")

if vim.fn.executable("rg") then
	vim.opt.grepprg = "rg --vimgrep --no-heading --smart-case"
	vim.opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end

if vim.fn.executable("prettier") then
	opt.formatprg = "prettier --stdin-filepath=%"
end

opt.guicursor = {
	"n-v:block",
	"i-c-ci-ve:ver25",
	"r-cr:hor20",
	"o:hor50",
	"i:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor",
	"sm:block-blinkwait175-blinkoff150-blinkon175",
}

vim.opt.formatoptions = "tcrqn"
vim.wo.relativenumber = true
vim.wo.number = true

vim.g.mapleader = " "

-- LSP diagnostic opthons setup
vim.fn.sign_define({
	{
		name = "DiagnosticSignError",
		text = " ",
		texthl = "DiagnosticSignError",
		linehl = "ErrorLine",
	},
	{
		name = "DiagnosticSignWarn",
		text = " ",
		texthl = "DiagnosticSignWarn",
		linehl = "WarningLine",
	},
	{
		name = "DiagnosticSignInfo",
		text = " ",
		texthl = "DiagnosticSignInfo",
		linehl = "InfoLine",
	},
	{
		name = "DiagnosticSignHint",
		text = " ",
		texthl = "DiagnosticSignHint",
		linehl = "HintLine",
	},
})

vim.diagnostic.config({
	virtual_text = false,
	signs = true,
	update_in_insert = true,
	underline = true,
	serverity_sort = false,
	float = {
		border = "rounded",
		source = "always",
		header = "",
		prefix = "",
	},
})

vim.opt.completeopt = { "menuone", "noselect", "noinsert" }
vim.opt.shortmess = vim.opt.shortmess + { c = true }
vim.api.nvim_set_option("updatetime", 300)

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])

-- window-local options
local window_options = {
	numberwidth = 4,
	number = true,
	relativenumber = true,
	linebreak = true,
	cursorline = true,
	foldenable = false,
	foldmethod = "expr",
	foldexpr = "nvim_treesitter#foldexpr()",
}

for k, v in pairs(window_options) do
	vim.wo[k] = v
end
