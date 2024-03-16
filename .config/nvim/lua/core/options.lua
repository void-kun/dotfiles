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
	cursorline = false,
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
	matchtime = 2,
	wildmode = "longest:full,full",
	number = true,
	linebreak = true,
	joinspaces = false,
	path = vim.opt.path + "**",
	isfname = vim.opt.isfname:append("@-@"),
	autochdir = true,
	relativenumber = true,
	numberwidth = 2,
	shada = "!,'50,<50,s10,h,r/tmp",
	expandtab = true,
	smarttab = true,
	smartindent = true,
	shiftround = true,
	shiftwidth = 2,
	tabstop = 2,
	foldenable = false,
	foldlevel = 99,
	foldlevelstart = 99,
	foldcolumn = "1",
	foldmethod = "expr",
	foldexpr = "nvim_treesitter#foldexpr()",
	undodir = os.getenv("HOME") .. "/.vim/undodir",
	undofile = true,
	showtabline = 0,
	mouse = "a",
	mousescroll = "ver:2,hor:6",
	scrolloff = 3,
	sidescrolloff = 3,
	wrap = true,
	list = false,
	lazyredraw = true,
	updatetime = 250,
	laststatus = 3,
	confirm = false,
	conceallevel = 3,
	cmdheight = 0,
}

for k, v in pairs(options) do
	vim.opt[k] = v
end

vim.cmd("set clipboard+=unnamedplus")

if vim.fn.executable("rg") then
	-- if ripgrep installed, use that as a grepper
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

vim.fn.sign_define({
	{
		name = "DiagnosticSignError",
		text = "",
		texthl = "DiagnosticSignError",
		linehl = "ErrorLine",
	},
	{
		name = "DiagnosticSignWarn",
		text = "",
		texthl = "DiagnosticSignWarn",
		linehl = "WarningLine",
	},
	{
		name = "DiagnosticSignInfo",
		text = "",
		texthl = "DiagnosticSignInfo",
		linehl = "InfoLine",
	},
	{
		name = "DiagnosticSignHint",
		text = "",
		texthl = "DiagnosticSignHint",
		linehl = "HintLine",
	},
})

vim.diagnostic.config({
	float = {
		border = "rounded",
	},
})

-- window-local options
window_options = {
	numberwidth = 2,
	number = true,
	relativenumber = true,
	linebreak = true,
	cursorline = false,
	foldenable = false,
}

for k, v in pairs(window_options) do
	vim.wo[k] = v
end
