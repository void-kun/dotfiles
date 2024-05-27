-- options.lua --- Zrik's neovim setup.
--- Code:

local vim = vim
local opt = vim.opt

vim.g.matchparen_timeout = 20
vim.g.matchparen_insert_timeout = 20

local options = {
    equalalways = true,
    swapfile = false,
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
    relativenumber = false,
    numberwidth = 4,
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
    wrapmargin = 10,
    lazyredraw = true,
    updatetime = 250,
    laststatus = 3,
    confirm = false,
    conceallevel = 3,
    cmdheight = 1,
    formatoptions = "tcrqn",
    smoothscroll = true,
    spell = true,
    clipboard = "unnamedplus",
    completeopt = { "menuone", "noselect", "noinsert" },
    shortmess = vim.opt.shortmess + { c = true },
}

-- show listchars
opt.list = true
opt.listchars = {
    -- show tabs as ▸
    tab = "▸ ",
    -- show tailing spaces as ·
    trail = "·",
}

for k, v in pairs(options) do
    opt[k] = v
end

if vim.fn.executable("rg") then
    opt.grepprg = "rg --vimgrep --no-heading --smart-case"
    opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end

if vim.fn.executable("prettier") then
    opt.formatprg = "prettier --stdin-filepath=%"
end

vim.wo.relativenumber = true
vim.wo.number = true

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
--- options.lua ends here
