-- options.lua --- Zrik's neovim setup.
--- Code:

-- Option =================================================================
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

for k, v in pairs(options) do
    opt[k] = v
end

local cmd = vim.cmd
local api = vim.api
local nvim_create_autocmd = api.nvim_create_autocmd
local nvim_set_hl = api.nvim_set_hl

-- Whitespace =================================================================
local space = "·"
opt.list = true
opt.listchars:append {
    tab = "│─",
    multispace = space,
    lead = space,
    trail = space,
    nbsp = space
}

cmd([[match TrailingWhitespace /\s\+$/]])
nvim_set_hl(0, "TrailingWhitespace", { link = "Error" })
nvim_create_autocmd("InsertEnter", {
    callback = function()
        opt.listchars.trail = nil
        nvim_set_hl(0, "TrailingWhitespace", { link = "Whitespace" })
    end
})

nvim_create_autocmd("InsertLeave", {
    callback = function()
        opt.listchars.trail = space
        nvim_set_hl(0, "TrailingWhitespace", { link = "Error" })
    end
})

if vim.fn.executable("rg") then
    opt.grepprg = "rg --vimgrep --no-heading --smart-case"
    opt.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end

if vim.fn.executable("prettier") then
    opt.formatprg = "prettier --stdin-filepath=%"
end

-- Window Option ==============================================================
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

-- Diagnostic =================================================================
-- Specify how the border looks like
local border = {
    { '┌', 'FloatBorder' },
    { '─', 'FloatBorder' },
    { '┐', 'FloatBorder' },
    { '│', 'FloatBorder' },
    { '┘', 'FloatBorder' },
    { '─', 'FloatBorder' },
    { '└', 'FloatBorder' },
    { '│', 'FloatBorder' },
}

-- Add border to the diagnostic popup window
vim.diagnostic.config({
    virtual_text = {
        prefix = '■ ',
    },
    float = { border = border },
})

-- LSP diagnostic opthons setup
vim.fn.sign_define({
    {
        name = "DiagnosticSignError",
        text = " ",
        texthl = "DiagnosticSignError",
        linehl = "ErrorLine",
    },
    {
        name = "DiagnosticSignWarn",
        text = " ",
        texthl = "DiagnosticSignWarn",
        linehl = "WarningLine",
    },
    {
        name = "DiagnosticSignInfo",
        text = " ",
        texthl = "DiagnosticSignInfo",
        linehl = "InfoLine",
    },
    {
        name = "DiagnosticSignHint",
        text = "󰛩 ",
        texthl = "DiagnosticSignHint",
        linehl = "HintLine",
    },
})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    update_in_insert = true,
    underline = true,
    serverity_sort = true,
    float = {
        show_header = true,
        border = "rounded",
        source = "always",
        header = "",
    },
})

vim.api.nvim_set_option("updatetime", 200)

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])

--- options.lua ends here
