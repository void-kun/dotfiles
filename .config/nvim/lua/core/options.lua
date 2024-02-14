-- options.lua --------------------------------

local opt = vim.opt

opt.clipboard = "unnamedplus"
opt.cmdheight = 0
opt.completeopt = { "menuone", "noselect" }
opt.conceallevel = 0
opt.fileencoding = "utf-8"
opt.hlsearch = true
opt.ignorecase = true
opt.mouse = "a"
opt.pumheight = 10
opt.showmode = true
opt.showtabline = 0
opt.smartcase = true
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.swapfile = false
opt.termguicolors = true
opt.timeoutlen = 1000
opt.undofile = true
opt.updatetime = 300
opt.writebackup = false
opt.expandtab = true
opt.softtabstop = 4
opt.tabstop = 4
opt.shiftwidth = 4
opt.cursorline = true
opt.number = true
opt.relativenumber = true
opt.laststatus = 3
opt.showcmd = true
opt.ruler = false
opt.numberwidth = 4
opt.signcolumn = "yes"
opt.wrap = false
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.guifont = "IosevkaLyteTerm:h17"
opt.fillchars.eob=" "
opt.shortmess:append "c"
opt.whichwrap:append("<,>,[,],h,l")
opt.iskeyword:append("-")
opt.formatoptions:remove({"c", "r", "o"})
opt.linebreak = true

-- restore cursor pos
local api = vim.api

api.nvim_create_autocmd({ "BufReadPost" }, {
    pattern = { "*" },
    callback = function()
	api.nvim_exec('silent! normal! g`"zv', false)
    end,
})

vim.g.gui_font_default_size = 1
vim.g.gui_font_size = vim.g.gui_font_default_size
vim.g.gui_font_face = "IosevkaLyteTerm"

RefreshGuiFont = function()
    opt.guifont = string.format("%s:h%s", vim.g.gui_font_face, vim.g.gui_font_size)
end

ResizeGuiFont = function(delta)
    vim.g.gui_font_size = vim.g.gui_font_size + delta
end

ResetGuiFont = function()
    vim.g.gui_font_size = vim.g.gui_font_default_size
    RefreshGuiFont()
end

-- Call reset font by startup
ResetGuiFont()

local opts = {silent = true}
vim.keymap.set({'n', 'i'}, "<C-+>", function () ResizeGuiFont(1) end, opts)
vim.keymap.set({'n', 'i'}, "<C-->", function () ResizeGuiFont(-1) end, opts)
vim.keymap.set({'n', 'i'}, "<C-BS>", function () ResetGuiFont() end, opts)

-- options.lua ends here

