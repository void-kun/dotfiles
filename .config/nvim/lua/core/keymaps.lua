--- keymaps.lua --- Zrik's neovim setup.
--- Code:

local vim = vim
local keymap = vim.keymap

vim.g.mapleader = " "

local function cmd(command)
    return table.concat({ '<Cmd>', command, '<CR>' })
end

-- map for normal mode
local function keyboard_quit()
    
end
keymap.set("t", "<Esc>", keyboard_quit)

-- remaps ; to act as :
keymap.set("n", ";", cmd "FineCmdline")

keymap.set("n", "<C-a>", cmd "ggVG")  -- select all
keymap.set("n", "<leader>w", cmd "w") -- save current file
-- keymap.set("n", "<leader>q", cmd "q") -- quit
keymap.set("n", "U", "<C-r>")
keymap.set({ "n", "v" }, "H", "^")
keymap.set({ "n", "v" }, "L", "$")

-- remaps jj for Esc
keymap.set("i", "jj", "<ESC>", { silent = true })

-- telescope
keymap.set("n", "<leader>ff", cmd "Telescope find_files")
keymap.set("n", "<leader>lg", cmd "Telescope live_grep")
keymap.set("n", "<leader>bf", cmd "Telescope buffers")
keymap.set("n", "<leader>pj", cmd "Telescope neovim-project discover")

keymap.set("n", "<leader>lr", cmd "Telescope lsp_references")
keymap.set("n", "<leader>ld", cmd "Telescope lsp_document_symbols")
keymap.set("n", "<leader>ss", cmd "Telescope current_buffer_fuzzy_find")
keymap.set("n", "<leader>td", cmd "TodoTelescope")

-- lspsaga
keymap.set({ "n", "t" }, "<A-d>", cmd "Lspsaga term_toggle")
keymap.set("n", "<leader>dc", cmd "Lspsaga code_action")
keymap.set("n", "<leader>dp", cmd "Lspsaga goto_definition")
keymap.set("n", "<leader>ds", cmd "Lspsaga finder tyd+ref+imp+def")
keymap.set("n", "<leader>df", cmd "Lspsaga rename")

-- buffers
keymap.set("n", "<leader>c", cmd "bd")
keymap.set("n", "<leader><leader>", cmd "b#")

-- window
keymap.set("n", "|", cmd "vsplit")
keymap.set("n", "_", cmd "split")
keymap.set("n", "=", cmd "wincmd =")

keymap.set('n', '<C-w>z', cmd 'WindowsMaximize')
keymap.set('n', '<C-w>_', cmd 'WindowsMaximizeVertically')
keymap.set('n', '<C-w>|', cmd 'WindowsMaximizeHorizontally')
keymap.set('n', '<C-w>=', cmd 'WindowsEqualize')

-- move lines
keymap.set("n", "<A-k>", ":m .-2<CR>==")
keymap.set("n", "<A-j>", ":m .+1<CR>==")
keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv")
keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv")


-- DAP
local function open_dap()
    local widgets = require('dap.ui.widgets');
    local sidebar = widgets.sidebar(widgets.scopes);
    sidebar.open()
end

keymap.set("n", "<C-d>p", cmd "DapToggleBreakpoint")
keymap.set("n", "<C-d>o", open_dap)

--- keymaps.lua ends here
