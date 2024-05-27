--- keymaps.lua --- Zrik's neovim setup.
--- Code:

local vim = vim
local keymap = vim.keymap

vim.g.mapleader = " "

-- map for normal mode
keymap.set("t", "<Esc>", [[<C-\><C-n>]])

-- remaps ; to act as :
keymap.set("n", ";", "<cmd>FineCmdline<cr>", { noremap = true })

keymap.set("n", "<C-a>", "ggVG")           -- select all
keymap.set("n", "<leader>w", "<cmd>w<cr>") -- save current file
keymap.set("n", "<leader>q", "<cmd>q<cr>") -- quit
keymap.set("n", "U", "<C-r>", { noremap = true })
keymap.set("n", "H", "^", { noremap = true })
keymap.set("n", "L", "$", { noremap = true })

-- search panel
keymap.set('n', '<leader>S', '<cmd>lua require("spectre").toggle()<cr>', { desc = "Toggle Spectre" })
keymap.set('n', '<leader>sw', '<cmd>lua require("spectre").open_visual({select_word=true})<cr>',
    { desc = "Search current word" })
keymap.set('v', '<leader>sw', '<cmd>lua require("spectre").open_visual()<cr>', { desc = "Search current word" })
keymap.set('n', '<leader>sp', '<cmd>lua require("spectre").open_file_search({select_word=true})<cr>',
    { desc = "Search on current file" })

-- telescope
keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>")
keymap.set("n", "<leader>lg", "<cmd>Telescope live_grep<cr>")
keymap.set("n", "<leader>bf", "<cmd>Telescope buffers<cr>")
keymap.set("n", "<leader>pj", "<cmd>Telescope neovim-project discover<cr>")

keymap.set("n", "<leader>lr", "<cmd>Telescope lsp_references<cr>")
keymap.set("n", "<leader>ld", "<cmd>Telescope lsp_document_symbols<cr>")
keymap.set("n", "<leader>td", "<cmd>TodoTelescope<cr>")

-- lsp actions
keymap.set({ "n", "t" }, "<A-d>", "<cmd>Lspsaga term_toggle<cr>")
keymap.set("n", "<leader>dc", "<cmd>Lspsaga code_action<cr>")
keymap.set("n", "<leader>dp", "<cmd>Lspsaga goto_definition<cr>")
keymap.set("n", "<leader>ds", "<cmd>Lspsaga finder tyd+ref+imp+def<cr>")

-- buffers
keymap.set("n", "<leader>c", "<cmd>bd<cr>")
keymap.set("n", "<leader><leader>", "<cmd>b#<cr>")

-- split window
keymap.set("n", "|", "<cmd>vsplit<cr><c-w><c-w>")
keymap.set("n", "_", "<cmd>split<cr>")
keymap.set("n", "=", "<cmd>wincmd =<cr>")

--- keymaps.lua ends here
