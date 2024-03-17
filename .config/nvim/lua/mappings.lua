require "nvchad.mappings"

-- map   ======================================================

local map = vim.keymap.set

map("n", "<S-L>", "$", { desc = "go to end of line" })
map("n", "<S-H>", "^", { desc = "go to begin of line" })
map("v", "<S-L>", "$", { desc = "go to end of line" })
map("v", "<S-H>", "^", { desc = "go to begin of line" })

map("n", "<leader>w", "<cmd>w<CR>", { desc = "Save file" })
map("n", "<leader>q", "<cmd>q<CR>", { desc = "Quit file" })

map("i", "jk", "<ESC>", { desc = "Escape insert mode" })
map("n", ";", "<cmd>FineCmdline<CR>", { desc = "Instead for cmd" })

map("n", "<C-a>", "ggVG")             -- select all
map({ "n", "x" }, "<leader>p", '"0p') -- paste not overwritten by delete
map("n", "<leader>w", "<cmd>w<cr>")   -- save current file

map("n", "Q", "<cmd>q<cr>")           -- quit
map("n", "<leader>q", "<cmd>q<cr>")   -- quit
map("n", "U", "<C-r>")
map("n", "H", "^")
map("n", "L", "$")

map("v", "H", "^")
map("v", "L", "$")

-- telescope
map("n", "<leader>ff", "<cmd>Telescope find_files<cr>")
map("n", "<leader>gg", "<cmd>Telescope live_grep<cr>")
map("n", "<leader>b", "<cmd>Telescope buffers<cr>")
map("n", "<leader>d", "<cmd>Telescope diagnostics<cr>")
map("n", "<leader>s", "<cmd>Telescope lsp_document_symbols<cr>")
map("n", "<leader>k", "<cmd>Telescope lsp_references<cr>")
map("n", "<C-t>", "<cmd>Telescope lsp_document_symbols<cr>")

-- lsp and rust
map("n", "<leader>x", "<cmd>RustRunnables<cr>")

-- buffers
map("n", "]b", "<cmd>bnext<cr>")
map("n", "[b", "<cmd>bprevious<cr>")
map("n", "<leader>c", "<cmd>bd<cr>")
map("n", "<leader><leader>", "<cmd>b#<cr>")

-- DAP
local function open_sidebar()
    local widgets = require('dap.ui.widgets')
    local sidebar = widgets.sidebar(widgets.scopes)
    sidebar.open()
end

map('n', '<leader>db', '<cmd>DapToggleBreakpoint<CR>', { desc = "toggle breakpoint" })
map('n', '<leader>dus', open_sidebar, { desc = "open dap sidebar" })

-- Rust
local function rust_crate_upgrade()
    require("crates").upgrade_all_crates()
end

map('n', '<leader>rcu', rust_crate_upgrade, { desc = "upgrade all crates" })

-- Go
local function go_debug_test()
    require("dap-go").debug_test()
end
map('n', '<leader>dgt', go_debug_test, { desc = "debug test for go" })

local function go_debug_last_test()
    require("dap-go").debug_last()
end
map('n', '<leader>dgl', go_debug_last_test, { desc = "debug last test for go" })


