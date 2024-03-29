local keymap = vim.keymap

-- remaps ; to act as :
keymap.set("n", ";", "<cmd>FineCmdline<cr>", { noremap = true })

keymap.set("n", "<C-a>", "ggVG") -- select all
keymap.set({ "n", "x" }, "<leader>p", '"0p') -- paste not overwritten by delete
keymap.set("n", "<leader>w", "<cmd>w<cr>") -- save current file

keymap.set("n", "Q", "<cmd>q<cr>") -- quit
keymap.set("n", "<leader>q", "<cmd>q<cr>") -- quit
keymap.set("n", "U", "<C-r>", { noremap = true })
keymap.set("n", "H", "^", { noremap = true })
keymap.set("n", "L", "$", { noremap = true })

-- telescope
keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>")
keymap.set("n", "<leader>gg", "<cmd>Telescope live_grep<cr>")
keymap.set("n", "<leader>b", "<cmd>Telescope buffers<cr>")
keymap.set("n", "<leader>d", "<cmd>Telescope diagnostics<cr>")
keymap.set("n", "<leader>s", "<cmd>Telescope lsp_document_symbols<cr>")
keymap.set("n", "<leader>k", "<cmd>Telescope lsp_references<cr>")
keymap.set("n", "<C-t>", "<cmd>Telescope lsp_document_symbols<cr>")

-- lsp and rust
keymap.set("n", "<leader>x", "<cmd>RustRunnables<cr>")

-- buffers
keymap.set("n", "]b", "<cmd>bnext<cr>")
keymap.set("n", "[b", "<cmd>bprevious<cr>")
keymap.set("n", "<leader>c", "<cmd>bd<cr>")
keymap.set("n", "<leader><leader>", "<cmd>b#<cr>")

-- splits
keymap.set("n", "|", "<cmd>vsplit<cr><c-w><c-w>")
keymap.set("n", "_", "<cmd>split<cr>")
keymap.set("n", "=", "<cmd>wincmd =<cr>")

vim.keymap.set("n", "<C-;>", function()
	require("Comment.api").toggle.linewise.current()
end, { noremap = true, silent = true })
-- ends
