-- lspsaga.lua --------------------------------
local status_ok, saga = pcall(require, "lspsaga")
if not status_ok then
    return 
end

saga.setup({
    ui = {
        winblend = 10,
        border = "rounded",
    },
    -- migrate to bbq
    sumbol_in_winbar = {
        enable = false,
    },
    diagnostic = {
        on_insert = false,
        on_insert_follow = false,
    }
})

local keymap = vim.keymap.set

keymap("n", "gh", "<cmd>Lspsaga lsp_finder<CR>")
-- code action
keymap({"n", "v"}, "<leader>ca", "<cmd>Lspsaga code_action<CR>")
-- rename
keymap("n", "<leader>gr", "<cmd>Lspsaga rename<CR>")
-- go to definition
keymap("n", "gd", "<cmd>Lspsaga goto_definition<CR>")
-- show cursor diagnostic
keymap("n", "<leader>sc", "<cmd>Lspsaga show_cursor_diagnostics<CR>")
-- show buffer diagnostic
keymap("n", "<leader>sb", "<cmd>Lspsaga show_buf_diagnostics<CR>")
-- jump diagnostic
keymap("n", "<C-m>", "<cmd>Lspsaga diagnostic_jump_prev<CR>")
keymap("n", "<C-n>", "<cmd>Lspsaga diagnostic_jump_next<CR>")

keymap("i", "<C-l>", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
keymap("n", "gp", "<cmd>Lspsaga peek_definition<CR>")

-- lspsaga.lua ends here
