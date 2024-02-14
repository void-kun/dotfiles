-- mappings.lua --------------------------------

local opts = { silent = true }
local keymap = vim.keymap.set

-- Remap leader key to space

keymap("", "<Space>", "<Nop>")
vim.g.mapleader = " "

-- Modes
-- normal       : "n"
-- insert       : "i"
-- visual       : "v"
-- visual_block : "x"
-- term         : "t"
-- command      : "c"

-- =================================================
-- normal mode

-- window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)
-- resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)
-- navigate buffer
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)
-- clear highlight
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
-- close buffer
keymap("n", "<S-w>", ":bdelete", opts)
-- save 
keymap("n", "<C-s>", ":w<CR>", opts)
-- telescope:find
keymap("n", "<leader>ff", ":Telescope find_files<CR>", opts)
keymap("n", "<leader>ft", ":Telescope live_grep<CR>", opts)
keymap("n", "<leader>fp", ":Telescope projects<CR>", opts)
keymap("n", "<leader>fb", ":Telescope buffers<CR>", opts)
keymap("n", "<leader>fo", ":Telescope oldfiles<CR>", opts)
-- telescope:git 
keymap("n", "<leader>cm", ":Telescope git_commits<CR>", opts)
keymap("n", "<leader>gt", ":Telescope git_status<CR>", opts)
-- telescope:hiden term
keymap("n", "<leader>pt", ":Telescope terms<CR>", opts)
-- others
keymap("n", "<leader>ro", "SearchReplaceSingleBufferOpen<CR>", opts)
keymap("n", "<leader>rw", "SearchReplaceSingleBufferCWord<CR>", opts)
-- DAP
keymap("n", "<leader>db", "<cmd>lua require'dap'.toggle_breakpoint()<CR>", opts)
keymap("n", "<leader>dc", "<cmd>lua require'dap'.continue()<CR>", opts)
keymap("n", "<leader>di", "<cmd>lua require'dap'.step_into()<CR>", opts)
keymap("n", "<leader>do", "<cmd>lua require'dap'.step_over()<CR>", opts)
keymap("n", "<leader>dO", "<cmd>lua require'dap'.step_out()<CR>", opts)
keymap("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<CR>", opts)
keymap("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<CR>", opts)
keymap("n", "<leader>du", "<cmd>lua require'dap'.toggle()<CR>", opts)
keymap("n", "<leader>dt", "<cmd>lua require'dap'.terminate()<CR>", opts)
-- LSP
keymap("n", "<leader>lf", "<cmd>lua vim.lsp.buf.format{ async = true }<cr>", opts)
-- other ultilities
keymap("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
keymap("n", "<leader>x", "<cmd>!chmod +x %<CR>", opts)
-- lazygit
keymap("n", "<leader>gg", "<cmd>LazyGit<CR>", opts)
-- select all
keymap("n", "<C-a>", "ggVG", opts)
-- markdown text open
keymap("n", "<leader>mp", "<cmd>PeekOpen<CR>", opts)
-- open ranger
keymap("n", "<leader>t", "<cmd>RnvimrToggle<CR>", opts)
-- resize windows
keymap("n", "<C-w>z", "<cmd>WindowsMaximize<CR>")
keymap("n", "<C-w>_", "<cmd>WindowsMaximizeVertically<CR>")
keymap("n", "<C-w>|", "<cmd>WindowsMaximizeHorizontally<CR>")
keymap("n", "<C-w>=", "<cmd>WindowsEqualize<CR>")

-- =================================================
-- insert mode

-- press jk to enter
keymap("i", "jk", "<ESC>", opts)
-- save 
keymap("i", "<C-s>", ":w<CR>", opts)

-- =================================================
-- visual mode
 
-- copy/cut/paste
keymap("v", "<C-c>", '"+y', opts)
keymap("v", "<C-v>", '"_dP', opts)
-- stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- save 
keymap("v", "<C-s>", ":w<CR>", opts)

keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")


-- =================================================
-- visual_block mode

-- =================================================
-- term mode

-- =================================================
-- command mode

-- mappings.lua ends here

