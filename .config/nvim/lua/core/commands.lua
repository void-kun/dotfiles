local autocmd = vim.api.nvim_create_autocmd
local user_cmd = vim.api.nvim_create_user_command

-- rewrite all augroups using tis function
local function augroup(name)
	return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })
end

user_cmd("DiagnosticToggle", function()
	local config = vim.diagnostic.config
	local vt = config().virtual_text
	config({
		virtual_text = not vt,
		underline = not vt,
		signs = not vt,
	})
end, { desc = "toggle diagnostic" })

autocmd({ "BufEnter" }, {
	group = augroup("buf_enter"),
	pattern = { "" },
	callback = function()
		local buf_ft = vim.bo.filetype
		if buf_ft == "" or buf_ft == nil then
			vim.keymap.set("n", "<leader>q", "<cmd>close<cr>", { buffer = true, silent = true })
		end
	end,
})

-- Highlight text on yank
augroup("YankHighlight")
autocmd("TextYankPost", {
	group = augroup("YankHighlight"),
	callback = function()
		vim.highlight.on_yank({ higroup = "IncSearch", timeout = "700" })
	end,
	desc = "Highlight yanked text",
})

autocmd("BufEnter", {
	group = augroup("lsp_disable_diagnostic"),
	pattern = "*",
	command = "lua vim.diagnostic.disable()",
	desc = "Disable diagnostic for a while",
})

-- augroup('formatonsave', { clear = true })
autocmd("BufWritePost", {
	group = augroup("formatonsave"),
	desc = "Trigger format on save",
	pattern = { "*.go", "*.lua", "*.py", "*.rb", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },
	callback = function()
		vim.lsp.buf.format()
	end,
})

autocmd({ "VimEnter", "CursorMoved" }, {
	group = augroup("yankpost"),
	pattern = "*",
	callback = function()
		cursor_pos = vim.fn.getpos(".")
	end,
	desc = "Stores cursor position",
})

autocmd("TextYankPost", {
	pattern = "*",
	group = augroup("yankpost"),
	callback = function()
		if vim.v.event.operator == "y" then
			vim.fn.setpos(".", cursor_pos)
		end
	end,
})

autocmd({ "VimResized" }, {
	group = augroup("vimresized"),
	pattern = "*",
	callback = function()
		vim.schedule(function()
			vim.cmd("tabdo wincmd =")
		end)
	end,
	desc = "wondows in equal size",
})

autocmd({ "CursorHold" }, {
	group = augroup("start_luasnip"),
	callback = function()
		local status_ok, luasnip = pcall(require, "luasnip")
		if not status_ok then
			return
		end
		if luasnip.expand_or_jumpable() then
			-- ask maintainer for option to make this silent
			-- luasnip.unlink_current()
			vim.cmd([[silent! lua require("luasnip").unlink_current()]])
		end
	end,
	desc = "Start luasnip",
})

autocmd("FileType", {
	group = augroup("easy_quit"),
	pattern = {
		"help",
		"man",
		"lspinfo",
		"checkhealth",
	},
	callback = function(event)
		vim.bo[event.buf].buflisted = false
		-- vim.keymap.set('n', '<leader>q', '<cmd>q<cr>', { buffer = event.buf, silent = true })
		vim.keymap.set("n", "<leader>q", "<cmd>q<cr>", { buffer = true, silent = true })
	end,
})

autocmd("BufWritePre", {
	group = augroup("write_pre"),
	callback = function(event)
		if event.match:match("^%w%w+://") then
			return
		end
		local file = vim.loop.fs_realpath(event.match) or event.match
		vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
	end,
	desc = "Create dir during file save",
})

-- Check for spelling in text filetypes and enable wrapping, and set gj and gk keymaps
autocmd("FileType", {
	group = augroup("set_wrap"),
	pattern = {
		"gitcommit",
		"markdown",
		"text",
	},
	callback = function()
		local opts = { noremap = true, silent = true }
		vim.opt_local.spell = true
		vim.opt_local.wrap = true
		vim.api.nvim_buf_set_keymap(0, "n", "j", "gj", opts)
		vim.api.nvim_buf_set_keymap(0, "n", "k", "gk", opts)
	end,
})

autocmd("BufReadPost", {
	group = augroup("restore_position"),
	callback = function()
		local exclude = { "gitcommit" }
		local buf = vim.api.nvim_get_current_buf()
		if vim.tbl_contains(exclude, vim.bo[buf].filetype) then
			return
		end
		local mark = vim.api.nvim_buf_get_mark(0, '"')
		local lcount = vim.api.nvim_buf_line_count(0)
		if mark[1] > 0 and mark[1] <= lcount then
			pcall(vim.api.nvim_win_set_cursor, 0, mark)
			vim.api.nvim_feedkeys("zz", "n", true)
		end
	end,
	desc = "Go to the last loc when opening a buffer",
})

autocmd({
	"FocusGained",
	"TermClose",
	"TermLeave",
}, {
	group = augroup("checktime"),
	command = "checktime",
	group = "userconf",
	desc = "Check if the file needs to be reloaded when it's changed",
})

autocmd({
	"BufEnter",
	"FocusGained",
	"InsertLeave",
	"CmdlineLeave",
	"WinEnter",
}, {
	pattern = "*",
	group = augroup("EnableRelativenumber"),
	callback = function()
		if vim.o.nu and vim.api.nvim_get_mode().mode ~= "i" then
			vim.opt.relativenumber = true
		end
	end,
	desc = "Enable relative number in normal mode",
})

autocmd({
	"BufLeave",
	"FocusLost",
	"InsertEnter",
	"CmdlineEnter",
	"WinLeave",
}, {
	pattern = "*",
	group = augroup("DisableRelativenumber"),
	callback = function()
		if vim.o.nu then
			vim.opt.relativenumber = false
			vim.cmd("redraw")
		end
	end,
	desc = "Disable relative number in insert mode",
})

autocmd("RecordingEnter", {
	group = augroup("record_action"),
	pattern = "*",
	command = "lua vim.opt_local.cmdheight = 1",
	desc = "Show recording status",
})

autocmd("RecordingLeave", {
	group = augroup("record_leave"),
	pattern = "*",
	command = "lua vim.opt_local.cmdheight = 0",
	desc = "Show recording status",
})

autocmd("BufWritePost", {
	group = augroup("make-executable"),
	pattern = { "*.sh", "*.zsh", "*.py" },
	-- command = [[!chmod +x %]],
	callback = function()
		local file = vim.fn.expand("%p")
		local status = require("core.utils").is_executable()
		if status ~= true then
			vim.fn.setfperm(file, "rwxr-x---")
		end
	end,
	desc = "Make files ended with *.sh, *.py executable",
})
