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

autocmd("BufWritePost", {
	group = augroup("formatonsave"),
	desc = "Trigger format on save",
	pattern = { "*.go", "*.lua", "*.py", "*.rb", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },
	callback = function()
		vim.lsp.buf.format()
	end,
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
		vim.keymap.set("n", "<leader>q", "<cmd>q<cr>", { buffer = true, silent = true })
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
