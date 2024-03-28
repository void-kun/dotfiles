local vim = vim
local autocmd = vim.api.nvim_create_autocmd

-- rewrite all augroups using tis function
local function augroup(name)
	return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })
end

autocmd("BufWritePost", {
	group = augroup("formatonsave"),
	desc = "Trigger format on save",
	pattern = { "*.go", "*.lua", "*.py", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },
	callback = function()
		vim.lsp.buf.format()
	end,
})
