return {
	"iamcco/markdown-preview.nvim",
	cmd = { "MardownPreviewToggle", "MardownPreview", "MardownPreviewStop" },
	ft = { "markdown", "md" },
	build = function()
		vim.fn["mkdp#util#install"]()
	end,
}
