local vim = vim

function ColorMyPencils(color)
	color = color or "rose-pine"
	vim.cmd.colorscheme(color)

	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

return {
	{
		"jacoborus/tender.vim",
		name = "tender",
		config = function()
			-- require("tender").setup({})

			vim.cmd("colorscheme tender")
		end,
	},
}
