return {
	"mg979/vim-visual-multi",
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		opts = {},
		config = function()
			require("ibl").setup()
		end,
	},
	{ "romainl/vim-cool", lazy = false },
}
