return {
	"loctvl842/monokai-pro.nvim",
	lazy = false, -- make sure we load this during startup if it is your main colorscheme
	priority = 1000, -- make sure to load this before all the other start plugins
	config = function()
		require("monokai-pro").setup({})

		vim.cmd("colorscheme monokai-pro")
	end,
}
