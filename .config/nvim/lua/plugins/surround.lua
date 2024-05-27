--- surround.lua --- Zrik's neovim setup.
--- Code:

return {
	"kylechui/nvim-surround",
	version = "*",
	event = "VeryLazy",
	config = function()
		require("nvim-surround").setup({})
	end,
}

--- surround.lua ends here
