--- todo.lua --- Zrik's neovim setup.
--- Code:

return {
	"folke/todo-comments.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	config = function()
		require("todo-comments").setup()
	end,
}


--- todo.lua ends here
