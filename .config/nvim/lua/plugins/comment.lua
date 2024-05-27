--- comment.lua --- Zrik's neovim setup.
--- Code:

return {
	"numToStr/Comment.nvim",
	lazy = false,
	config = function()
		require("Comment").setup()
	end,
}

--- comment.lua ends here
