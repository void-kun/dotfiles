-- return {
--     "catppuccin/nvim",
--     lazy = false,
--     name = "catppuccin",
--     priority = 1000,
--     config = function()
--         vim.cmd.colorscheme("catppuccin-frappe")
--     end,
-- }
return {
	"blazkowolf/gruber-darker.nvim",
	lazy = false,
	name = "gruber-darker",
	priority = 1000,
	opts = {
		bold = false,
		italic = {
			strings = false,
		},
	},
	config = function()
		vim.cmd.colorscheme("gruber-darker")
	end,
}
