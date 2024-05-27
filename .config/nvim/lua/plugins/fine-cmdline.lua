--- fine-cmdline.lua --- Zrik's neovim setup.
--- Code:

return {
	"VonHeikemen/fine-cmdline.nvim",
	dependencies = { "MunifTanjim/nui.nvim" },
	config = function()
		vim.api.nvim_set_keymap("n", ":", "<cmd>FineCmdline<CR>", { noremap = true })
		require("fine-cmdline.").setup({
			cmdline = {
				enable_keymaps = true,
				smart_history = true,
		prompt = ": ",
			},
			popup = {
				position = {
					row = "10%",
					col = "50%",
				},
				size = {
					width = "60%",
				},
				border = {
					style = "rounded",
				},
				win_options = {
					winhighlight = "Normal:Normal,FloatBorder:FloatBorder",
				},
			},
		})
	end,
}

--- fine-cmdline.lua ends here
