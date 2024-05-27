--- telescope.lua --- Zrik's neovim setup.
--- Code:

return {
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.5",
		dependencies = { "nvim-lua/plenary.nvim" },

		config = function()
			local actions = require("telescope.actions")
			require("telescope").setup({
				defaults = {
					mappings = {
						i = {
							["Esc"] = actions.close,
						},
					},
					file_ignore_patterns = {
						".git",
						"lazy-lock.json",
						"node_modules",
						"yarn.lock",
						"target",
					},
					dynamic_preview_title = true,
					path_display = { "smart" },
				},
				pickers = {
					find_files = {
						hidden = true,
					},
					buffers = {
						sort_lastused = true,
					},
				},
				layout_config = {
					horizontal = {
						preview_cutoff = 100,
						preview_width = 0.5,
					},
				},
			})
		end,
	},
	{
		"nvim-telescope/telescope-ui-select.nvim",
		config = function()
			require("telescope").setup({
				extensions = {
					["ui-select"] = {
						require("telescope.themes").get_dropdown({
							-- even more opts
						}),
					},
				},
			})
			require("telescope").load_extension("ui-select")
		end,
	},
}

--- telescope.lua ends here
