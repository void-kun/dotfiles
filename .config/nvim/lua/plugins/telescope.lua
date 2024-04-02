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

			local bg_col = "#1b1b1b"
			local colors = require("rose-pine.palette")
			local telescope_color = {
				-- TelescopeMatching = { fg = colors.base },
				-- TelescopeSelection = { fg = colors.text, bg = colors.surface, bold = true },
				-- TelescopePromptPrefix = { bg = colors.surface },
				-- TelescopePromptNormal = { bg = colors.surface },
				-- TelescopeResultsNormal = { bg = colors.subtle },
				-- TelescopePreviewNormal = { bg = colors.subtle },
				TelescopePromptBorder = { bg = bg_col, fg = colors.rose },
				TelescopeResultsBorder = { bg = bg_col, fg = colors.rose },
				TelescopePreviewBorder = { bg = bg_col, fg = colors.rose },
				-- TelescopePromptTitle = { bg = colors.pine, fg = colors.subtle },
				-- TelescopeResultsTitle = { fg = colors.subtle },
				-- TelescopePreviewTitle = { bg = colors.pine, fg = colors.subtle },
			}
			for hl, col in pairs(telescope_color) do
				vim.api.nvim_set_hl(0, hl, col)
			end
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
