local vim = vim

return {
	{
		"williamboman/mason.nvim",
		dependencies = {
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
		},
		config = function()
			require("mason").setup()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"lua_ls",
					"rust_analyzer",
					"biome",
					"tsserver",
					"gopls",
					"marksman",
				},
			})
			require("mason-tool-installer").setup({
				ensure_installed = {
					"lua-language-server",
					"stylua",
					"eslint_d",
					"prettierd",
					"biome",
					"prettierd",
					"rust-analyzer",
					"gopls",
					"golangci-lint",
					"goimports",
					"pylsp",
				},
			})
		end,
	},
	{
		"j-hui/fidget.nvim",
		dependencies = { "simrat39/rust-tools.nvim" },
		opts = {},
	},
	{
		"neovim/nvim-lspconfig",
		depends = { "nvim-lua/lsp-status.nvim" },
		config = function()
			local lspconfig = require("lspconfig")
			lspconfig.lua_ls.setup({})
			lspconfig.tsserver.setup({})

			-- lsp go
			lspconfig.gopls.setup({})
			lspconfig.golangci_lint_ls.setup({})

			-- lsp python
			lspconfig.pylsp.setup({
				settings = {
					pylsp = {
						plugins = {
							-- formatter options
							black = { enabled = true },
							autopep8 = { enabled = false },
							yapf = { enabled = false },
							-- linter options
							pylint = { enabled = true, executable = "pylint" },
							pyflakes = { enabled = false },
							pycodestyle = { enabled = false },
							-- type checker
							pylsp_mypy = { enabled = true },
							-- auto-completion options
							jedi_completion = { fuzzy = true },
							-- import sorting
							pyls_isort = { enabled = true },
						},
					},
				},
				flags = {
					debounce_text_changes = 200,
				},
			})

			local keymap = vim.keymap
			keymap.set("n", "[d", vim.diagnostic.goto_prev)
			keymap.set("n", "]d", vim.diagnostic.goto_next)

			-- Define the color
			local bg_color = "#1a1a1a"
			local fg_color = "#31748f" -- For the border foreground color

			-- Create an autocmd that sets the highlight groups when the colorscheme changes
			vim.api.nvim_create_autocmd("ColorScheme", {
				pattern = "*",
				callback = function()
					vim.api.nvim_set_hl(0, "NormalFloat", { bg = bg_color })
					vim.api.nvim_set_hl(0, "FloatBorder", { fg = fg_color, bg = bg_color })
				end,
			})

			-- Trigger the autocmd to apply the color immediately
			vim.cmd("doautocmd ColorScheme")

			-- lsp actions with telescope
			local builtin = require("telescope.builtin")

			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("UserLspConfig", {}),
				callback = function(ev)
					-- Enable completion triggered by <c-x><c-o>
					vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"

					-- Buffer local mappings.
					-- See `:help vim.lsp.*` for documentation on any of the below functions
					local opts = { buffer = ev.buf }
					vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
					vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
					vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
					vim.keymap.set("n", "D", vim.diagnostic.open_float, opts)
					vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
					vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, opts)
					vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, opts)
					vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts)
					vim.keymap.set("n", "gr", builtin.lsp_references, opts)
					vim.keymap.set("n", "<S-f>", function()
						vim.lsp.buf.format({ async = true })
					end, opts)
				end,
			})
		end,
	},
}
