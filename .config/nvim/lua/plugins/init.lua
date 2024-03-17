return {
    {
        "stevearc/conform.nvim",
        config = function()
            require "configs.conform"
        end,
    },

    {
        "nvim-tree/nvim-tree.lua",
        opts = {
            git = { enable = true },
        },
    },

    {
        "williamboman/mason.nvim",
        opts = {
            ensure_installed = {
                -- lsp
                "rust-analyzer",
                "gopls",
                "biome",
                "lua-language-server",
                "clangd",
                -- formatter
                "prettier",
                "stylua"
            }
        }
    },

    {
        "nvim-treesitter/nvim-treesitter",
        opts = {
            ensure_installed = {
                -- defaults
                "vim",
                "lua",
                "vimdoc",
                -- web dev
                "html",
                "css",
                "javascript",
                "typescript",
                "tsx",
                -- low level
                "c",
                "rust",
                "go",
            },
        },
    },

    {
        "neovim/nvim-lspconfig",
        config = function()
            require("nvchad.configs.lspconfig").defaults()
            require "configs.lspconfig"
        end,
    },

    {
        "mfussenegger/nvim-dap",
    },

    {
        "hrsh7th/nvim-cmp",
        opts = function()
            local M = require("nvchad.configs.cmp")
            table.insert(M.sources, { name = "crates" })
            return M
        end
    },

    {
        "rust-lang/rust.vim",
        ft = "rust",
        init = function()
            vim.g.rustfmt_autosave = 1
        end
    },

    {
        "simrat39/rust-tools.nvim",
        ft = "rust",
        dependencies = "neovim/nvim-lspconfig",
        opts = function()
            local configs = require("nvchad.configs.lspconfig")
            return {
                on_init = configs.on_init,
                on_attach = configs.on_attach,
                capabilities = configs.capabilities,
            }
        end,
        config = function(_, opts)
            require("rust-tools").setup(opts)
        end
    },

    {
        "saecki/crates.nvim",
        ft = { "rust", "toml" },
        config = function(_, opts)
            local crates = require("crates")
            crates.setup(opts)
            crates.show()
        end
    },

    {
        "jose-elias-alvarez/null-ls.nvim",
        ft = "go",
        opts = function ()
            local null_ls = require("null-ls")
            local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

            local opts = {
                sources = {
                    null_ls.builtins.formatting.gofumpt,
                    null_ls.builtins.formatting.goimports_reviser,
                    null_ls.builtins.formatting.golines,
                },
                on_attach = function (client, bufnr)
                    if client.supports_method("textDocument/formatting") then
                        vim.api.nvim_clear_autocmds({
                            group = augroup,
                            buffer = bufnr,
                        })
                        vim.api.nvim_create_autocmd("BufWritePre", {
                            group = augroup,
                            buffer = bufnr,
                            callback = function ()
                                vim.lsp.buf.format({ bufnr = bufnr })
                            end,
                        })
                    end
                end,
            }
            return opts
        end
    },

    {
        "leoluz/nvim-dap-go",
        ft = "go",
        dependencies = "mfussenegger/nvim-dap",
        config = function (_, opts)
            require("dap-go").setup(opts)
        end
    },

    {
        "olexsmir/gopher.nvim",
        ft = "go",
        config = function (_, opts)
            require("gopher").setup(opts)
        end,
        build = function ()
            vim.cmd [[silent! GoInstallDeps]]
        end
    }
}
