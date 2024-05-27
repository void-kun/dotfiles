--- lsp.lua --- Zrik's neovim setup.
--- Code:

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
                    "ansiblels",
                    "bashls",
                    "clangd",
                    "cmake",
                    "cssls",
                    "diagnosticls",
                    "dockerls",
                    "docker_compose_language_service",
                    "emmet_ls",
                    "gopls",
                    "grammarly",
                    "html",
                    "biome",
                    "lua_ls",
                    "autotools_ls",
                    "marksman",
                    "pyright",
                    "rust_analyzer",
                    "sqls",
                    "tailwindcss",
                    "volar",
                    "yamlls",
                },
            })
            require("mason-tool-installer").setup({
                ensure_installed = {
                    "lua-language-server",
                    "stylua",
                    "eslint_d",
                    "prettierd",
                    "gopls",
                    "golangci-lint",
                    "goimports",
                    "clang-format",
                    "black"
                },
                start_delay = 3000, -- 3 second delay
                debounce_hours = 5,
                integrations = {
                    ["mason-lspconfig"] = true,
                    ["mason-null-ls"] = true,
                    ["mason-nvim-dap"] = true,
                }
            })
        end,
    },
    {
        "neovim/nvim-lspconfig",
        event = { "VeryLazy", "BufReadPost", "BufNewFile" },
        config = function()
            require("plugins.lsp.lspconfig")
        end,
    },
    {
        'nvimdev/lspsaga.nvim',
        config = function()
            require('lspsaga').setup({})
        end,
        dependencies = {
            'nvim-treesitter/nvim-treesitter', -- optional
            'nvim-tree/nvim-web-devicons',     -- optional
        }
    },
    {
        "jose-elias-alvarez/null-ls.nvim",
        opts = function()
            return require("plugins.lsp.null-ls")
        end
    }
}

--- lsp.lua ends here
