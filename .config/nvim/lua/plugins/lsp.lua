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
                    "diagnosticls",
                    "dockerls",
                    "docker_compose_language_service",
                    "gopls",
                    "grammarly",
                    "lua_ls",
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
    },
    {
        "rcarriga/nvim-dap-ui",
        dependencies = {
            "mfussenegger/nvim-dap",
            "nvim-neotest/nvim-nio"
        },
        config = function()
            local dap, dapui = require("dap"), require("dapui")
            dap.listeners.before.attach.dapui_config = function()
                dapui.open()
            end
            dap.listeners.before.launch.dapui_config = function()
                dapui.open()
            end
            dap.listeners.before.event_terminated.dapui_config = function()
                dapui.close()
            end
            dap.listeners.before.event_exited.dapui_config = function()
                dapui.close()
            end
        end
    },
}

--- lsp.lua ends here
