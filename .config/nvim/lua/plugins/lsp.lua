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
    },
    {
        'rust-lang/rust.vim',
        ft = "rust",
        init = function()
            vim.g.rustfmt_autosave = 1
        end
    },
    {
        'simrat39/rust-tools.nvim',
        dependencies = {
            "neovim/nvim-lspconfig"
        },
        ft = "rust",
        opts = {
  tools = {
    inlay_hints = {
      -- automatically set inlay hints (type hints)
      -- default: true
      auto = true,

      -- Only show inlay hints for the current line
      only_current_line = false,

      -- whether to show parameter hints with the inlay hints or not
      -- default: true
      show_parameter_hints = true,

      -- prefix for parameter hints
      -- default: "<-"
      parameter_hints_prefix = "<- ",

      -- prefix for all the other hints (type, chaining)
      -- default: "=>"
      other_hints_prefix = "=> ",

      -- whether to align to the length of the longest line in the file
      max_len_align = false,

      -- padding from the left if max_len_align is true
      max_len_align_padding = 1,

      -- whether to align to the extreme right or not
      right_align = false,

      -- padding from the right if right_align is true
      right_align_padding = 7,

      -- The color of the hints
      highlight = "Comment",
    },
  },
  server = {
    -- standalone file support
    -- setting it to false may improve startup time
    standalone = true,
  }, -- rust-analyzer options
        }
    }
}

--- lsp.lua ends here
