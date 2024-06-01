--- rust.lua --- Zrik's neovim setup.
--- Code:


return {
    -- lsp rust =================================================
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
                    auto = true,
                    only_current_line = false,
                    show_parameter_hints = true,
                    parameter_hints_prefix = "<- ",
                    other_hints_prefix = "=> ",
                    max_len_align = false,
                    max_len_align_padding = 1,
                    right_align = false,
                    right_align_padding = 7,
                    highlight = "Comment",
                },
            },
            server = {
                standalone = true,
            }, -- rust-analyzer options
        }
    },
    {
        'saecki/crates.nvim',
        ft = { "rust", "toml" },
        config = function(_, opts)
            local crates = require('crates')
            crates.setup(opts)
            crates.show()
        end
    }
}

--- rust.lua ends here
