--- treesitter.lua --- Zrik's neovim setup.
--- Code:

return {
    "nvim-treesitter/nvim-treesitter",
    event = { "BufReadPre", "BufNewFile" },
    build = ":TSUpdate",
    dependencies = {
        "windwp/nvim-ts-autotag",
        "axelvc/template-string.nvim",
    },
    config = function()
        local config = require("nvim-treesitter.configs")
        config.setup({
            ensure_installed = {
                -- scripts
                "bash",
                -- docs
                "markdown",
                "markdown_inline",
                "mermaid",
                -- web
                "tsx",
                "typescript",
                "javascript",
                "html",
                "css",
                "scss",
                "vue",
                -- language
                "lua",
                "rust",
                "go",
                "c",
                "cpp",
                "python",
                -- others
                "xml",
                "json",
                "yaml",
                "regex",
                "diff",
                "csv",
                "dockerfile",
                "gomod",
                "ini",
                "luadoc",
                "make",
                "sql",
                "vim",
                "vimdoc"
            },
            sync_install = false,
            auto_install = true,
            highlight = { enable = true, additional_vim_regex_highlighting = false },
            indent = { enable = true },
            rainbow = {
                enable = true,
                extended_mode = true,
                max_file_lines = nil,
            },
            autotag = {
                enable = true,
            },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "<cr>",
                    node_incremental = "<cr>",
                    scope_incremental = false,
                    node_decremental = "<bs>",
                },
            },
        })

        require("template-string").setup({})

        -- fold
        vim.opt.foldmethod = "expr"
        vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
        vim.opt.foldenable = false
    end,
}

--- treesitter.lua ends here
