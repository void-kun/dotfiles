--- lspconfig.lua --- Zrik's neovim setup.
--- Code:

local lsp_installed, lspconfig = pcall(require, "lspconfig")
if not lsp_installed then
    return
end

-- Specify how the border looks like
local border = {
    { '┌', 'FloatBorder' },
    { '─', 'FloatBorder' },
    { '┐', 'FloatBorder' },
    { '│', 'FloatBorder' },
    { '┘', 'FloatBorder' },
    { '─', 'FloatBorder' },
    { '└', 'FloatBorder' },
    { '│', 'FloatBorder' },
}

-- Add border to the diagnostic popup window
vim.diagnostic.config({
    virtual_text = {
        prefix = '■ ',
    },
    float = { border = border },
})

-- LSP diagnostic opthons setup
vim.fn.sign_define({
    {
        name = "DiagnosticSignError",
        text = " ",
        texthl = "DiagnosticSignError",
        linehl = "ErrorLine",
    },
    {
        name = "DiagnosticSignWarn",
        text = " ",
        texthl = "DiagnosticSignWarn",
        linehl = "WarningLine",
    },
    {
        name = "DiagnosticSignInfo",
        text = " ",
        texthl = "DiagnosticSignInfo",
        linehl = "InfoLine",
    },
    {
        name = "DiagnosticSignHint",
        text = "󰛩 ",
        texthl = "DiagnosticSignHint",
        linehl = "HintLine",
    },
})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    update_in_insert = true,
    underline = true,
    serverity_sort = false,
    float = {
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
    },
})

vim.api.nvim_set_option("updatetime", 200)

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])

vim.api.nvim_set_hl(0, "FloatBorder", { fg = "#8A8786" })
vim.api.nvim_set_hl(0, "NormalFloat", { fg = "#8a8786" })
vim.api.nvim_set_hl(0, "WinSeparator", { fg = "#ffffff" })

----------------------------- language servers --------------------------------

--Enable (broadcasting) snippet capability for completion
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- scripts --------------------------------------------------------------------
-- bash, requires bash-language-server
lspconfig.bashls.setup({
    capabilities = capabilities
})

-- web ------------------------------------------------------------------------
-- javascript linter, requires vscode-langservers-extracted
lspconfig.eslint.setup({})

-- css, html
lspconfig.cssls.setup({ capabilities = capabilities })
lspconfig.html.setup({ capabilities = capabilities })
lspconfig.emmet_ls.setup({
    capabilities = capabilities,
    filetypes = { "html", "typescriptreact", "javascriptreact", "css", "sass", "scss", "less", },
})

-- json
lspconfig.biome.setup({
    capabilities = capabilities,
    provideFormatter = true,
})

-- languages ------------------------------------------------------------------
-- go
lspconfig.gopls.setup({
    capabilities = capabilities
})

-- c/cpp
lspconfig.clangd.setup({
    capabilities = capabilities,
    root_dir = function(fname)
        return require("lspconfig.util").root_pattern(
            "Makefile",
            "configure.ac",
            "configure.in",
            "config.h.in",
            "meson.build",
            "meson_options.txt",
            "build.ninja"
        )(fname) or require("lspconfig.util").root_pattern("compile_commands.json", "compile_flags.txt")(
            fname
        ) or require("lspconfig.util").find_git_ancestor(fname)
    end,
})

-- lua
lspconfig.lua_ls.setup({
    capabilities = capabilities
})

-- pyright lsp for python
lspconfig.pyright.setup({
    capabilities = capabilities,
})

-- rust
lspconfig.rust_analyzer.setup({
    capabilities = capabilities,
    settings = {
        ['rust-analyzer'] = {
            cargo = {
                allFeatures = true,
            }
        }
    }
})

--- lspconfig.lua ends here
