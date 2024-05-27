--- lspconfig.lua --- Zrik's neovim setup.
--- Code:

local lsp_installed, lspconfig = pcall(require, "lspconfig")
if not lsp_installed then
    return
end

-- enable logging
vim.lsp.set_log_level("info")

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
    capabilities = capabilities
})

--- lspconfig.lua ends here
