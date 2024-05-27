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

local py_lsp_installed, py_lsp = pcall(require, "py_lsp")
if py_lsp_installed then
    local py_lsp_loaded = false
    local py_cwd = nil
    local augroup = vim.api.nvim_create_augroup("py_lsp", { clear = true })
    vim.api.nvim_create_autocmd("BufReadPost", {
        group = augroup,
        pattern = { "*.py" },
        desc = "Lazy load py_lsp after *.py file opened and activate venv",
        callback = function()
            if not py_lsp_loaded then
                py_lsp_loaded = true
                py_cwd = vim.fn.getcwd()
                vim.defer_fn(function()
                    -- python lsp with venv using pyrigth
                    py_lsp.setup({
                        host_python = "~/environment/pyenv/bin/python",
                        language_server = "pyright",
                        default_venv_name = "pyenv",
                    })
                end, 500)
            else
                -- change venv if cwd changed since setup
                local cwd = vim.fn.getcwd()
                if cwd ~= py_cwd then
                    vim.api.nvim_command("PyLspActivateVenv")
                    py_cwd = cwd
                end
            end
        end,
    })
end

-- rust
lspconfig.rust_analyzer.setup({
    capabilities = capabilities
})

--- lspconfig.lua ends here
