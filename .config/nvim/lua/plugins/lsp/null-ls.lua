--- null-ls.lua --- Zrik's neovim setup.
--- Code:

local null_ls = require("null-ls")

local opts = {
    sources = {
        -- python
        null_ls.builtins.formatting.black,
        -- go
        null_ls.builtins.formatting.gofumpt,
        null_ls.builtins.formatting.goimports_reviser,
        null_ls.builtins.formatting.golines,
        -- c/c++
null_ls.builtins.formatting.clang_format
    },
    on_attach = function(client, bufnr)
        if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({
                group = augroup,
                buffer = bufnr,
            })
            vim.api.nvim_create_autocmd("BufWritePre", {
                group = augroup,
                buffer = bufnr,
                callback = function()
                    vim.lsp.buf.format({ bufnr = bufnr })
                end
            })
        end
    end
}

return opts

--- null-ls.lua ends here
