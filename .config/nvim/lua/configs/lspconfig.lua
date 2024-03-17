local configs = require("nvchad.configs.lspconfig")
local util = require("lspconfig/util")

local on_attach = configs.on_attach
local on_init = configs.on_init
local capabilities = configs.capabilities

local lspconfig = require "lspconfig"
local servers = { "html", "lua_ls", "biome", "clangd", "rust_analyzer" }

for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
        on_init = on_init,
        on_attach = on_attach,
        capabilities = capabilities,
    }
end

-- LSP: golangs
lspconfig.gopls.setup {
    on_init = on_init,
    on_attach = on_attach,
    capabilities = capabilities,
    cmd = { "gopls" },
    filetypes = { "go", "gomod", "gowork", "gotmpl" },
    root_dir = util.root_pattern("go.work", "go.mod", ".git"),
    settings = {
        gopls = {
            completeUnimported = true,
            usePlaceholders = true,
            analyses = {
                unusedparams = true,
            }
        }
    }
}
