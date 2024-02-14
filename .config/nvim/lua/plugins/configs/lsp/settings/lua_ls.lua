-- lua_ls.lua --------------------------------

return {
    settings = {
        lua = {
            diagnostics = {
                globals = { "vim" },
            },
            workspace = {
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.stdpath("config") .. "/lua"] = true,
                }
            },
            telemetry = {
                enable = false,
            }
        }
    }
}

-- lua_ls.lua ends here
