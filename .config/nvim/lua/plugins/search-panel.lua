--- search_panel.lua --- Zrik's neovim setup.
--- Code:

return {
    "nvim-pack/nvim-spectre",
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    config = function()
        require("spectre").setup({
            is_insert_mode = true
        })
    end
}
--- search_panel.lua ends here
