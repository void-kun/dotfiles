--- indent-blankline.lua --- Zrik's neovim setup.
--- Code:

return {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {
        indent = { char = "│" },
        scope = { char = "│" },
    },
    config = function(_, opts)
        require("ibl").setup(opts)
    end,
}

--- indent-blankline.lua ends here
