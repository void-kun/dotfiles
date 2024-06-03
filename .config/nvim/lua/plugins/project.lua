--- project.lua --- Zrik's neovim setup.
--- Code:

return {
    "coffebar/neovim-project",
    opts = {
        projects = {
            "~/.config/nvim",
            -- personal projects
            "~/code/*",
            "~/source/*/*",
            "~/code/bookcrawler/bws"
        },
        last_session_on_startup = false,
        filetype_autocmd_timeout = 200,
    },
    init = function()
        vim.opt.sessionoptions:append("globals")
    end,
    dependencies = {
        { "nvim-lua/plenary.nvim" },
        { "nvim-telescope/telescope.nvim" },
        { "Shatur/neovim-session-manager" },
    },
    lazy = false,
    priority = 100,
}

--- project.lua ends here
