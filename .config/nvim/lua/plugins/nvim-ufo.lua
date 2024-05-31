--- nvim-ufo.lua --- Zrik's neovim setup.
--- Code:

return {
    "kevinhwang91/nvim-ufo",
    dependencies = {
        "kevinhwang91/promise-async"
    },
    config = function()
        vim.o.foldcolumn = '1'
        vim.o.foldlevel = 99
        vim.o.foldlevelstart = 99
        vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]
        vim.o.foldenable = true

        vim.keymap.set('n', "zR", require('ufo').openAllFolds)
        vim.keymap.set('n', "zM", require('ufo').closeAllFolds)

        -- Setup UFO with treesitter
        require('ufo').setup({
            provider_selector = function(bufnr, filetype, buftype)
                return { 'treesitter', 'indent' }
            end
        })
    end
}

--- nvim-ufo.lua ends here
