--- undotree.lua --- Zrik's neovim setup.
--- Code:

return {
	"mbbill/undotree",
    config = function()
		vim.keymap.set('n', '<leader><F5>', vim.cmd.UndotreeToggle)
	end,
}

--- undotree.lua ends here
