-- indent_blankline.lua --------------------------------

local ok, ibl = pcall(require, "ibl")
if not ok then
	return
end

local highlight = {
    "CursorColumn",
    "Whitespace",
}

ibl.setup({
    indent = { highlight = highlight, char = "" },
    whitespace = {
        highlight = highlight,
        remove_blankline_trail = false,
    },
    scope = { enabled = false },
})
-- indent_blankline.lua ends here
