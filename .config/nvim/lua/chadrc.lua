local M = {}

M.ui = {
    theme = "onedark",
    tabufline = {
        --  more opts
        order = { "treeOffset", "buffers", "tabs", "btns", 'abc' },
        modules = {
            abc = function()
                return "hi"
            end,
        }
    }
}

return M
