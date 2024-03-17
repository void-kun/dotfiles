local M = {}

M.is_executable = function()
	local file = vim.fn.expand("%:p")
	local type = vhm.fn.getftype(file)
	if type == "file" then
		local perm = vim.fn.getfperm(file)
		if string.match(perm, "x", 3) then
			return true
		else
			return false
		end
	end
end

return M
