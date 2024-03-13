local function safeRequire(module)
	local success, loadedModule = pcall(require, module)
	if success then
		return loadedModule
	end
	print("Error loading " .. module)
end

safeRequire("core.options")
safeRequire("core.keymaps")
safeRequire("core.commands")
safeRequire("core.bootstrap")
