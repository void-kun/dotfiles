Vim�UnDo� ,Ӯ⪍���F��Jj����	���Ւ��Z�      I    return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })                              e�o�    _�                            ����                                                                                                                                                                                                                                                                                                                                                V       e�o�     �                --[[   autocmd({ "BufEnter" }, {   	group = augroup("buf_enter"),   	pattern = { "" },   	callback = function()    		local buf_ft = vim.bo.filetype   '		if buf_ft == "" or buf_ft == nil then   W			vim.keymap.set("n", "<leader>q", "<cmd>close<cr>", { buffer = true, silent = true })   		end   	end,   }) ]]       --[[ autocmd("BufEnter", {   +	group = augroup("lsp_disable_diagnostic"),   	oattern = "*",   *	command = "lua vim.diagnostic.disable()",   )	desc = "Disable diagnostic for a while",   })   ]]5��                          �       �              5�_�                           ����                                                                                                                                                                                                                                                                                                                                                V       e�o�    �      	         end5��                         �                      5�_�                            ����                                                                                                                                                                                                                                                                                                                            	          	          V       e�o�     �                  �                  +local autocmd = vim.api.nvim_create_autocmd   1local user_cmd = vim.api.nvim_create_user_command       *-- rewrite all augroups using tis function   local function augroup(name)   F	return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })   end       autocmd("BufWritePost", {   !	group = augroup("formatonsave"),   !	desc = "Trigger format on save",   P	pattern = { "*.go", "*.lua", "*.py", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },   	callback = function()   		vim.lsp.buf.format()   	end,   })5��                                 �      �      �                          �                     5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       e�o�     �                1local user_cmd = vim.api.nvim_create_user_command5��                          ,       2               5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       e�o�    �               I    return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })�               $    group = augroup("formatonsave"),   $    desc = "Trigger format on save",   S    pattern = { "*.go", "*.lua", "*.py", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },       callback = function()           vim.lsp.buf.format()       end,5��                       �       �       �       �                         u                     5�_�                            ����                                                                                                                                                                                                                                                                                                                                                V       e�o�     �                  �                  +local autocmd = vim.api.nvim_create_autocmd       *-- rewrite all augroups using tis function   local function augroup(name)   F	return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })   end       autocmd("BufWritePost", {   !	group = augroup("formatonsave"),   !	desc = "Trigger format on save",   P	pattern = { "*.go", "*.lua", "*.py", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },   	callback = function()   		vim.lsp.buf.format()   	end,   })5��                                 �      �      �                          �                     5�_�                             ����                                                                                                                                                                                                                                                                                                                                                V       e�o�    �               I    return vim.api.nvim_create_augroup("lolo_" .. name, { clear = true })�               $    group = augroup("formatonsave"),   $    desc = "Trigger format on save",   S    pattern = { "*.go", "*.lua", "*.py", "*.rs", "*.ts", "*.tsx", "*.sh", "*.md" },       callback = function()           vim.lsp.buf.format()       end,5��                       �       �       �       �                         u                     5��