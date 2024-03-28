local vim = vim

if vim.g.neovide then
	vim.opt.linespace = 4
	vim.g.neovide_scale_factor = 1.0

	vim.g.neovide_padding_top = 0
	vim.g.neovide_padding_bottom = 0
	vim.g.neovide_padding_right = 0
	vim.g.neovide_padding_left = 0

	-- Helper function for transparency formatting
	local alpha = function()
		return string.format("%x", math.floor((255 * vim.g.transparency) or 0.8))
	end
	-- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
	vim.g.neovide_transparency = 0.9
	vim.g.transparency = 0.9
	vim.g.neovide_background_color = "#0f1117" .. alpha()
	vim.g.neovide_window_blurred = true

	vim.g.neovide_floating_blur_amount_x = 2.0
	vim.g.neovide_floating_blur_amount_y = 2.0

	vim.g.neovide_show_border = true
	vim.g.neovide_scroll_animation_length = 0.3
	vim.g.neovide_scroll_animation_far_lines = 1
end
