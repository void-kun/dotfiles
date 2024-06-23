--- neovide.lua --- Zrik's neovim setup.
--- Code:

local vim = vim

if vim.g.neovide then
    vim.opt.linespace = 0
    vim.g.neovide_scale_factor = 1.0
    vim.g.neovide_text_gamma = 0.0
    vim.g.neovide_text_contrast = 0.5

    vim.g.neovide_padding_top = 0
    vim.g.neovide_padding_bottom = 0
    vim.g.neovide_padding_right = 0
    vim.g.neovide_padding_left = 0

    vim.g.neovide_floating_shadow = true
    vim.g.neovide_floating_z_height = 10
    vim.g.neovide_light_angle_degrees = 45
    vim.g.neovide_light_radius = 5

    vim.g.neovide_show_border = true

    vim.g.neovide_scroll_animation_length = 0.3
    vim.g.neovide_scroll_animation_far_lines = 1
    vim.g.neovide_hide_mouse_when_typing = true
    vim.g.neovide_underline_stroke_scale = 1.0

    vim.g.neovide_cursor_vfx_mode = "pixiedust"
    vim.g.neovide_cursor_vfx_particle_speed = 20.0
end

--- neovide.lua ends here
