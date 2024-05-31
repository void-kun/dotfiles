--- cmp.lua --- Zrik's neovim setup.
--- Code:

-- load luasnips + cmp related in insert mode only
return {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
        {
            -- snippet plugin
            "L3MON4D3/LuaSnip",
            dependencies = "rafamadriz/friendly-snippets",
            opts = { history = true, updateevents = "TextChanged,TextChangedI" },
            config = function(_, opts)
                require("luasnip").config.set_config(opts)
                -- vscode format
                require("luasnip.loaders.from_vscode").lazy_load { exclude = vim.g.vscode_snippets_exclude or {} }
                require("luasnip.loaders.from_vscode").lazy_load { paths = vim.g.vscode_snippets_path or "" }

                -- snipmate format
                require("luasnip.loaders.from_snipmate").load()
                require("luasnip.loaders.from_snipmate").lazy_load { paths = vim.g.snipmate_snippets_path or "" }

                -- lua format
                require("luasnip.loaders.from_lua").load()
                require("luasnip.loaders.from_lua").lazy_load { paths = vim.g.lua_snippets_path or "" }

                vim.api.nvim_create_autocmd("InsertLeave", {
                    callback = function()
                        if
                            require("luasnip").session.current_nodes[vim.api.nvim_get_current_buf()]
                            and not require("luasnip").session.jump_active
                        then
                            require("luasnip").unlink_current()
                        end
                    end,
                })
            end,
        },

        -- cmp sources plugins
        {
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-nvim-lua",
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
        },
    },
    opts = function()
        local cmp = require "cmp"

        local cmp_ui = {
            icons = true,
            lspkind_text = true,
            style = "default"
        }
        local cmp_style = cmp_ui.style

        local field_arrangement = {
            atom = { "kind", "abbr", "menu" },
            atom_colored = { "kind", "abbr", "menu" },
        }

        local formatting_style = {
            fields = field_arrangement[cmp_style] or { "abbr", "kind", "menu" },
            format = function(_, item)
                local icons = {
                    Namespace = "󰌗",
                    Text = "󰉿",
                    Method = "󰆧",
                    Function = "󰆧",
                    Constructor = "",
                    Field = "󰜢",
                    Variable = "󰀫",
                    Class = "󰠱",
                    Interface = "",
                    Module = "",
                    Property = "󰜢",
                    Unit = "󰑭",
                    Value = "󰎠",
                    Enum = "",
                    Keyword = "󰌋",
                    Snippet = "",
                    Color = "󰏘",
                    File = "󰈚",
                    Reference = "󰈇",
                    Folder = "󰉋",
                    EnumMember = "",
                    Constant = "󰏿",
                    Struct = "󰙅",
                    Event = "",
                    Operator = "󰆕",
                    TypeParameter = "󰊄",
                    Table = "",
                    Object = "󰅩",
                    Tag = "",
                    Array = "[]",
                    Boolean = "",
                    Number = "",
                    Null = "󰟢",
                    String = "󰉿",
                    Calendar = "",
                    Watch = "󰥔",
                    Package = "",
                    Copilot = "",
                    Codeium = "",
                    TabNine = "",
                }
                local icon = (cmp_ui.icons and icons[item.kind]) or ""

                if cmp_style == "atom" or cmp_style == "atom_colored" then
                    icon = " " .. icon .. " "
                    item.menu = cmp_ui.lspkind_text and "   (" .. item.kind .. ")" or ""
                    item.kind = icon
                else
                    icon = cmp_ui.lspkind_text and (" " .. icon .. " ") or icon
                    item.kind = string.format("%s %s", icon, cmp_ui.lspkind_text and item.kind or "")
                end

                return item
            end,
        }

        local function border(hl_name)
            return {
                { "╭", hl_name },
                { "─", hl_name },
                { "╮", hl_name },
                { "│", hl_name },
                { "╯", hl_name },
                { "─", hl_name },
                { "╰", hl_name },
                { "│", hl_name },
            }
        end

        local options = {
            completion = {
                completeopt = "menu,menuone,noselect",
            },
            window = {
                completion = {
                    side_padding = (cmp_style ~= "atom" and cmp_style ~= "atom_colored") and 1 or 0,
                    winhighlight = "Normal:CmpPmenu,CursorLine:IncSearch",
                    scrollbar = false,
                },
                documentation = {
                    border = border "CmpDocBorder",
                    winhighlight = "Normal:CmpDoc",
                },
            },
            snippet = {
                expand = function(args)
                    require("luasnip").lsp_expand(args.body)
                end,
            },

            formatting = formatting_style,

            mapping = {
                ["<C-p>"] = cmp.mapping.select_prev_item(),
                ["<C-n>"] = cmp.mapping.select_next_item(),
                ["<C-d>"] = cmp.mapping.scroll_docs(-4),
                ["<C-f>"] = cmp.mapping.scroll_docs(4),
                ["<C-Space>"] = cmp.mapping.complete(),
                ["<C-e>"] = cmp.mapping.close(),

                ["<CR>"] = cmp.mapping.confirm {
                    behavior = cmp.ConfirmBehavior.Insert,
                    select = true,
                },

                ["<Tab>"] = cmp.mapping(function(fallback)
                    if cmp.visible() then
                        cmp.select_next_item()
                    elseif require("luasnip").expand_or_jumpable() then
                        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>luasnip-expand-or-jump", true, true, true),
                            "")
                    else
                        fallback()
                    end
                end, { "i", "s" }),

                ["<S-Tab>"] = cmp.mapping(function(fallback)
                    if cmp.visible() then
                        cmp.select_prev_item()
                    elseif require("luasnip").jumpable(-1) then
                        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>luasnip-jump-prev", true, true, true), "")
                    else
                        fallback()
                    end
                end, { "i", "s" }),
            },
            sources = {
                { name = "nvim_lsp" },
                { name = "luasnip" },
                { name = "buffer" },
                { name = "nvim_lua" },
                { name = "path" },
            },
        }

        if cmp_style ~= "atom" and cmp_style ~= "atom_colored" then
            options.window.completion.border = border "CmpBorder"
        end

        return options
    end,
    config = function(_, opts)
        require("cmp").setup(opts)
    end,
}

--- cmp.lua ends here
