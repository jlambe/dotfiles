return {
    -- LSP
    -- https://github.com/VonHeikemen/lsp-zero.nvim
    {
        'williamboman/mason.nvim',
        opts = {},
    },
    {
        'williamboman/mason-lspconfig.nvim',
        opts = function()
            local lsp_zero = require('lsp-zero')

            return {
                -- Define list of LSP servers to install
                ensure_installed = { 'ansiblels', 'clangd', 'cssls', 'eslint', 'html', 'intelephense', 'jsonls', 'lua_ls', 'rust_analyzer', 'ts_ls', 'yamlls' },
                handlers = {
                    lsp_zero.default_setup,
                    lua_ls = function()
                        require('lspconfig').lua_ls.setup({
                            single_file_support = true,
                            settings = {
                                Lua = {
                                    workspace = {
                                        checkThirdParty = false,
                                    },
                                },
                            },
                        })
                    end
                },
            }
        end,
    },
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v3.x',
        config = function()
            local lsp_zero = require('lsp-zero')
            lsp_zero.extend_lspconfig()

            ---@diagnostic disable-next-line: unused-local
            lsp_zero.on_attach(function(client, bufnr)
                -- see :help lsp-zero-keybindings
                -- to learn the available actions
                lsp_zero.default_keymaps({ buffer = bufnr })

                -- Hover help
                vim.keymap.set('n', 'K', function ()
                    vim.lsp.buf.hover()
                end)
                -- Signature help
                vim.keymap.set('n', 'gS', function ()
                    vim.lsp.buf.signature_help()
                end)
                -- Definition
                vim.keymap.set('n', 'gd', function ()
                    vim.lsp.buf.definition()
                end)
                -- Declaration
                vim.keymap.set('n', 'gD', function ()
                    vim.lsp.buf.declaration()
                end)
                -- Implementation
                vim.keymap.set('n', 'gi', function ()
                    vim.lsp.buf.implementation()
                end)
                -- Type definition
                vim.keymap.set('n', 'go', function ()
                    vim.lsp.buf.type_definition()
                end)
                -- References
                vim.keymap.set('n', 'gr', function ()
                    vim.lsp.buf.references()
                end)
                -- Rename
                vim.keymap.set('n', '<F6>', function()
                    vim.lsp.buf.rename()
                end, { buffer = bufnr })
                -- Code action
                vim.keymap.set('n', '<C-x>', function()
                    vim.lsp.buf.code_action()
                end, { buffer = bufnr })
                -- Format
                vim.keymap.set('n', '<F7>', function()
                    vim.lsp.buf.format()
                end, { buffer = bufnr })
                -- Diagnostics Movements
                vim.keymap.set('n', '<leader>dj', function()
                    vim.diagnostic.goto_next()
                end, { buffer = bufnr })
                vim.keymap.set('n', '<leader>dk', function()
                    vim.diagnostic.goto_prev()
                end, { buffer = bufnr })
            end)

            lsp_zero.set_sign_icons = false
        end,
    },
    {
        'neovim/nvim-lspconfig',
    },
    {
        'hrsh7th/cmp-nvim-lsp'
    },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'saadparwaiz1/cmp_luasnip',
        },
        opts = function()
            local cmp = require('cmp')

            return {
                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    { name = 'luasnip' },
                    { name = 'nvim_lua' },
                    { name = 'codeium', group_index = 1, priority = 100, },
                }, {
                    { name = 'buffer' },
                }),
                mapping = cmp.mapping.preset.insert({
                    -- 'Enter' key to confirm completion
                    ['<CR>'] = cmp.mapping.confirm({ select = true }),
                })
            }
        end
    },
    {
        'L3MON4D3/LuaSnip',
        config = function()
            require('luasnip').setup({
                update_events = { "TextChanged", "TextChangedI" },
                enable_autosnippets = true,
            })

            require("luasnip.loaders.from_lua").lazy_load({
                paths = {
                    '~/.config/nvim/snippets',
                },
            })
        end,
        keys = {
            {
                '<C-j>',
                function()
                    local ls = require('luasnip')

                    if ls.jumpable(-1) then
                        ls.jump(-1)
                    end
                end,
                mode = { 'i', 's' },
                silent = true,
                desc = 'Jump backward in snippet arguments'
            },
            {
                '<C-k>',
                function()
                    local ls = require('luasnip')

                    if ls.expand_or_jumpable() then
                        ls.expand_or_jump()
                    end
                end,
                mode = { 'i', 's' },
                silent = true,
                desc = 'Jump forward in snippet arguments'
            },
            {
                '<C-l>',
                function()
                    local ls = require('luasnip')

                    if ls.choice_active() then
                        ls.change_choice()
                    end
                end,
                mode = { 'i' },
                silent = true,
                desc = 'Cycle in snippet list of options'
            },
        },
    },
}
