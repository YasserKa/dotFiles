require('plugins')
require('mini.align').setup()
require('mini.bufremove').setup()
require("indent_blankline").setup()
require("which-key").setup({})
require("nvim-treesitter").setup({})
local cmp = require'cmp'
  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'path' },
      -- { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })
-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})

local saga = require('lspsaga')
saga.init_lsp_saga({
    symbol_in_winbar = {
        enable = true,
    },
})



local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")
require("telescope").setup{
    defaults = {
        mappings = {
            i = {
                ["<esc>"] = actions.close,
                ["<C-u>"] = false,
                ['<C-d>'] = require('telescope.actions').delete_buffer,
                ["<M-p>"] = action_layout.toggle_preview,
                ["<C-s>"] = actions.select_horizontal,
                ["<C-j>"] = actions.select_default,
            },
        },
    },
    pickers = {
        find_files = {
            find_command = {'fd', '-L' , "--type", "f", "--strip-cwd-prefix"}
        },
    },
    extensions = {
        ["ui-select"] = {
            require("telescope.themes").get_dropdown {
                -- even more opts
            }
        },
        fzf = {
            fuzzy = true,                    -- false will only do exact matching
            override_generic_sorter = true,  -- override the generic sorter
            override_file_sorter = true,     -- override the file sorter
            case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
            -- the default case_mode is "smart_case"
        }
    }
}
require("telescope").load_extension("ui-select")
require('telescope').load_extension('fzf')

require("nvim-treesitter.configs").setup {
    rainbow = {
        enable = true,
        extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
        max_file_lines = nil, -- Do not enable for files with more than n lines, int
    },
    context_commentstring = {
        enable = true,
        enable_autocmd = false,
    },
    textobjects = {
        select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
                -- You can use the capture groups defined in textobjects.scm
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["ac"] = "@class.outer",
                -- You can optionally set descriptions to the mappings (used in the desc parameter of
                -- nvim_buf_set_keymap) which plugins like which-key display
                ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
            },
            -- You can choose the select mode (default is charwise 'v')
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * method: eg 'v' or 'o'
            -- and should return the mode ('v', 'V', or '<c-v>') or a table
            -- mapping query_strings to modes.
            selection_modes = {
                ['@parameter.outer'] = 'v', -- charwise
                ['@function.outer'] = 'V', -- linewise
                ['@class.outer'] = '<c-v>', -- blockwise
            },
            -- If you set this to `true` (default is `false`) then any textobject is
            -- extended to include preceding or succeeding whitespace. Succeeding
            -- whitespace has priority in order to act similarly to eg the built-in
            -- `ap`.
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * selection_mode: eg 'v'
            -- and should return true of false
            include_surrounding_whitespace = true,
        },
    },
}

vim.keymap.set('n', '<leader>c', MiniBufremove.delete)
vim.keymap.set('n', '<leader>C', '<cmd>bdelete<cr>')
vim.cmd "colorscheme gruvbox"

local wk = require("which-key")
vim.opt.termguicolors = true

wk.register({
    ["<leader>"] = {
        name = "ipython-cell",
        n = {

            r = { ":w<CR>:IPythonCellRun<CR>", "Run file"},
            R = { ":w<CR>:IPythonCellRunTime<CR>", "Run file with timer" },
            c = { ":IPythonCellExecuteCell<CR>", "Execute cell" },
            vc= { " :IPythonCellExecuteVerboseCell<CR>", "Execute cell verbosely" },
            C = { ":IPythonCellExecuteCellJump<CR>", "Execute cell and jump to next" },
            vC= { " :IPythonCellExecuteCellVerboseJump<CR>", "Execute cell verbosly and jump to next" },
            l = { ":IPythonCellClear<CR>", "Clear shell" },
            x = { ":IPythonCellClose<CR>", "Close shell" },
            Q = { ":IPythonCellRestart<CR>", "Restart shell" },
            p = { ":IPythonCellPrevCommand<CR>", "Execute last command" },
            s = { ":SlimeSend1 ipython --matplotlib<CR>", "Start shell" },
            h = { "<Plug>SlimeLineSend", "Send line" },
            d = { ":SlimeSend1 %debug<CR>", "Execute cell with debug" },
            q = { ":SlimeSend1 exit<CR>", "Exit" },
            m = { "<Plug>IPythonCellToMarkdown", "To markdown" },
            I = { ":IPythonCellInsertAbove<CR>o", "Insert cell above"},
            i = { ":IPythonCellInsertBelow<CR>o", "Insert cell below"},
        } 
    },
    ["[c"] = { ":IPythonCellPrevCell<CR>", "Previous Cell"},
    ["]c"] = { ":IPythonCellNextCell<CR>", "Next Cell"},
    -- F2 is bounded to <C-,> in kitty (A hack, because tmux doesn't understand <C-,>)
    ["<F2>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above", mode = "i"},
    ["<F2>ni"]  = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below", mode = "i"},
})


require('goto-preview').setup {}
wk.register({
    ["g"] = {
        name="Go to",
        pd = { function() require('goto-preview').goto_preview_definition() end, "Preview definition"},
        pt = { function() require('goto-preview').goto_preview_type_definition() end, "Preview type"},
        pi = { function() require('goto-preview').goto_preview_implementation() end, "Preview implementation"},
        P  = { function() require('goto-preview').close_all_win() end, "Close previews"},
        pr  = { function() require('goto-preview').goto_preview_references() end, "Preview references"},
        d = { function() vim.lsp.buf.definition() end, "Show the definition of current symbol" },
        r = { function() vim.lsp.buf.references() end, "References of current symbol" },

    }
})

wk.register({
    ["K"] = { function() vim.lsp.buf.hover() end, "Hover symbol details" },
    ["<leader>la"] = { function() vim.lsp.buf.code_action() end, "LSP code action" },
    ["<leader>lf"] = { function() vim.lsp.buf.format {sync = true } end, "Format code" },
    ["<leader>lh"] = { function() vim.lsp.buf.signature_help() end, "Signature help" },
    ["<leader>lr"] = { function() vim.lsp.buf.rename() end, "Rename current symbol" },
    ["gD"] = { function() vim.lsp.buf.declaration() end, "Declaration of current symbol" },
    ["gT"] = { function() vim.lsp.buf.type_definition() end, "Definition of current type" },
    ["gI"] = { function() vim.lsp.buf.implementation() end, "Implementation of current symbol" },
    ["<leader>ld"] = { function() vim.diagnostic.open_float() end, "Hover diagnostics" },
    ["[d"] = { function() vim.diagnostic.goto_prev() end, "Previous diagnostic" },
    ["]d"] = { function() vim.diagnostic.goto_next() end, "Next diagnostic" },
    ["gl"] = { function() vim.diagnostic.open_float() end, "Hover diagnostics" },
})

-- vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
-- vim.api.nvim_create_autocmd("CursorHold", {
    --     group = "lsp_document_highlight",
    --     pattern = "<buffer>",
    --     callback = vim.lsp.buf.document_highlight,
    -- })
    -- vim.api.nvim_create_autocmd("CursorMovedI", {
        --     group = "lsp_document_highlight",
        --     pattern = "<buffer>",
        --     callback = vim.lsp.buf.clear_references,
        -- })
        -- vim.api.nvim_create_autocmd("CursorMoved", {
            --     group = "lsp_document_highlight",
            --     pattern = "<buffer>",
            --     callback = vim.lsp.buf.clear_references,
            -- })

            wk.register({
                ["<leader>la"] = { function() vim.lsp.buf.range_code_action() end, "Range LSP code action", mode = "v" },
                ["<leader>lf"] = {
                    function()
                        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, true, true), "n", false)
                        vim.lsp.buf.range_formatting()
                    end, "Range format code", mode = "v"
                } 
            })

            require("null-ls").setup({
                sources = {
                    require("null-ls").builtins.formatting.stylua,
                    require("null-ls").builtins.diagnostics.eslint,
                    require("null-ls").builtins.completion.spell,
                },
            })
            local sources = { require("null-ls").builtins.code_actions.refactoring }

            -- vim.diagnostic.config({
                --   signs = true,
                --   update_in_insert = false,
                --   underline = true,
                --   severity_sort = true,
                --   virtual_text = {
                    --     prefix = '🔥',
                    --     source = true,
                    --   },
                    -- })


                    -- TODO: Use lsp's formaters
                    -- wk.register({
                        --     ["<leader>"] = {
                            --         lf = { ":ALEFix<CR>", "Fix"}
                            --     }
                            -- })

                            wk.register({
                                ["<leader>"] = {
                                    n = {
                                        name = "python-cella",
                                        h = { "<Plug>SlimeRegionSend", "Send shell" },
                                    }
                                }
                            }, {mode = "v"})
                            require('neogen').setup({ snippet_engine = "luasnip" })

                            wk.register({
                                ["<leader>"] = {
                                    pc = { "<cmd>PackerCompile<cr>", "Packer Compile" },
                                    pi = { "<cmd>PackerInstall<cr>", "Packer Install" },
                                    ps = { "<cmd>PackerSync<cr>", "Packer Sync" },
                                    pS = { "<cmd>PackerStatus<cr>", "Packer Status" },
                                    pu = { "<cmd>PackerUpdate<cr>", "Packer Update" },
                                }
                            })

                            wk.register({
                                ["<leader>"] = {
                                    a = {
                                        name = "Annotate",
                                        ["<cr>"] = { function() require("neogen").generate() end, "Current" },
                                        c = { function() require("neogen").generate { type = "class" } end, "Class" },
                                        f = { function() require("neogen").generate { type = "func" } end, "Function" },
                                        t = { function() require("neogen").generate { type = "type" } end, "Type" },
                                        F = { function() require("neogen").generate { type = "file" } end, "File" },
                                    }
                                }
                            })
                            local capabilities = require('cmp_nvim_lsp').default_capabilities()
                            require'lspconfig'['pyright'].setup{
                                capabilities = capabilities
                            }
                            require'lspconfig'.bashls.setup{}

                            require("mason").setup()
                            require("mason-null-ls").setup()
                            -- Package Manager
                            wk.register({
                                ["<leader>"] = { 
                                    lI = { "<cmd>Mason<cr>", "LSP installer" },
                                    li = { "<cmd>LspInfo<cr>", "LSP information" },
                                }
                            })

                            require('gitsigns').setup()
                            wk.register({
                                ["<leader>"] = {
                                    g = {
                                        name = "git",
                                        j = { function() require("gitsigns").next_hunk() end, "Next git hunk" },
                                        k = { function() require("gitsigns").prev_hunk() end, "Previous git hunk" },
                                        l = { function() require("gitsigns").blame_line() end, "View git blame" },
                                        p = { function() require("gitsigns").preview_hunk() end, "Preview git hunk" },
                                        h = { function() require("gitsigns").reset_hunk() end, "Reset git hunk" },
                                        r = { function() require("gitsigns").reset_buffer() end, "Reset git buffer" },
                                        s = { function() require("gitsigns").stage_hunk() end, "Stage git hunk" },
                                        u = { function() require("gitsigns").undo_stage_hunk() end, "Unstage git hunk" },
                                        d = { function() require("gitsigns").diffthis() end, "View git diff" },
                                    }
                                }
                            })


                            wk.register({
                                ["<leader>"] = {
                                    fw = { function() require("telescope.builtin").live_grep() end, "Search words" },

                                    fW = {
                                        function()
                                            require("telescope.builtin").live_grep {
                                                additional_args = function(args) return vim.list_extend(args, { "--hidden", "--no-ignore" }) end,
                                            }
                                        end,
                                        "Search words in all files",
                                    },
                                    gt = { function() require("telescope.builtin").git_status() end, "Git status" },
                                    gb = { function() require("telescope.builtin").git_branches() end, "Git branches" },
                                    gc = { function() require("telescope.builtin").git_commits() end, "Git commits" },
                                    ff = { function() require("telescope.builtin").find_files() end, "Search files" },
                                    fF = {
                                        function() require("telescope.builtin").find_files { hidden = true, no_ignore = true } end,
                                        "Search all files",
                                    },
                                    fb = { function() require("telescope.builtin").buffers() end, "Search buffers" },
                                    fh = { function() require("telescope.builtin").help_tags() end, "Search help" },
                                    fm = { function() require("telescope.builtin").marks() end, "Search marks" },
                                    fo = { function() require("telescope.builtin").oldfiles() end, "Search history" },
                                    fc =
                                    { function() require("telescope.builtin").grep_string() end, "Search for word under cursor" },
                                    sb = { function() require("telescope.builtin").git_branches() end, "Git branches" },
                                    sh = { function() require("telescope.builtin").help_tags() end, "Search help" },
                                    sm = { function() require("telescope.builtin").man_pages() end, "Search man" },
                                    sn =
                                    { function() require("telescope").extensions.notify.notify() end, "Search notifications" },
                                    sr = { function() require("telescope.builtin").registers() end, "Search registers" },
                                    sk = { function() require("telescope.builtin").keymaps() end, "Search keymaps" },
                                    sc = { function() require("telescope.builtin").commands() end, "Search commands" },
                                    lG =
                                    { function() require("telescope.builtin").lsp_workspace_symbols() end, "Search workspace symbols" },
                                    lR = { function() require("telescope.builtin").lsp_references() end, "Search references" },
                                    lD = { function() require("telescope.builtin").diagnostics() end, "Search diagnostics" },

                                }
                            })

                            -- Visually select last inserted text
                            vim.keymap.set('n', 'gl', "`[v`]")


                            -- {{{ smart-splits.nvim
                            vim.keymap.set('n', '<C-h>', require('smart-splits').move_cursor_left)
                            vim.keymap.set('n', '<C-j>', require('smart-splits').move_cursor_down)
                            vim.keymap.set('n', '<C-k>', require('smart-splits').move_cursor_up)
                            vim.keymap.set('n', '<C-l>', require('smart-splits').move_cursor_right)

                            vim.keymap.set('n', '<C-S-h>', require('smart-splits').resize_left)
                            vim.keymap.set('n', '<C-S-j>', require('smart-splits').resize_down)
                            vim.keymap.set('n', '<C-S-k>', require('smart-splits').resize_up)
                            vim.keymap.set('n', '<C-S-l>', require('smart-splits').resize_right)
                            --- }}}
                            -- {{{ auto-pairs 
                            require("nvim-autopairs").setup({})
                            local Rule = require('nvim-autopairs.rule')
                            local npairs = require('nvim-autopairs')

                            local cond = require('nvim-autopairs.conds')
                            npairs.add_rules({
                                Rule("$", "$",{"tex", "latex"}) 
                            })

                            npairs.add_rules({
                                Rule("$$", "$$",{"tex", "latex"})
                                :with_pair(cond.not_after_regex("$"))
                            })
                            -- }}}
                            -- {{{ readline.nvim
                            local readline = require 'readline'
                            vim.keymap.set('!', '<C-k>', readline.kill_line)
                            vim.keymap.set('!', '<C-u>', readline.backward_kill_line)
                            vim.keymap.set('!', '<M-d>', readline.kill_word)
                            vim.keymap.set('!', '<M-BS>', readline.backward_kill_word)
                            vim.keymap.set('!', '<C-d>', '<Delete>')  -- delete-char
                            vim.keymap.set('!', '<C-h>', '<BS>')      -- backward-delete-char
                            vim.keymap.set('!', '<C-a>', readline.beginning_of_line)
                            vim.keymap.set('!', '<C-e>', readline.end_of_line)
                            vim.keymap.set('!', '<M-f>', readline.forward_word)
                            vim.keymap.set('!', '<M-b>', readline.backward_word)
                            vim.keymap.set('!', '<C-f>', '<Right>') -- forward-char
                            vim.keymap.set('!', '<C-b>', '<Left>')  -- backward-char
                            -- }}}
                            --- {{{ vim-maximizer
                            wk.register({
                                ["<C-w>"] = {
                                    name = "Window",
                                    m = { "<cmd>MaximizerToggle!<CR>", "Full Maximizer"},
                                }
                            })
                            --- }}}



                            -- disable netrw at the very start of your init.lua (strongly advised)
                            vim.g.loaded_netrw = 1
                            vim.g.loaded_netrwPlugin = 1

                            -- empty setup using defaults
                            require("nvim-tree").setup()


                            wk.register({
                                ["yex"] = { "<cmd>NvimTreeToggle<cr>", "Toggle Explorer" },
                                ["<leader>o"] = { "<cmd>NvimTreeFocus<cr>", "Toggle Focus" },
                            })

                            -- OR setup with some options
                            require("nvim-tree").setup({
                                sort_by = "case_sensitive",
                                view = {
                                    adaptive_size = true,
                                    mappings = {
                                        list = {
                                            { key = "u", action = "dir_up" },
                                        },
                                    },
                                },
                                renderer = {
                                    group_empty = true,
                                },
                                filters = {
                                    dotfiles = true,
                                },
                            })
