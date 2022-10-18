require('plugins')
require('mini.align').setup()
require('mini.bufremove').setup()
require("indent_blankline").setup()
require("which-key").setup({})
require("nvim-treesitter").setup({})

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
            },
            n = {
                ['<C-d>'] = require('telescope.actions').delete_buffer,
                ["<CR>"] = actions.select_default,
            },
        },
    },
    pickers = {
      find_files = {
        find_command = {'fd', '-L' , "--type", "f", "--strip-cwd-prefix"}
      },
    }
}

require("nvim-treesitter.configs").setup {
    rainbow = {
        enable = true,
        extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
        max_file_lines = nil, -- Do not enable for files with more than n lines, int
    }
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
    ["<A-,>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above", mode = "i"},
    ["<A-,>ni"]  = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below", mode = "i"},
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
    ["<leader>"] = {
        lf = { ":ALEFix<CR>", "Fix"}
    }
})

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
require'lspconfig'.pyright.setup{}
require'lspconfig'.bashls.setup{}

require("mason").setup()
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

-- {{{ smart-splits.nvim
vim.keymap.set('n', '<C-h>', require('smart-splits').move_cursor_left)
vim.keymap.set('n', '<C-j>', require('smart-splits').move_cursor_down)
vim.keymap.set('n', '<C-k>', require('smart-splits').move_cursor_up)
vim.keymap.set('n', '<C-l>', require('smart-splits').move_cursor_right)

vim.keymap.set('n', '<C-S-h>', require('smart-splits').resize_left)
vim.keymap.set('n', '<C-S-j>', require('smart-splits').resize_down)
vim.keymap.set('n', '<C-S-k>', require('smart-splits').resize_up)
vim.keymap.set('n', '<C-S-l>', require('smart-splits').resize_right)
--- }}
-- {{{ auto-pairs 
require("nvim-autopairs").setup({})
local Rule = require('nvim-autopairs.rule')
local npairs = require('nvim-autopairs')

local cond = require('nvim-autopairs.conds')
npairs.add_rules({
  Rule("$", "$",{"tex", "latex"}) }
)

npairs.add_rules({
  Rule("$$", "$$",{"tex", "latex"})
    :with_pair(cond.not_after_regex("$"))
  })
-- }}
