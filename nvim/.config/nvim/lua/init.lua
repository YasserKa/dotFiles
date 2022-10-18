require('plugins')
require('mini.align').setup()
require('mini.bufremove').setup()
require("indent_blankline").setup()
require("which-key").setup({})
require("nvim-treesitter").setup({})
-- require("nvim-ts-rainbow").setup({})

local actions = require("telescope.actions")
-- local action_layout = require("telescope.actions.layout")
require("telescope").setup{
  defaults = {
    mappings = {
      i = {
        ["<C-[>"] = actions.close,
        ["^["] = actions.close,
        ["<Escape>"] = actions.close,
        ["<C-u>"] = false,
        -- ["<M-p>"] = action_layout.toggle_preview,
        -- ["<C-s>"] = actions.cycle_previewers_next,
        -- ["<C-a>"] = actions.cycle_previewers_prev,
      },
    },
  },
  -- pickers = {
  --   find_files = {
  --     find_command = {'fd', '-L' , "--type", "f", "--strip-cwd-prefix"}
  --   },
  -- }
}

require("nvim-treesitter.configs").setup {
  highlight = {
      -- ...
  },
  -- ...
  rainbow = {
    enable = true,
    -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
    -- colors = {}, -- table of hex strings
    -- termcolors = {} -- table of colour name strings
  }
}

vim.keymap.set('n', '<leader>c', MiniBufremove.delete)
vim.keymap.set('n', '<leader>C', '<cmd>bdelete<cr>')
vim.cmd "colorscheme gruvbox"

local wk = require("which-key")
vim.opt.termguicolors = true

wk.register({
    ["<leader>"] = {
        name = "ipython-cell", -- optional group name
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
            q = { ":SlimeSend1 exit<CR>", "" },
            m = { "<Plug>IPythonCellToMarkdown", "" },
            I = { ":IPythonCellInsertAbove<CR>o", ""},
            i = { ":IPythonCellInsertBelow<CR>o", ""},
        } 
    },
    ["[c"] = { ":IPythonCellPrevCell<CR>", "Previous Cell"},
    ["]c"] = { ":IPythonCellNextCell<CR>", "Next Cell"},
    ["<A-,>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above", mode = "i"},
    ["<A-,>ni"]  = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below", mode = "i"},
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
require('goto-preview').setup {}
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

-- LSP Installer

local wk = require("which-key")
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
        fw = { ":Rg<space>", "Search words" },

        ff = { "<CMD>Files<CR>", "Search words" },
        fb = { "<CMD>Buffers<CR>", "Search words" },
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
