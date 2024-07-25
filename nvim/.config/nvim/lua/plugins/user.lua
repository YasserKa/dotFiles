-- You can also add or configure plugins by creating files in this `plugins/` folder
-- Here are some examples:

---@type LazySpec
return {

  -- == Examples of Adding Plugins ==

  -- You can disable default plugins as follows:
  { "max397574/better-escape.nvim", enabled = false },
  { "goolord/alpha-nvim", enabled = false },
  { "stevearc/aerial.nvim", enabled = false },

  -- colorscheme
  { "EdenEast/nightfox.nvim" },

  -- You can also easily customize additional setup of plugins that is outside of the plugin's setup call
  {
    "L3MON4D3/LuaSnip",
    config = function(plugin, opts)
      require "astronvim.plugins.configs.luasnip"(plugin, opts) -- include the default astronvim config that calls the setup call
      -- add more custom luasnip configuration such as filetype extend or custom snippets
      local luasnip = require "luasnip"
      luasnip.filetype_extend("javascript", { "javascriptreact" })
    end,
  },

  {
    "windwp/nvim-autopairs",
    config = function(plugin, opts)
      require "astronvim.plugins.configs.nvim-autopairs"(plugin, opts) -- include the default astronvim config that calls the setup call
      -- add more custom autopairs configuration such as custom rules
      local npairs = require "nvim-autopairs"
      local Rule = require "nvim-autopairs.rule"
      local cond = require "nvim-autopairs.conds"
      npairs.add_rules(
        {
          Rule("$", "$", { "tex", "latex", "plaintex" })
            -- don't add a pair if the next character is %
            :with_pair(cond.not_after_regex "%%")
            -- don't add a pair if  the previous character is xxx
            :with_pair(
              cond.not_before_regex("xxx", 3)
            )
            -- don't move right when repeat character
            :with_move(cond.none())
            -- don't delete if the next character is xx
            :with_del(cond.not_after_regex "xx")
            -- disable adding a newline when you press <cr>
            :with_cr(cond.none()),
        },
        -- disable for .vim files, but it work for another filetypes
        Rule("a", "a", "-vim")
      )
    end,
  },
  -- It's bugged in stable channel
  -- https://github.com/AstroNvim/AstroNvim/issues/1376 fixes in nightly
  -- nightly has its own issues
  -- https://github.com/AstroNvim/AstroNvim/issues/1523
  { -- override nvim-cmp plugin
    "hrsh7th/nvim-cmp",
    dependencies = { "https://github.com/kdheepak/cmp-latex-symbols" },
    -- override the options table that is used in the `require("cmp").setup()` call
    opts = function(_, opts)
      -- opts parameter is the default options table
      -- the function is lazy loaded so cmp is able to be required
      local cmp = require "cmp"
      -- modify the mapping part of the table

      opts.mapping["<A-k>"] = cmp.mapping(cmp.mapping.scroll_docs(-5), { "i", "c" })
      opts.mapping["<A-j>"] = cmp.mapping(cmp.mapping.scroll_docs(5), { "i", "c" })

      function Has_words_before()
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
      end

      opts.mapping["<C-k>"] = cmp.mapping(function(fallback)
        if require("luasnip").jumpable(-1) then
          require("luasnip").jump(-1)
        else
          fallback()
        end
      end, {
        "i",
        "s",
      })
      opts.mapping["<C-j>"] = cmp.mapping(function(fallback)
        if require("luasnip").expandable() then
          cmp.confirm()
        elseif require("luasnip").expand_or_jumpable() then
          require("luasnip").expand_or_jump()
        elseif Has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, {
        "i",
        "s",
      })
      opts.sources = cmp.config.sources {
        { name = "luasnip", priority = 1000 },
        { name = "nvim_lsp", priority = 750 },
        { name = "latex_symbols", priority = 500 },
        { name = "buffer", priority = 500 },
        { name = "path", priority = 250 },
      }

      -- return the new table to be used
      return opts
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    opts = function()
      local actions = require "telescope.actions"
      local get_icon = require("astroui").get_icon
      return {
        defaults = {
          prompt_prefix = string.format("%s ", get_icon "Search"),
          selection_caret = string.format("%s ", get_icon "Selected"),
          path_display = { "truncate" },
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = {
              prompt_position = "top",
              preview_width = 0.55,
            },
            vertical = {
              mirror = false,
            },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
          },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
              ["<C-u>"] = false,
              ["<C-[>"] = actions.close,
              ["<C-j>"] = actions.select_default,
              ["<C-d>"] = actions.delete_buffer,
              ["<C-s>"] = actions.select_horizontal,
              ["<C-n>"] = actions.move_selection_next,
              ["<C-p>"] = actions.move_selection_previous,
              ["<A-j>"] = actions.cycle_previewers_next,
              ["<A-k>"] = actions.cycle_previewers_prev,
            },
          },
        },
        pickers = {
          find_files = {
            find_command = { "fd", "--follow", "--type", "f", "--strip-cwd-prefix" },
          },
          live_grep = {
            additional_args = { "--follow" },
          },
        },
      }
    end,
  },
  {
    "nvim-telescope/telescope-bibtex.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    ft = { "tex" },
    config = function() require("telescope").load_extension "bibtex" end,
  },
  { "https://github.com/andymass/vim-matchup", lazy = false },
  {
    "https://github.com/echasnovski/mini.nvim",
    lazy = false,
    config = function()
      require("mini.surround").setup {
        mappings = {
          add = "ys",
          delete = "ds",
          find = "",
          find_left = "",
          highlight = "",
          replace = "cs",
          update_n_lines = "",
          -- Add this only if you don't want to use extended mappings
          suffix_last = "",
          suffix_next = "",
        },
        search_method = "cover_or_next",
      }
      require("mini.align").setup()
      require("mini.bufremove").setup()
      -- Remap adding surrounding to Visual mode selection
      vim.api.nvim_del_keymap("x", "ys")
      vim.api.nvim_set_keymap("x", "S", [[:<C-u>lua MiniSurround.add('visual')<CR>]], { noremap = true })
      -- Make special mapping for "add surrounding for line",
      vim.api.nvim_set_keymap("n", "yss", "ys_", { noremap = false })
    end,
  },
  {
    "https://github.com/sQVe/sort.nvim",
    lazy = false,
    config = function()
      vim.api.nvim_exec2(
        [[
 		nnoremap <silent> gs <Cmd>Sort<CR>
 		vnoremap <silent> gs <Esc><Cmd>Sort<CR>

 		nnoremap <silent> gsi" vi"<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi' vi'<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi( vi(<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi) vi)<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi] vi]<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi[ vi[<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsip vip<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi{ vi{<Esc><Cmd>Sort<CR>
 		nnoremap <silent> gsi} vi}<Esc><Cmd>Sort<CR>
 					]],
        {}
      )
    end,
  },
  {
    "https://github.com/assistcontrol/readline.nvim",
    lazy = false,
    config = function()
      local readline = require "readline"

      -- Navigation
      vim.keymap.set("!", "<C-a>", readline.dwim_beginning_of_line) -- Goes to start of line or non-blank
      vim.keymap.set("!", "<C-x><C-a>", "<C-a>") -- Goes to start of line or non-blank
      vim.keymap.set("!", "<M-a>", "<C-o>^")
      vim.keymap.set("c", "<C-a>", readline.beginning_of_line) -- C-o doesn't work in command mode
      vim.keymap.set("!", "<C-e>", readline.end_of_line)

      vim.keymap.set("!", "<C-b>", "<Left>")
      vim.keymap.set("!", "<C-f>", "<Right>")
      vim.keymap.set("!", "<M-f>", "<S-Right>") -- This is preferred, since it doesn't consider symbols as words
      vim.keymap.set("c", "<M-f>", readline.forward_word) -- S-Right/Left works for WORD instead of word in command mode
      vim.keymap.set("!", "<M-b>", "<S-Left>")
      vim.keymap.set("c", "<M-b>", readline.backward_word)

      vim.keymap.set("!", "<M-S-f>", "<C-o>E<Right>")
      vim.keymap.set("c", "<M-S-f>", "<S-Right>")
      vim.keymap.set("!", "<M-S-b>", "<C-o>B")
      vim.keymap.set("c", "<M-S-b>", "<S-Left>")

      -- # Editing
      -- vim.keymap.set("!", "<C-u>", readline.backward_kill_line) (Already exists)
      vim.keymap.set("!", "<C-k>", readline.kill_line)

      vim.keymap.set("!", "<C-h>", "<BS>") -- backward-delete-char
      vim.keymap.set("!", "<C-d>", "<Delete>") -- delete-char
      -- bind 'C-w'   backward-kill-word (Already exists)
      vim.keymap.set("!", "<M-d>", "<C-o>dw")
      vim.keymap.set("c", "<M-d>", readline.kill_word)
      vim.keymap.set("!", "<M-w>", readline.unix_word_rubout)
      vim.keymap.set("!", "<M-S-d>", "<C-o>dE")
      vim.keymap.set("c", "<M-S-d>", readline.kill_word)
    end,
  },
  { "https://github.com/tpope/vim-unimpaired", lazy = false },
  { "https://github.com/tpope/vim-repeat", lazy = false }, -- Used to repeat vim-unimpaired actions
  {
    "https://github.com/folke/todo-comments.nvim",
    lazy = false,
    dependencies = "nvim-lua/plenary.nvim",

    config = function()
      require("todo-comments").setup {
        signs = false,
        keywords = {
          REFACTOR = { icon = "" },
        },
      }
    end,
  },
  { "https://github.com/lervag/vimtex", ft = "tex" },
  { "https://github.com/jbyuki/nabla.nvim", ft = "tex" },
  {
    "https://github.com/EdenEast/nightfox.nvim",
    groups = {
      all = {
        -- Some examples, make sure only to define Conceal once
        Conceal = {
          link = "Comment",
          fg = "syntax.comment",
        }, -- link `Conceal` to a spec value
      },
    },
  },
  -- { "https://github.com/Ron89/thesaurus_query.vim" },
  --  File type support
  { "https://github.com/fladson/vim-kitty", ft = "kitty" },
  { "https://github.com/YasserKa/vim-sxhkdrc", ft = "sxhkdrc" },
  { "https://github.com/Fymyte/mbsync.vim", ft = "mbsync" },
  { "https://github.com/sheerun/vim-polyglot", lazy = false }, -- provides better indentation & syntax highlight

  -- Git
  { "https://github.com/tpope/vim-fugitive", event = "VeryLazy" },
  {
    "https://github.com/junegunn/gv.vim",
    dependencies = "https://github.com/tpope/vim-fugitive",
    event = "VeryLazy",
  },
  {
    "https://github.com/tpope/vim-rhubarb",
    dependencies = "https://github.com/tpope/vim-fugitive",
    event = "VeryLazy",
  },

  { "https://github.com/terryma/vim-expand-region", lazy = false },
  { "https://github.com/jeetsukumaran/vim-commentary", event = "VeryLazy" },
  { "https://github.com/szw/vim-maximizer", lazy = false },
  { "https://github.com/simnalamburt/vim-mundo", cmd = "MundoToggle" },
  {
    "https://github.com/iamcco/markdown-preview.nvim",
    cmd = "MarkdownPreviewToggle",
    build = function() vim.fn["mkdp#util#install"]() end,
    config = function() vim.g.mkdp_filetypes = { "markdown", "plantuml" } end,
    ft = { "markdown", "plantuml" },
  },
  {
    "https://github.com/kdheepak/cmp-latex-symbols",
    dependencies = "hrsh7th/nvim-cmp",
    ft = { "tex" },
  },
  {
    "ray-x/lsp_signature.nvim",
    event = "VeryLazy",
    opts = {},
    config = function(_, opts) require("lsp_signature").setup(opts) end,
  },
  { "https://github.com/romainl/vim-cool", lazy = false }, -- Disable search highlighting when done
  { "https://github.com/honza/vim-snippets", lazy = false },
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function(_, config) -- overrides `require("null-ls").setup(config)`
      -- config variable is the default configuration table for the setup function call
      local null_ls = require "null-ls"

      -- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
      config.sources = {
        -- Bash
        null_ls.builtins.code_actions.shellcheck,
        null_ls.builtins.hover.printenv,
        null_ls.builtins.diagnostics.dotenv_linter,
        null_ls.builtins.formatting.shfmt.with {
          extra_args = { "--case-indent" },
        },
        -- Python
        -- null_ls.builtins.diagnostics.ruff,
        null_ls.builtins.formatting.isort,
        null_ls.builtins.formatting.black.with {
          extra_args = { "--experimental-string-processing" },
        },
        -- TOML
        null_ls.builtins.formatting.dprint,
        null_ls.builtins.formatting.taplo,

        -- null_ls.builtins.formatting.stylua,
        null_ls.builtins.formatting.prettier,
      }
      return config
    end,
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    opts = function()
      return {
        window = {
          mappings = {
            ["<tab>"] = {
              "toggle_node",
              nowait = false,
            },
            ["l"] = {
              "toggle_node",
              nowait = false,
            },
            ["h"] = {
              "close_node",
              nowait = false,
            },
            ["v"] = "open_vsplit",
            ["x"] = "open_split",
          },
        },
        filesystem = {
          window = {
            mappings = {
              ["<Leader>H"] = "toggle_hidden",
              ["h"] = {
                "close_node",
                nowait = false,
              },
            },
          },
        },
      }
    end,
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    build = ":TSUpdate",
    config = function()
      require("ts_context_commentstring").setup {
        enable_autocmd = false,
        context_commentstring = {
          enable = true,
          commentary_integration = {
            -- change default mapping
            Commentary = "g/",
            -- disable default mapping
            CommentaryLine = false,
          },
        },
      }
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function()
      return {
        ensure_installed = {
          "bash",
          "dockerfile",
          "html",
          "javascript",
          "json",
          "latex",
          "lua",
          "python",
          "toml",
          "vim",
        },

        indent = { enable = true, disable = { "python" } },
        matchup = { enable = true, enable_quotes = true },
        auto_install = true,
        highlight = {
          enable = true,
          -- vimtex conceal doesn't work with treesitter check :h vimtex-faq-treesitter
          disable = { "latex", "help", "man" },
        },
        tree_surfer = { enable = true },
        textobjects = {
          select = {
            enable = true,
            disable = { "man" },

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["a/"] = "@comment.outer",
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
              ["@parameter.outer"] = "v", -- charwise
              ["@function.outer"] = "V", -- linewise
              ["@class.outer"] = "<c-v>", -- blockwise
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
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]k"] = "@function.outer",
              ["]]"] = { query = "@class.outer", desc = "Next class start" },
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[k"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
        },
      }
    end,
  },
  {
    "https://github.com/danymat/neogen",
    dependencies = "nvim-treesitter/nvim-treesitter",
    cmd = "Neogen",
    config = function()
      require("neogen").setup {
        snippet_engine = "luasnip",
      }
    end,
  },
  {
    "https://github.com/ziontee113/syntax-tree-surfer",
    dependencies = "nvim-treesitter",
    lazy = false,
  },
  -- use mason-lspconfig to configure LSP installations
  {
    "williamboman/mason-lspconfig.nvim", -- overrides `require("mason-lspconfig").setup(...)`
    opts = function(_, opts)
      -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
      -- taplo: toml
      opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed, {
        "lua_ls",
        "pyright",
        "bashls",
        "taplo",
      })
    end,
  },
  -- use mason-null-ls to configure Formatters/Linter installation for null-ls sources
  {
    "jay-babu/mason-null-ls.nvim", -- overrides `require("mason-null-ls").setup(...)`
    opts = function(_, opts)
      opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed, {
        "prettier",
        "stylua",
        "isort",
        "ruff",
        "black",
        "shellcheck",
        "shfmt",
        "dprint",
        "taplo",
      })
    end,
  },
  {
    "jay-babu/mason-nvim-dap.nvim",
    opts = function(_, opts)
      -- add more things to the ensure_installed table protecting against community packs modifying it
      opts.ensure_installed = require("astrocore").list_insert_unique(opts.ensure_installed, {})
    end,
  },
  -- Misc
  -- Offline Documentation
  {
    "KabbAmine/zeavim.vim",
    lazy = false,
    -- ft = "python",
    config = function()
      vim.api.nvim_exec2(
        [[
 nmap <Leader>z <Plug>Zeavim
 vmap <Leader>z <Plug>ZVVisSelection
 nmap gz <Plug>ZVOperator
 nmap <Leader><Leader>z <Plug>ZVKeyDocset
 ]],
        {}
      )
    end,
  },
  -- Jupyter notebook
  {
    "untitled-ai/jupyter_ascending.vim",
    event = "BufEnter *sync.py",
    config = function()
      local wk = require "which-key"

      -- Execute cells using the command line by passing the line numbers
      Execute_cells = function(line_nums)
        local file_path = vim.fn.expand "%:p"
        local shell_command = "silent !{ "
        -- { python -m jupyter_ascending.requests.execute --filename a.sync.py --line 1 && python -m jupyter_ascending.requests.execute --filename a.sync.py --line 6; } >/dev/null & disown
        for _, line_num in ipairs(line_nums) do
          shell_command = shell_command
            .. " python -m jupyter_ascending.requests.execute --filename "
            .. file_path
            .. " --linenumber "
            .. line_num
            .. ";"
        end
        shell_command = shell_command .. " } >/dev/null & disown"
        vim.cmd(shell_command)
      end

      -- Execute visually selected cells
      Execute_selected_cells = function()
        -- Leave visual mode to update the "<" and ">" marks
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "v", true)
        local start_line = vim.api.nvim_buf_get_mark(0, "<")[1]
        local end_line = vim.api.nvim_buf_get_mark(0, ">")[1]
        local lines = {}
        -- Add first cell if the # %% isn't selected
        local first_line = vim.api.nvim_buf_get_lines(0, start_line - 1, start_line, false)[1]
        if not first_line:find "^# %%" then table.insert(lines, start_line) end

        for line_num = start_line, end_line do
          local line = vim.api.nvim_buf_get_lines(0, line_num - 1, line_num, false)[1]
          if line:find "^# %%" then table.insert(lines, line_num) end
        end
        Execute_cells(lines)
      end

      wk.register {
        ["<localLeader>"] = {
          n = {
            name = "jupyter ascending",
            c = { "<Plug>JupyterExecute", "Execute cell" },
            C = {
              "<Plug>JupyterExecute <cmd>call search('# %%$')<cr>",
              "Execute cell and jump to next cell",
            },
            r = { "<Plug>JupyterExecuteAll", "Run file" },
            R = { "<Plug>JupyterRestart", "Restart Jupyter" },
            J = {
              '<cmd>silent !$BROWSER --target window "http://localhost:8888/notebooks/"'
                .. vim.fn.expand "%:r"
                .. ".ipynb &>/dev/null & disown<cr>",
              "Run Jupyter server",
            },
          },
        },
      }

      wk.register({
        ["<localLeader>"] = {
          n = {
            name = "Jupyter Ascending",
            c = { ":lua Execute_selected_cells()<CR>", "Execute selected cells" },
          },
        },
      }, { mode = "v" })
    end,
  },
  -- For inserting and navigating cells
  {
    "https://github.com/hanschen/vim-ipython-cell",
    event = "BufRead *.sync.py",
    dependencies = "untitled-ai/jupyter_ascending.vim",
    config = function()
      local wk = require "which-key"
      wk.register {
        ["<localLeader>"] = {
          n = {
            m = { "<cmd>IPythonCellToMarkdown<cr>", "To markdown" },
            I = { ":IPythonCellInsertAbove<CR>o", "Insert cell above" },
            i = { ":IPythonCellInsertBelow<CR>o", "Insert cell below" },
            j = { "<cmd>call search('# %%$')<cr>", "Go to next cell" },
            k = { "<cmd>call search('# %%$', 'b')<cr>", "Go to previous cell" },
          },
        },
        ["[c"] = { ":IPythonCellPrevCell<CR>", "Previous Cell" },
        ["]c"] = { ":IPythonCellNextCell<CR>", "Next Cell" },
      }
      wk.register({
        ["<C-,>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above" },
        ["<F2>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above" },

        ["<C-,>ni"] = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below" },
        ["<F2>ni"] = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below" },
      }, { mode = "i" })
    end,
  },
}
