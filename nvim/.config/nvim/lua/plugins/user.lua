-- You can also add or configure plugins by creating files in this `plugins/` folder
-- Here are some examples:

---@type LazySpec
return {

  -- == Examples of Adding Plugins ==

  -- You can disable default plugins as follows:
  { "max397574/better-escape.nvim", enabled = false },
  { "stevearc/aerial.nvim", enabled = false },

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
  {
    "Saghen/blink.cmp",
    dependencies = {
      -- add the legacy cmp source as a dependency for `blink.cmp`
      "kdheepak/cmp-latex-symbols",
    },
    specs = {
      -- install the blink, nvim-cmp compatibility layer
      { "Saghen/blink.compat", version = "*", lazy = true, opts = {} },
    },
    -- require('blink.cmp')[command]
    opts = {
      sources = {
        default = { "latex" },
        providers = {
          snippets = { score_offset = 8 },
          lsp = { score_offset = 7 },
          latex = {
            name = "latex_symbols",
            module = "blink.compat.source",
            score_offset = -1,
          },
          buffer = { score_offset = 5 },
          path = { score_offset = 8 },
        },
      },
      keymap = {
        ["<A-k>"] = { function(cmp) cmp.scroll_documentation_up(4) end, "fallback" },
        ["<A-j>"] = { function(cmp) cmp.scroll_documentation_down(4) end, "fallback" },
        ["<C-k>"] = { "snippet_backward", "fallback" },
        ["<C-j>"] = { "snippet_forward", "fallback" },
      },
    },
  },

  {
    "folke/snacks.nvim",
    opts = {
      dashboard = {
        enabled = false,
      },
      picker = {
        layout = "my_layout",
        layouts = {
          my_layout = {
            cycle = true,
            layout = {
              box = "horizontal",
              width = 0.8,
              height = 0.8,
              min_width = 120,
              border = "none",
              {
                box = "vertical",
                {
                  win = "input",
                  height = 1,
                  border = "none",
                  title = "{title} {live} {flags}",
                  title_pos = "center",
                },
                { win = "list", title = " Results ", title_pos = "center", border = "none" },
              },
              {
                win = "preview",
                title = "{preview:Preview}",
                border = "rounded",
                title_pos = "center",
              },
            },
          },
        },
        win = {
          input = {
            keys = {
              ["<Esc>"] = { "close", mode = { "n", "i" } },
              ["<C-[>"] = { "close", mode = { "n", "i" } },
              ["<C-u>"] = false,
              ["<C-x>"] = { "edit_split", mode = { "n", "i" } },
              -- ["<C-d>"] = actions.delete_buffer,
              -- ["<C-s>"] = actions.select_horizontal,
              -- ["<C-n>"] = actions.move_selection_next,
              -- ["<C-p>"] = actions.move_selection_previous,
              ["<A-j>"] = { "preview_scroll_down", mode = { "i", "n" } },
              ["<A-k>"] = { "preview_scroll_up", mode = { "i", "n" } },
            },
          },
        },
        cwd_bonus = true, -- give bonus for matching files in the cwd
        frecency = true, -- frecency bonus
      },
    },
    keys = {
      {
        "<Leader>ff",
        function()
          require("snacks").picker.files {
            cmd = "fd",
            format = "file",
            show_empty = false,
            hidden = false,
            ignored = false,
            follow = true,
            supports_live = true,
          }
        end,
        desc = "Files",
      },
      {
        "<Leader>fF",
        function()
          require("snacks").picker.files {
            cmd = "fd",
            format = "file",
            show_empty = false,
            hidden = true,
            ignored = false,
            follow = true,
            supports_live = true,
          }
        end,
        desc = "Files",
      },
      {
        "<Leader>fw",
        function() require("snacks").picker.grep {} end,
        desc = "Words",
      },
      {
        "<Leader>fW",
        function()
          require("snacks").picker.grep {
            cmd = "rg",
            hidden = true,
            ignored = false,
          }
        end,
        desc = "Words",
      },
      {
        "<Leader>f?",
        function() require("snacks").picker.help {} end,
        desc = "Help tags",
      },
      {
        "<Leader>f'",
        function() require("snacks").picker.marks {} end,
        desc = "Marks",
      },
    },
  },

  { -- Used only for bibtex
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
      }
    end,
  },
  {
    "nvim-telescope/telescope-bibtex.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    ft = { "tex" },
    config = function()
      require("telescope").load_extension "bibtex"

      local wk = require "which-key"
      wk.add {
        { "<Leader>fB", "<cmd>Telescope bibtex<cr>", desc = "BibTeX" },
      }
    end,
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
  {
    "https://github.com/ColinKennedy/cursor-text-objects.nvim",
    lazy = false,
    config = function()
      local down_description = "Operate from your current cursor to the end of some text-object."
      local up_description = "Operate from the start of some text-object to your current cursor."

      vim.keymap.set("o", "[", "<Plug>(cursor-text-objects-up)", { desc = up_description })
      vim.keymap.set("o", "]", "<Plug>(cursor-text-objects-down)", { desc = down_description })
      vim.keymap.set("x", "[", "<Plug>(cursor-text-objects-up)", { desc = up_description })
      vim.keymap.set("x", "]", "<Plug>(cursor-text-objects-down)", { desc = down_description })
    end,
  },
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
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = function(plugin)
      if vim.fn.executable "npx" then
        vim.cmd("!cd " .. plugin.dir .. " && cd app && npx --yes yarn install")
      else
        vim.cmd [[Lazy load markdown-preview.nvim]]
        vim.fn["mkdp#util#install"]()
      end
    end,
    init = function()
      if vim.fn.executable "npx" then vim.g.mkdp_filetypes = { "markdown" } end
    end,
    config = function() vim.g.mkdp_filetypes = { "markdown", "plantuml" } end,
    ft = { "markdown", "plantuml" },
  },
  {
    "https://github.com/vim-pandoc/vim-markdownfootnotes",
    event = "BufEnter qutebrowser-editor-*,tuir*,neomutt-*",
    ft = { "markdown" },
    config = function()
      vim.api.nvim_exec2(
        [[
        source $XDG_DATA_HOME/nvim/lazy/vim-markdownfootnotes/autoload/markdownfootnotes.vim 
        source $XDG_DATA_HOME/nvim/lazy/vim-markdownfootnotes/ftplugin/markdown/markdownfootnotes.vim 
        " Remove plugin bindings
        iunmap <buffer> <Leader>f
        iunmap <buffer> <Leader>r
        " Remove timeout when pressing <C-,>
        set notimeout

        inoremap <expr> <C-,> CommaPrefix()

        " Function to handle the comma prefix
        function! CommaPrefix()
        " Wait for the next key
        return ''
        endfunction
        " Add specific follow-up mappings
        ]],
        {}
      )
      local wk = require "which-key"
      wk.add {
        {
          { "<localleader>i", "<Plug>AddVimFootnote", desc = "Insert footnote" },
          { "<localleader>r", "<Plug>ReturnFromFootnote", desc = "Return from footnote" },
          { "<localleader>c", "<Plug>FootnoteNumber", desc = "Return from footnote" },
        },
        {
          { "<C-,>f", "<C-o><Plug>AddVimFootnote", desc = "Insert footnote" },
          { "<C-,>r", "<C-o><Plug>ReturnFromFootnote", desc = "Return from footnote" },
          { "<C-c><C-c>", "<C-o><Plug>ReturnFromFootnote", desc = "Return from footnote" },
          { "<C-,>c", "<C-o><Plug>FootnoteNumber", desc = "Return from footnote" },
          mode = "i",
        },
      }
    end,
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
    "nvimtools/none-ls.nvim",
    opts = function(_, opts) -- overrides `require("null-ls").setup(config)`
      -- config variable is the default configuration table for the setup function call
      local null_ls = require "null-ls"

      -- https://github.com/nvimtools/none-ls.nvim/blob/main/doc/BUILTINS.md
      opts.sources = require("astrocore").list_insert_unique(opts.sources, {
        -- Bash
        -- null_ls.builtins.code_actions.shellcheck,
        null_ls.builtins.hover.printenv,
        null_ls.builtins.diagnostics.dotenv_linter,
        null_ls.builtins.formatting.shfmt.with {
          extra_args = { "--case-indent" },
        },
        -- Python
        -- null_ls.builtins.diagnostics.ruff,
        -- Lags python
        -- null_ls.builtins.formatting.isort,
        null_ls.builtins.formatting.black.with {
          extra_args = { "--experimental-string-processing" },
        },
        -- TOML
        -- null_ls.builtins.formatting.dprint,
        -- null_ls.builtins.formatting.taplo,

        -- null_ls.builtins.formatting.stylua,
        null_ls.builtins.formatting.prettier,
      })
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
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    -- overrides `require("mason-tool-installer").setup(...)`
    opts = {
      -- Make sure to use the names found in `:Mason`
      ensure_installed = {
        -- Language servers
        -- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
        -- taplo: toml
        "lua-language-server",
        -- "pyright",
        "bash-language-server",
        "taplo",

        -- Formatters
        "prettier",
        "stylua",
        -- Lags python
        -- "isort",
        "ruff",
        "black",
        "shellcheck",
        "shfmt",
        "dprint",
        "taplo",
        --         -- install debuggers
        --         "debugpy",

        --         -- install any other package
        --         "tree-sitter-cli",
      },
    },
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
  -- For inserting and navigating cells
  {
    "https://github.com/hanschen/vim-ipython-cell",
    event = "BufReadPost *.py",
    dependencies = "https://github.com/jpalardy/vim-slime",
    config = function()
      -- Vim-slime setting
      -- always use tmux
      vim.g.slime_target = "tmux"

      -- https://github.com/jpalardy/vim-slime/tree/main/ftplugin/python
      vim.g.slime_bracketed_ipython = 1

      -- always send text to the top-right pane in the current tmux tab without asking
      vim.g.slime_default_config = {
        socket_name = vim.split(vim.env.TMUX or "", ",")[1],
        target_pane = "{top-right}",
      }

      vim.g.slime_dont_ask_default = 1

      -- Override the comment that makes a cell take "##"
      -- this will cause a problem if there's a string having "##"
      vim.g.ipython_cell_tag = { "# %%" }

      local function setup_python_buffer()
        local bufnr = vim.api.nvim_get_current_buf()
        local first_line_file = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1]
        if first_line_file:match "^#%s*%%%%" then
          local wk = require "which-key"
          wk.add {
            { "<localLeader>nI", ":IPythonCellInsertAbove<CR>o", desc = "Insert cell above" },
            { "<localLeader>ni", ":IPythonCellInsertBelow<CR>o", desc = "Insert cell below" },
            { "<localLeader>nj", "<cmd>call search('# %%$')<cr>", desc = "Go to next cell" },
            { "<localLeader>nk", "<cmd>call search('# %%$', 'b')<cr>", desc = "Go to previous cell" },
            { "<localLeader>nm", "<cmd>IPythonCellToMarkdown<cr>", desc = "To markdown" },
            { "[c", "<cmd>call search('# %%$', 'b')<cr>", desc = "Previous Cell" },
            { "]c", "<cmd>call search('# %%$')<cr>", desc = "Next Cell" },
          }
          wk.add {
            {
              mode = { "i" },
              { "<C-,>nI", "<C-o>:IPythonCellInsertAbove<CR><CR>", desc = "Insert cell above" },
              { "<C-,>ni", "<C-o>:IPythonCellInsertBelow<CR><CR>", desc = "Insert cell below" },
              { "<F2>nI", "<C-o>:IPythonCellInsertAbove<CR><CR>", desc = "Insert cell above" },
              { "<F2>ni", "<C-o>:IPythonCellInsertBelow<CR><CR>", desc = "Insert cell below" },
            },
          }
        end
      end

      vim.api.nvim_create_autocmd({ "BufRead" }, {
        pattern = { "*.py" },
        callback = setup_python_buffer,
      })
      -- Trigger immediately if current buffer is already a .py file
      if vim.fn.expand("%"):match "%.py$" then setup_python_buffer() end
    end,
  },
}
