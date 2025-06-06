-- AstroCore provides a central place to modify mappings, vim options, autocommands, and more!
-- Configuration documentation can be found with `:h astrocore`
-- NOTE: We highly recommend setting up the Lua Language Server (`:LspInstall lua_ls`)
--       as this provides autocomplete and documentation while editing

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- Configure core features of AstroNvim
    features = {
      large_buf = { size = 1024 * 256, lines = 10000 }, -- set global limits for large files for disabling features like treesitter
      autopairs = true, -- enable autopairs at start
      cmp = true, -- enable completion at start
      diagnostics = { virtual_text = true, virtual_lines = false }, -- diagnostic settings on startup
      highlighturl = true, -- highlight URLs at start
      notifications = true, -- enable notifications at start
    },
    -- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
    diagnostics = {
      virtual_text = true,
      underline = true,
    },
    -- vim options can be configured here
    options = {
      opt = { -- vim.opt.<key>
        relativenumber = true, -- sets vim.opt.relativenumber
        number = true, -- sets vim.opt.number
        spell = false, -- sets vim.opt.spell
        signcolumn = "yes", -- sets vim.opt.signcolumn to yes
        wrap = false, -- sets vim.opt.wrap
        foldmethod = "marker", -- Folds at start
        foldmarker = "{{{,}}}", -- Folds format
        scrolloff = 5, -- Show 5 lines above/below the cursor
        autowrite = true, -- Show 5 lines above/below the cursor
      },
      g = { -- vim.g.<key>
        -- configure global vim variables (vim.g)
        -- NOTE: `mapleader` and `maplocalleader` must be set in the AstroNvim opts or before `lazy.setup`
        -- This can be found in the `lua/lazy_setup.lua` file
      },
    },
    -- Mappings can be configured through AstroCore as well.
    -- NOTE: keycodes follow the casing in the vimdocs. For example, `<Leader>` must be capitalized
    mappings = {
      -- first key is the mode
      n = {
        -- second key is the lefthand side of the map

        -- navigate buffer tabs
        ["]b"] = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        ["[b"] = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- mappings seen under group name "Buffer"
        ["<Leader>bd"] = {
          function()
            require("astroui.status.heirline").buffer_picker(
              function(bufnr) require("astrocore.buffer").close(bufnr) end
            )
          end,
          desc = "Close buffer from tabline",
        },

        -- tables with just a `desc` key will be registered with which-key if it's installed
        -- this is useful for naming menus
        -- ["<Leader>b"] = { desc = "Buffers" },

        -- setting a mapping to false will disable it
        -- ["<C-S>"] = false,
        --
        ["<Leader>e"] = false,
        ["<Leader>ex"] = { "<cmd>Neotree toggle<cr>", desc = "Toggle Explorer" },
        ["gx"] = { "<cmd>lua system_open()<cr>", desc = "Open the file under cursor with system app" },
        ["<space><space>"] = { "<cmd>buffer#<cr>", desc = "Alternate buffer" },
        ["<localLeader>l"] = false,
        ["<localLeader>m"] = {
          function() require("nabla").popup() end,
          desc = "Preview Math",
        },
        ["<localLeader>M"] = {
          function() require("nabla").toggle_virt() end,
          desc = "Preview Math",
        },
        ["<Leader>."] = { "<cmd>cd %:p:h<cr>", desc = "Set CWD" },
        ["<Leader>bO"] = { "<cmd>silent :%bdelete | edit# | bdelete#<cr>", desc = "Remove all other buffers" },
        ["<Leader>gS"] = {
          function() require("gitsigns").stage_buffer() end,
          desc = "Stage Buffer",
        },
        ["<Leader>gU"] = {
          function() require("gitsigns").reset_buffer_index() end,
          desc = "Unstage Buffer",
        },
        ["<Leader>gc"] = { "<cmd>silent Git commit --quiet<CR>", noremap = true, desc = "Commit" },
        ["<Leader><M-f>gc"] = { "<cmd>silent Git commit --force --quiet<CR>", noremap = true, desc = "Commit force" },
        ["<Leader>a"] = { name = "Annotate" },
        ["<Leader>a<cr>"] = {
          function() require("neogen").generate() end,
          desc = "Current",
        },
        ["<Leader>ac"] = {
          function() require("neogen").generate { type = "class" } end,
          desc = "Class",
        },
        ["<Leader>af"] = {
          function() require("neogen").generate { type = "func" } end,
          desc = "Function",
        },
        ["<Leader>at"] = {
          function() require("neogen").generate { type = "type" } end,
          desc = "Type",
        },
        ["<Leader>aF"] = {
          function() require("neogen").generate { type = "file" } end,
          desc = "File",
        },
        ["<Leader>bb"] = { "<cmd>tabnew<cr>", desc = "New tab" },
        ["<Leader>bc"] = { "<cmd>BufferLinePickClose<cr>", desc = "Pick to close" },
        ["<Leader>bj"] = { "<cmd>BufferLinePick<cr>", desc = "Pick to jump" },
        ["<Leader>bt"] = { "<cmd>BufferLineSortByTabs<cr>", desc = "Sort by tabs" },

        ["<Leader>q"] = { "<cmd>exit<cr>", desc = "Delete buffer" },
        ["<Leader>Q"] = { "<cmd>quitall<cr>", desc = "Exit Vim" },

        -- Navigation
        L = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        H = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- Text
        ["j"] = { "gj" },
        ["k"] = { "gk" },
        ["gj"] = { "j" },
        ["gk"] = { "k" },
        ["g:"] = { "g;" },
        ["q;"] = { "q:" },
        ["@;"] = { "@;" },
        ["<Leader>es"] = { "<cmd>split ~/.config/nvim/lua/plugins/user.lua<cr>", desc = "Split config file" },
        ["<Leader>ss"] = { "<cmd>source ~/.config/nvim/lua/plugins/user.lua<cr>", desc = "Source config file" },

        ["<C-s>"] = { "<cmd>w!<cr>", desc = "Save File" },

        ["yod"] = {
          function() require("astrocore.toggles").diagnostics() end,
          desc = "Toggle diagnostics",
        },
        ["yog"] = {
          function() require("astrocore.toggles").signcolumn() end,
          desc = "Toggle signcolumn",
        },
        ["yoi"] = {
          function() require("astrocore.toggles").indent() end,
          desc = "Change indent setting",
        },
        ["yol"] = {
          function() require("astrocore.toggles").statusline() end,
          desc = "Toggle statusline",
        },
        ["yon"] = {
          function() require("astrocore.toggles").number() end,
          desc = "Change line numbering",
        },
        ["yos"] = {
          function() require("astrocore.toggles").spell() end,
          desc = "Toggle spellcheck",
        },
        ["yop"] = {
          function() require("astrocore.toggles").paste() end,
          desc = "Toggle paste mode",
        },
        ["yot"] = {
          function() require("astrocore.toggles").tabline() end,
          desc = "Toggle tabline",
        },
        ["you"] = {
          function() require("astrocore.toggles").url_match() end,
          desc = "Toggle URL highlight",
        },
        ["yow"] = {
          function() require("astrocore.toggles").wrap() end,
          desc = "Toggle wrap",
        },
        ["yoy"] = {
          function() require("astrocore.toggles").buffer_syntax() end,
          desc = "Toggle syntax highlight",
        },
      },
      v = {
        [";"] = { ":" },
        [":"] = { ";" },
      },
      i = {
        ["<C-l>"] = { "<c-g>u<Esc>[s1z=`]a<c-g>u", desc = "Fix last misspell" },
        ["<C-S-v>"] = { "<c-r>+", desc = "Fix last misspell" },
        ["<C-s>"] = { "<C-o>:w!<cr>", desc = "Save File" },
      },
      x = {
        ["il"] = { "g_o^", desc = "Inside line text object" },
        ["al"] = { "$o^", desc = "Around line text object" },
      },
      o = {
        -- line text-objects
        ["il"] = { ":normal vil<cr>", desc = "Inside line text object" },
        ["al"] = { ":normal val<cr>", desc = "Around line text object" },
      },
    },
  },
}
