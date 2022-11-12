local config = {

	-- Configure AstroNvim updates
	updater = {
		remote = "origin", -- remote to use
		channel = "nightly", -- "stable" or "nightly"
		version = "latest", -- "latest", tag name, or regex search like "v1.*" to only do updates before v2 (STABLE ONLY)
		branch = "main", -- branch name (NIGHTLY ONLY)
		spellfile = "~/.config/nvim/spell/en.utf-8.add",
		commit = nil, -- commit hash (NIGHTLY ONLY) lua
		pin_plugins = nil, -- nil, true, false (nil will pin plugins on stable only)
		skip_prompts = false, -- skip prompts about breaking changes
		show_changelog = true, -- show the changelog after performing an update
		auto_reload = false, -- automatically reload and sync packer after a successful update
		auto_quit = false, -- automatically quit the current session after a successful update
		-- remotes = { -- easily add new remotes to track
		--   ["remote_name"] = "https://remote_url.come/repo.git", -- full remote url
		--   ["remote2"] = "github_user/repo", -- GitHub user/repo shortcut,
		--   ["remote3"] = "github_user", -- GitHub user assume AstroNvim fork
		-- },
	},

	-- Set colorscheme to use
	colorscheme = "nordfox",

	-- Add highlight groups in any theme
	highlights = {
		-- init = { -- this table overrides highlights in all themes
		--   Normal = { bg = "#000000" },
		-- }
		-- duskfox = { -- a table of overrides/changes to the duskfox theme
		--   Normal = { bg = "#000000" },
		-- },
	},

	-- set vim options here (vim.<first_key>.<second_key> =  value)
	options = {
		opt = {
			-- set to true or false etc.
			signcolumn = "auto", -- sets vim.opt.signcolumn to auto
			wrap = true, -- sets vim.opt.wrap
			textwidth = 80, -- sets vim.opt.wrap
			virtualedit = "all", -- Moving in whitespace
			ignorecase = true, -- Do case insensitive search...
			smartcase = true, -- ...unless capital letters are used
			confirm = true, -- Confirm :q in case of unsaved changes
			autowrite = true, -- Save file when switching buffers
			termguicolors = true,
			scrolloff = 5, -- Show 5 lines above/below the cursor
			spell = false, -- sets vim.opt.spell
			cursorline = true, -- Highlight current line"
			number = true, -- Show the number line
			relativenumber = true,
			listchars = "eol:¶,tab:>-,trail:.,nbsp:_,extends:+,precedes:+",
			foldexpr = "nvim_treesitter#foldexpr()", -- set Treesitter based folding
			conceallevel = 1,
			foldmethod = "expr", -- Fold method
			foldenable = false,
			foldmarker = "{{{,}}}", -- Folds format
			laststatus = 3, -- Use Global statusline
			expandtab = true, -- Use spaces instead of tabs
		},
		g = {
			mapleader = " ", -- sets vim.g.mapleader
			maplocalleader = ",", -- sets vim.g.mapleader
			autoformat_enabled = true, -- enable or disable auto formatting at start (lsp.formatting.format_on_save must be enabled)
			cmp_enabled = true, -- enable completion at start
			autopairs_enabled = true, -- enable autopairs at start
			diagnostics_enabled = true, -- enable diagnostics at start
			status_diagnostics_enabled = true, -- enable diagnostics in statusline
			icons_enabled = true, -- disable icons in the UI (disable if no nerd font is available, requires :PackerSync after changing)
		},
	},

	-- Default theme configuration
	default_theme = {
		-- enable or disable highlighting for extra plugins
		plugins = {
			aerial = false,
			beacon = false,
			bufferline = true,
			cmp = true,
			dashboard = false,
			highlighturl = true,
			hop = false,
			indent_blankline = true,
			lightspeed = false,
			["neo-tree"] = true,
			notify = true,
			["nvim-tree"] = false,
			["nvim-web-devicons"] = true,
			rainbow = true,
			symbols_outline = false,
			telescope = true,
			treesitter = true,
			vimwiki = false,
			["which-key"] = true,
		},
	},

	-- Diagnostics configuration (for vim.diagnostics.config({...})) when diagnostics are on
	diagnostics = {
		virtual_text = true,
		underline = true,
	},

	-- Extend LSP configuration
	lsp = {
		-- enable servers that you already have installed without mason
		servers = {},
		formatting = {
			-- control auto formatting on save
			format_on_save = {
				enabled = true, -- enable or disable format on save globally
				allow_filetypes = { -- enable format on save for specified filetypes only
				},
				ignore_filetypes = { -- disable format on save for specified filetypes
				},
			},
			disabled = { -- disable formatting capabilities for the listed language servers
			},
			timeout_ms = 1000, -- default format timeout
			-- filter = function(client) -- fully override the default formatting function
			--   return true
			-- end
		},
		-- easily add or disable built in mappings added during LSP attaching
		mappings = {
			n = {},
		},
		-- add to the global LSP on_attach function
		-- on_attach = function(client, bufnr)
		-- end,

		-- override the mason server-registration function
		-- server_registration = function(server, opts)
		--   require("lspconfig")[server].setup(opts)
		-- end,

		-- Add overrides for LSP server settings, the keys are the name of the server
		["server-settings"] = {
			-- example for addings schemas to yamlls
			-- yamlls = { -- override table for require("lspconfig").yamlls.setup({...})
			--   settings = {
			--     yaml = {
			--       schemas = {
			--         ["http://json.schemastore.org/github-workflow"] = ".github/workflows/*.{yml,yaml}",
			--         ["http://json.schemastore.org/github-action"] = ".github/action.{yml,yaml}",
			--         ["http://json.schemastore.org/ansible-stable-2.9"] = "roles/tasks/*.{yml,yaml}",
			--       },
			--     },
			--   },
			-- },
		},
	},

	-- Mapping data with "desc" stored directly by vim.keymap.set().
	--
	-- Please use this mappings table to set keyboard mapping since this is the
	-- lower level configuration and more robust one. (which-key will
	-- automatically pick-up stored data by this setting.)

	mappings = {
		-- first key is the mode
		n = {
			-- second key is the lefthand side of the map
			-- mappings seen under group name "Buffer"
			["<leader>bb"] = { "<cmd>tabnew<cr>", desc = "New tab" },
			["<leader>bc"] = { "<cmd>BufferLinePickClose<cr>", desc = "Pick to close" },
			["<leader>bj"] = { "<cmd>BufferLinePick<cr>", desc = "Pick to jump" },
			["<leader>bt"] = { "<cmd>BufferLineSortByTabs<cr>", desc = "Sort by tabs" },
			-- navigating wrapped lines
			["j"] = { "gj", desc = "Navigate down" },
			["k"] = { "gk", desc = "Navigate down" },
			["<leader>ff"] = {
				function()
					require("telescope.builtin").find_files()
				end,
				desc = "Search files",
			},
			["<leader>fw"] = {
				function()
					require("telescope.builtin").live_grep()
				end,
				desc = "Search words",
			},
			[";"] = { ":" },
			[":"] = { ";" },
			["g:"] = { "g;" },
			["q;"] = { "q:" },
			["@;"] = { "@;" },
			["<leader>es"] = { "<cmd>split ~/.config/nvim/lua/user/init.lua<cr>", desc = "Split config file" },
			["<leader>ss"] = { "<cmd>source ~/.config/nvim/init.lua<cr>", desc = "Source config file" },

			["<C-s>"] = { "<cmd>w!<cr>", desc = "Save File" },

			["yod"] = {
				function()
					astronvim.ui.toggle_diagnostics()
				end,
				desc = "Toggle diagnostics",
			},
			["yog"] = {
				function()
					astronvim.ui.toggle_signcolumn()
				end,
				desc = "Toggle signcolumn",
			},
			["yoi"] = {
				function()
					astronvim.ui.set_indent()
				end,
				desc = "Change indent setting",
			},
			["yol"] = {
				function()
					astronvim.ui.toggle_statusline()
				end,
				desc = "Toggle statusline",
			},
			["yon"] = {
				function()
					astronvim.ui.change_number()
				end,
				desc = "Change line numbering",
			},
			["yos"] = {
				function()
					astronvim.ui.toggle_spell()
				end,
				desc = "Toggle spellcheck",
			},
			["yop"] = {
				function()
					astronvim.ui.toggle_paste()
				end,
				desc = "Toggle paste mode",
			},
			["yot"] = {
				function()
					astronvim.ui.toggle_tabline()
				end,
				desc = "Toggle tabline",
			},
			["you"] = {
				function()
					astronvim.ui.toggle_url_match()
				end,
				desc = "Toggle URL highlight",
			},
			["yow"] = {
				function()
					astronvim.ui.toggle_wrap()
				end,
				desc = "Toggle wrap",
			},
			["yoy"] = {
				function()
					astronvim.ui.toggle_syntax()
				end,
				desc = "Toggle syntax highlight",
			},
		},
		v = {
			[";"] = { ":" },
		},
		i = {
			["<C-l>"] = { "<c-g>u<Esc>[s1z=`]a<c-g>u", desc = "Fix last misspell" },
			["<C-s>"] = { "<C-o>:w!<cr>", desc = "Save File" },
		},
	},
	-- Configure plugins
	plugins = {

		cmp = {
			mapping = {
				["<C-j>"] = require("cmp").mapping.confirm({ select = false }),
				["<A-k>"] = require("cmp").mapping(require("cmp").mapping.scroll_docs(-5), { "i", "c" }),
				["<A-j>"] = require("cmp").mapping(require("cmp").mapping.scroll_docs(5), { "i", "c" }),
			},
		},
		telescope = {
			defaults = {
				mappings = {
					i = {
						["<esc>"] = require("telescope.actions").close,
						["<C-u>"] = false,
						["<C-[>"] = require("telescope.actions").close,
						["<C-j>"] = require("telescope.actions").select_default,
						["<C-d>"] = require("telescope.actions").delete_buffer,
						["<C-s>"] = require("telescope.actions").select_horizontal,
						["<C-n>"] = require("telescope.actions").move_selection_next,
						["<C-p>"] = require("telescope.actions").move_selection_previous,
						["<A-j>"] = require("telescope.actions").cycle_previewers_next,
						["<A-k>"] = require("telescope.actions").cycle_previewers_prev,
					},
				},
			},
			pickers = {
				find_files = {
					find_command = { "fd", "-L", "--type", "f", "--strip-cwd-prefix" },
				},
				live_grep = {
					additional_args = { "-L" },
				},
			},
		},
		init = {
			["goolord/alpha-nvim"] = { disable = true },
			["numToStr/Comment.nvim"] = { disable = true }, -- Using vim-commentary
			["max397574/better-escape.nvim"] = { disable = true },
			{
				"nvim-telescope/telescope-bibtex.nvim",
				after = {
					"telescope.nvim",
				},
				config = function()
					require("telescope").load_extension("bibtex")
				end,
			},
			{ "https://github.com/andymass/vim-matchup" },
			{
				"https://github.com/echasnovski/mini.nvim",
				config = function()
					require("mini.surround").setup({
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
					})
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
				"https://github.com/nvim-treesitter/nvim-treesitter-textobjects",
				after = "nvim-treesitter",
			},
			{
				"https://github.com/ziontee113/syntax-tree-surfer",
				after = "nvim-treesitter",
			},
			{
				"https://github.com/linty-org/readline.nvim",
				config = function()
					local readline = require("readline")
					vim.keymap.set("!", "<C-k>", readline.kill_line)
					vim.keymap.set("!", "<C-u>", readline.backward_kill_line)
					vim.keymap.set("!", "<M-d>", readline.kill_word)
					vim.keymap.set("!", "<M-BS>", readline.backward_kill_word)
					vim.keymap.set("!", "<C-d>", "<Delete>") -- delete-char
					vim.keymap.set("!", "<C-h>", "<BS>") -- backward-delete-char
					vim.keymap.set("!", "<C-a>", readline.beginning_of_line)
					-- vim.keymap.set("!", "<C-e>", readline.end_of_line) -- used by luasnip to switch between choices
					vim.keymap.set("!", "<M-f>", readline.forward_word)
					vim.keymap.set("!", "<M-b>", readline.backward_word)
					vim.keymap.set("!", "<C-f>", "<Right>") -- forward-char
					vim.keymap.set("!", "<C-b>", "<Left>") -- backward-char
				end,
			},
			{ "https://github.com/tpope/vim-unimpaired" },
			{ "https://github.com/tpope/vim-repeat" }, -- Used to repeat vim-unimpaired actions
			{
				"https://github.com/folke/todo-comments.nvim",
				requires = "nvim-lua/plenary.nvim",
				config = function()
					require("todo-comments").setup({
						signs = false,
						keywords = {
							REFACTOR = { icon = "" },
						},
					})
				end,
			},
			{ "https://github.com/lervag/vimtex" },
			{ "https://github.com/jbyuki/nabla.nvim" },
			{ "https://github.com/YasserKa/vim-sxhkdrc" },
			{ "https://github.com/jpalardy/vim-slime" },
			{
				"https://github.com/hanschen/vim-ipython-cell",
				ft = "python",
				config = function()
					local wk = require("which-key")
					wk.register({
						["<localleader>"] = {
							n = {
								r = { ":w<CR>:IPythonCellRun<CR>", "Run file" },
								R = { ":w<CR>:IPythonCellRunTime<CR>", "Run file with timer" },
								c = { ":IPythonCellExecuteCell<CR>", "Execute cell" },
								vc = { " :IPythonCellExecuteVerboseCell<CR>", "Execute cell verbosely" },
								C = { ":IPythonCellExecuteCellJump<CR>", "Execute cell and jump to next" },
								vC = {
									" :IPythonCellExecuteCellVerboseJump<CR>",
									"Execute cell verbosly and jump to next",
								},
								l = { ":IPythonCellClear<CR>", "Clear shell" },
								x = { ":IPythonCellClose<CR>", "Close shell" },
								Q = { ":IPythonCellRestart<CR>", "Restart shell" },
								p = { ":IPythonCellPrevCommand<CR>", "Execute last command" },
								s = { ":SlimeSend1 ipython --matplotlib<CR>", "Start shell" },
								h = { "<Plug>SlimeLineSend", "Send line" },
								d = { ":SlimeSend1 %debug<CR>", "Execute cell with debug" },
								q = { ":SlimeSend1 exit<CR>", "Exit" },
								m = { "<Plug>IPythonCellToMarkdown", "To markdown" },
								I = { ":IPythonCellInsertAbove<CR>o", "Insert cell above" },
								i = { ":IPythonCellInsertBelow<CR>o", "Insert cell below" },
							},
						},
						["[c"] = { ":IPythonCellPrevCell<CR>", "Previous Cell" },
						["]c"] = { ":IPythonCellNextCell<CR>", "Next Cell" },
					})
					wk.register({
						["<C-,>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above" },
						["<F2>nI"] = { "<C-o>:IPythonCellInsertAbove<CR><CR>", "Insert cell above" },

						["<C-,>ni"] = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below" },
						["<F2>ni"] = { "<C-o>:IPythonCellInsertBelow<CR><CR>", "Insert cell below" },
					}, { mode = "i" })
					wk.register({
						["<localleader>"] = {
							n = {
								name = "python-cell",
								h = { "<Plug>SlimeRegionSend", "Send shell" },
							},
						},
					}, { mode = "v" })
				end,
			},
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
			{ "https://github.com/fladson/vim-kitty" },
			{ "https://github.com/tpope/vim-fugitive" },
			{ "https://github.com/terryma/vim-expand-region" },
			{ "https://github.com/jeetsukumaran/vim-commentary" },
			{ "https://github.com/szw/vim-maximizer" },
			{ "https://github.com/simnalamburt/vim-mundo" },
			{ "https://github.com/sheerun/vim-polyglot" }, -- provides better indentation & syntax highlight
			{
				"https://github.com/ggandor/leap.nvim",
				config = function()
					require("leap").add_default_mappings()
				end,
			},
			{
				"https://github.com/iamcco/markdown-preview.nvim",
				run = "cd app && npm install",
				setup = function()
					vim.g.mkdp_filetypes = { "markdown", "plantuml" }
				end,
				ft = { "markdown", "plantuml" },
			},
			{
				"https://github.com/kdheepak/cmp-latex-symbols",
				after = "nvim-cmp",

				config = function()
					astronvim.add_user_cmp_source("latex_symbols")
				end,
			},
			{
				"https://github.com/danymat/neogen",
				config = function()
					require("neogen").setup({
						snippet_engine = "luasnip",
					})
				end,
				requires = "nvim-treesitter/nvim-treesitter",
			},
			{ "https://github.com/romainl/vim-cool" }, -- Disable search highlighting when done}
		},
		["null-ls"] = function(config) -- overrides `require("null-ls").setup(config)`
			-- config variable is the default configuration table for the setup function call
			local null_ls = require("null-ls")

			-- Supported formatters and linters
			-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/formatting
			-- https://github.com/jose-elias-alvarez/null-ls.nvim/tree/main/lua/null-ls/builtins/diagnostics
			config.sources = {
				null_ls.builtins.diagnostics.flake8.with({
					-- extra_args = { "--max-line-length=88", "--extend-ignore=E203" },
				}),
				-- null_ls.builtins.diagnostics.mypy,
				-- Set a formatter
				-- null_ls.builtins.formatting.stylua,
				-- null_ls.builtins.formatting.prettier,
			}
			return config -- return final config table
		end,
		treesitter = { -- overrides `require("treesitter").setup(...)`
			-- ensure_installed = { "lua" },

			indent = { enable = true, disable = { "python" } },
			matchup = { enable = true },
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
		},
		-- use mason-lspconfig to configure LSP installations
		["mason-lspconfig"] = { -- overrides `require("mason-lspconfig").setup(...)`
			-- ensure_installed = { "sumneko_lua" },
		},
		-- use mason-null-ls to configure Formatters/Linter installation for null-ls sources
		["mason-null-ls"] = { -- overrides `require("mason-null-ls").setup(...)`
			-- ensure_installed = { "prettier", "stylua" },
		},
	},

	-- LuaSnip Options
	luasnip = {
		-- Add paths for including more VS Code style snippets in luasnip
		vscode_snippet_paths = {},
		history = true,
		enable_autosnippets = true,
		-- Extend filetypes
		filetype_extend = {
			-- javascript = { "javascriptreact" },
		},
		mapping = {},
	},

	-- CMP Source Priorities
	-- modify here the priorities of default cmp sources
	-- higher value == higher priority
	-- The value can also be set to a boolean for disabling default sources:
	-- false == disabled
	-- true == 1000
	cmp = {
		source_priority = {
			nvim_lsp = 1000,
			luasnip = 750,
			latex_symbols = 700,
			buffer = 500,
			path = 250,
		},
	},

	-- Modify which-key registration (Use this with mappings table in the above.)
	["which-key"] = {
		-- Add bindings which show up as group name
		register = {
			-- first key is the mode, n == normal mode
			n = {
				-- ["]"] = {
				-- 	f = "Next function start",
				-- 	x = "Next class start",
				-- 	F = "Next function end",
				-- 	X = "Next class end",
				-- },
				-- ["["] = {
				-- 	f = "Previous function start",
				-- 	x = "Previous class start",
				-- 	F = "Previous function end",
				-- 	X = "Previous class end",
				-- },
				["yex"] = { "<cmd>Neotree toggle<cr>", "Toggle Explorer" },
				["<space><space>"] = { "<cmd>buffer#<cr>", "Alternate buffer" },
				-- second key is the prefix, <leader> prefixes
				["<localleader>"] = {
					l = {},
					m = {
						function()
							require("nabla").popup()
						end,
						"Preview Math",
					},
					M = {
						function()
							require("nabla").toggle_virt()
						end,
						"Preview Math",
					},
				},
				["<leader>"] = {
					["."] = { "<cmd>cd %:p:h<cr>", "Set CWD" },
					-- third key is the key to bring up next level and its displayed
					-- group name in which-key top level menu
					["b"] = {
						name = "Buffer",
						["O"] = { "<cmd>silent :%bdelete | edit# | bdelete#<cr>", "Remove all other buffers" },
					},
					g = {
						S = {
							function()
								require("gitsigns").stage_buffer()
							end,
							"Stage Buffer",
						},
						U = {
							function()
								require("gitsigns").reset_buffer_index()
							end,
							"Unstage Buffer",
						},
					},
					a = {
						name = "Annotate",
						["<cr>"] = {
							function()
								require("neogen").generate()
							end,
							"Current",
						},
						c = {
							function()
								require("neogen").generate({ type = "class" })
							end,
							"Class",
						},
						f = {
							function()
								require("neogen").generate({ type = "func" })
							end,
							"Function",
						},
						t = {
							function()
								require("neogen").generate({ type = "type" })
							end,
							"Type",
						},
						F = {
							function()
								require("neogen").generate({ type = "file" })
							end,
							"File",
						},
					},
					f = {
						name = "Telescope",
						["?"] = { "<cmd>Telescope help_tags<cr>", "Find help" },
						["'"] = { "<cmd>Telescope marks<cr>", "Marks" },
						B = { "<cmd>Telescope bibtex<cr>", "BibTeX" },
					},
				},
			},
			x = {
				["il"] = { "g_o^", "Inside line text object" },
				["al"] = { "$o^", "Around line text object" },
			},
			o = {
				-- line text-objects
				["il"] = { ":normal vil<cr>", "Inside line text object" },
				["al"] = { ":normal val<cr>", "Around line text object" },
			},
		},
	},
	["nvim-autopairs"] = {
		add_rules = function()
			local Rule = require("nvim-autopairs.rule")

			return {
				Rule("$", "$", { "tex", "latex", "plaintex" }),
			}
		end,
	},

	-- This function is run last and is a good place to configuring
	-- augroups/autocommands and custom filetypes also this just pure lua so
	-- anything that doesn't fit in the normal config locations above can go here
	polish = function()
		vim.api.nvim_create_augroup("my_skeletons", { clear = true })
		vim.api.nvim_create_autocmd("BufNewFile", {
			desc = "Skeleton",
			group = "my_skeletons",
			pattern = { "*.sh" },
			command = "0r ~/.config/nvim/skeletons/skeleton.sh | exe 'normal jo' | startinsert",
		})
		vim.api.nvim_create_autocmd("BufNewFile", {
			desc = "Skeleton",
			group = "my_skeletons",
			pattern = { "*.bash" },
			command = "0r ~/.config/nvim/skeletons/skeleton.bash | exe 'normal jo' | startinsert",
		})
		vim.api.nvim_create_autocmd("BufNewFile", {
			desc = "Skeleton",
			group = "my_skeletons",
			pattern = "*.py",
			command = "0r ~/.config/nvim/skeletons/skeleton.py | exe 'normal jo' | startinsert",
		})
		vim.api.nvim_create_augroup("user_mail", {
			clear = true,
		})
		-- heirline starts flickering while using vimtex & cmdheight=0
		-- Problem from cmdheight
		-- https://github.com/lervag/vimtex/issues/2516
		-- https://github.com/AstroNvim/AstroNvim/issues/1124
		vim.api.nvim_create_autocmd({ "FileType" }, {
			pattern = { "tex" },
			command = "set cmdheight=1",
		})

		vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
			desc = "Settings for mail files",
			group = "user_mail",
			pattern = "neomutt-*",
			command = "set spell textwidth=100",
		})
		vim.api.nvim_create_autocmd({ "BufRead" }, {
			desc = "Settings for mail files",
			group = "user_mail",
			pattern = "neomutt-*",
			command = "normal 50%",
		})

		vim.api.nvim_create_autocmd({ "FileType" }, {
			pattern = { "markdown", "tex" },
			command = "set spell textwidth=100",
		})
		vim.api.nvim_create_augroup("user_markdown", {
			clear = true,
		})

		-- Yank org links to files
		_G.encodeChar = function(chr)
			return string.format("%%%X", string.byte(chr))
		end

		_G.encodeString = function(str)
			local output, _ = string.gsub(str, "[^%w]", encodeChar)
			return output
		end

		_G.YankOrgLink = function()
			local r, _ = unpack(vim.api.nvim_win_get_cursor(0))
			local cmd = '"${TERMINAL}" --directory "'
				.. vim.fn.expand("%:p:h")
				.. '" --detach -e "${EDITOR}" +'
				.. r
				.. ' "'
				.. vim.fn.expand("%:t")
				.. '"'
			local encoded_org_link = "[["
				.. "link-handler://"
				.. encodeString(cmd)
				.. "]["
				.. vim.fn.expand("%:t")
				.. "]]"
			vim.fn.setreg("+", encoded_org_link)
		end
		vim.api.nvim_create_user_command("YankOrgLink", _G.YankOrgLink, {})

		_G.WatchFile = function()
			vim.cmd(
				'silent !"${TERMINAL}" --directory "'
					.. vim.fn.expand("%:p:h")
					.. '" bash -c \'echo "'
					.. vim.fn.expand("%:t")
					.. "\" | entr -c /_'"
			)
		end
		vim.api.nvim_create_user_command("WatchFile", _G.WatchFile, {})

		vim.keymap.set("x", "@", '":norm @" . getcharstr() . "<cr>"', { expr = true })
		vim.keymap.set("n", "<leader>c", MiniBufremove.delete) -- Remove buffer, but keep split
		vim.keymap.set("n", "<leader>C", "<cmd>bdelete<cr>")

		vim.keymap.set("n", "<C-S-h>", require("smart-splits").resize_left)
		vim.keymap.set("n", "<C-S-j>", require("smart-splits").resize_down)
		vim.keymap.set("n", "<C-S-k>", require("smart-splits").resize_up)
		vim.keymap.set("n", "<C-S-l>", require("smart-splits").resize_right)

		vim.keymap.set("n", "n", "nzzzv")
		vim.keymap.set("n", "J", "mzJ`z") -- Join line without moving cursor
		vim.keymap.set("n", "N", "Nzzzv")

		local Path = require("plenary.path")
		require("session_manager").setup({
			autoload_mode = require("session_manager.config").AutoloadMode.CurrentDir,
		})
		require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets" })
		local unmap = vim.api.nvim_del_keymap
		unmap("n", "<C-q>")
		unmap("n", "<C-Up>")
		unmap("n", "<C-Down>")
		unmap("n", "<C-Left>")
		unmap("n", "<C-Right>")
		local utils = require("user.utils")

		vim.keymap.set("n", "<localleader>c", function()
			utils.vim_opt_toggle("conceallevel", 2, 0, "Conceal")
		end)
		local luasnip = require("luasnip")
		vim.keymap.set({ "i" }, "<C-e>", function()
			if luasnip.choice_active() then
				luasnip.change_choice(1)
			else
				require("readline").end_of_line()
			end
		end)

		require("syntax-tree-surfer") -- {{{
		-- Syntax Tree Surfer
		-- local opts = { noremap = true, silent = true }

		-- Normal Mode Swapping:
		-- Swap The Master Node relative to the cursor with it's siblings, Dot Repeatable
		-- vim.keymap.set("n", "vU", function()
		-- 	vim.opt.opfunc = "v:lua.STSSwapUpNormal_Dot"
		-- 	return "g@l"
		-- end, { silent = true, expr = true })
		-- vim.keymap.set("n", "vD", function()
		-- 	vim.opt.opfunc = "v:lua.STSSwapDownNormal_Dot"
		-- 	return "g@l"
		-- end, { silent = true, expr = true })

		-- -- Swap Current Node at the Cursor with it's siblings, Dot Repeatable
		-- vim.keymap.set("n", "vd", function()
		-- 	vim.opt.opfunc = "v:lua.STSSwapCurrentNodeNextNormal_Dot"
		-- 	return "g@l"
		-- end, { silent = true, expr = true })
		-- vim.keymap.set("n", "vu", function()
		-- 	vim.opt.opfunc = "v:lua.STSSwapCurrentNodePrevNormal_Dot"
		-- 	return "g@l"
		-- end, { silent = true, expr = true })

		-- -- Visual Selection from Normal Mode
		-- vim.keymap.set("n", "<leader>su", "<cmd>STSSelectMasterNode<cr>", opts)
		-- vim.keymap.set("n", "vn", "<cmd>STSSelectCurrentNode<cr>", opts)
		-- -- O in insert mode
		-- vim.keymap.set("n", "gO", function()
		-- 	require("syntax-tree-surfer").go_to_top_node_and_execute_commands(
		-- 		false,
		-- 		{ "normal! O", "normal! O", "startinsert" }
		-- 	)
		-- end, opts)

		-- Select Nodes in Visual Mode
		--vim.keymap.set("x", "J", "<cmd>STSSelectNextSiblingNode<cr>", opts)
		--vim.keymap.set("x", "K", "<cmd>STSSelectPrevSiblingNode<cr>", opts)
		--vim.keymap.set("x", "H", "<cmd>STSSelectParentNode<cr>", opts)
		--vim.keymap.set("x", "L", "<cmd>STSSelectChildNode<cr>", opts)

		---- Swapping Nodes in Visual Mode
		--vim.keymap.set("x", "<A-j>", "<cmd>STSSwapNextVisual<cr>", opts)
		--vim.keymap.set("x", "<A-k>", "<cmd>STSSwapPrevVisual<cr>", opts)
		---- Syntax Tree Surfer V2 Mappings
		---- Targeted Jump with virtual_text
		local sts = require("syntax-tree-surfer")
		--vim.keymap.set("n", "gsv", function() -- only jump to variable_declarations
		--	sts.targeted_jump({ "variable_declaration" })
		--end, opts)
		--vim.keymap.set("n", "gfu", function() -- only jump to functions
		--	sts.targeted_jump({ "function", "arrrow_function", "function_definition" })
		--	--> In this example, the Lua language schema uses "function",
		--	--  when the Python language uses "function_definition"
		--	--  we include both, so this keymap will work on both languages
		--end, opts)
		--vim.keymap.set("n", "gif", function() -- only jump to if_statements
		--	sts.targeted_jump({ "if_statement" })
		--end, opts)
		--vim.keymap.set("n", "gfo", function() -- only jump to for_statements
		--	sts.targeted_jump({ "for_statement" })
		--end, opts)
		--vim.keymap.set("n", "gj", function() -- jump to all that you specify
		--	sts.targeted_jump({
		--		"function",
		--		"if_statement",
		--		"else_clause",
		--		"else_statement",
		--		"elseif_statement",
		--		"for_statement",
		--		"while_statement",
		--		"switch_statement",
		--	})
		--end, opts)

		---------------------------------
		---- filtered_jump --
		---- "default" means that you jump to the default_desired_types or your lastest jump types
		---- vim.keymap.set("n", "<A-n>", function()
		---- 	sts.filtered_jump("default", true) --> true means jump forward
		---- end, opts)
		---- vim.keymap.set("n", "<A-p>", function()
		---- 	sts.filtered_jump("default", false) --> false means jump backwards
		---- end, opts)
		--vim.keymap.set("n", "gj", function() -- jump to all that you specify
		--	sts.targeted_jump({
		--		"function",
		--		"if_statement",
		--		"else_clause",
		--		"else_statement",
		--		"elseif_statement",
		--		"for_statement",
		--		"while_statement",
		--		"switch_statement",
		--	})
		--end, opts)

		-- Jump between nodes
		-- vim.keymap.set("n", "<A-n>", function()
		-- 	sts.filtered_jump({
		-- 		"function",
		-- 		"class",
		-- 		"arrow_function",
		-- 		"variable_assignment",
		-- 		"function_definition",
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 		"elseif_statement",
		-- 		"for_statement",
		-- 		"while_statement",
		-- 		"switch_statement",
		-- 	}, true)
		-- end, opts)
		-- vim.keymap.set("n", "<A-p>", function()
		-- 	sts.filtered_jump({
		-- 		"function",
		-- 		"class",
		-- 		"arrow_function",
		-- 		"function_definition",
		-- 		"variable_assignment",
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 		"elseif_statement",
		-- 		"for_statement",
		-- 		"while_statement",
		-- 		"switch_statement",
		-- 	}, false)
		-- end, opts)

		-- Navigate between elements
		vim.keymap.set("n", "]f", function()
			sts.filtered_jump({
				"if_statement",
			}, true)
		end, opts)
		vim.keymap.set("n", "[f", function()
			sts.filtered_jump({
				"if_statement",
			}, false)
		end, opts)

		vim.keymap.set("n", "]c", function()
			sts.filtered_jump({
				"class",
			}, true)
		end, opts)
		vim.keymap.set("n", "[c", function()
			sts.filtered_jump({
				"class",
			}, false)
		end, opts)

		vim.keymap.set("n", "]/", function()
			sts.filtered_jump({
				"comment",
			}, true)
		end, opts)
		vim.keymap.set("n", "[/", function()
			sts.filtered_jump({
				"comment",
			}, false)
		end, opts)

		-- "default" means that you jump to the default_desired_types or your lastest jump types
		vim.keymap.set("n", "<A-n>", function()
			sts.filtered_jump("default", true) --> true means jump forward
		end, opts)
		vim.keymap.set("n", "<A-p>", function()
			sts.filtered_jump("default", false) --> false means jump backwards
		end, opts)
		-------------------------------
		-- jump with limited targets --
		-- jump to sibling nodes only
		-- vim.keymap.set("n", "-", function()
		-- 	sts.filtered_jump({
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 	}, false, { destination = "siblings" })
		-- end, opts)
		-- vim.keymap.set("n", "=", function()
		-- 	sts.filtered_jump({ "if_statement", "else_clause", "else_statement" }, true, { destination = "siblings" })
		-- end, opts)

		-- local gen_spec = require("mini.ai").gen_spec
		-- require("mini.ai").setup({
		-- 	custom_textobjects = {
		-- 		["*"] = gen_spec.pair("*", "*", { type = "greedy" }),
		-- 		["_"] = gen_spec.pair("_", "_", { type = "greedy" }),
		-- 		B = gen_spec.treesitter({ a = "@block.outer", i = "@block.inner" }),
		-- 		C = gen_spec.treesitter({ a = "@conditional.outer", i = "@conditional.inner" }),
		-- 		F = gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
		-- 		L = gen_spec.treesitter({ a = "@loop.outer", i = "@loop.inner" }),
		-- 		P = gen_spec.treesitter({ a = "@parameter.outer", i = "@parameter.inner" }),
		-- 		x = gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
		-- 	},
		-- })
		-- jump to parent or child nodes only
		-- vim.keymap.set("n", "_", function()
		-- 	sts.filtered_jump({
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 	}, false, { destination = "parent" })
		-- end, opts)
		-- vim.keymap.set("n", "+", function()
		-- 	sts.filtered_jump({
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 	}, true, { destination = "children" })
		-- end, opts)

		-- -- Setup Function example:
		-- -- These are the default options:
		-- require("syntax-tree-surfer").setup({
		-- 	highlight_group = "STS_highlight",
		-- 	disable_no_instance_found_report = false,
		-- 	default_desired_types = {
		-- 		"function",
		-- 		"arrow_function",
		-- 		"function_definition",
		-- 		"if_statement",
		-- 		"else_clause",
		-- 		"else_statement",
		-- 		"elseif_statement",
		-- 		"for_statement",
		-- 		"while_statement",
		-- 		"switch_statement",
		-- 	},
		-- 	left_hand_side = "fdsawervcxqtzb",
		-- 	right_hand_side = "jkl;oiu.,mpy/n",
		-- 	icon_dictionary = {
		-- 		["if_statement"] = "",
		-- 		["else_clause"] = "",
		-- 		["else_statement"] = "",
		-- 		["elseif_statement"] = "",
		-- 		["for_statement"] = "ﭜ",
		-- 		["while_statement"] = "ﯩ",
		-- 		["switch_statement"] = "ﳟ",
		-- 		["function"] = "",
		-- 		["function_definition"] = "",
		-- 		["variable_declaration"] = "",
		-- 	},
		-- })

		--  }}}
		-- {{{ vim-expand-region
		vim.keymap.set({ "n", "x" }, "+", "<Plug>(expand_region_expand)")
		vim.keymap.set({ "n", "x" }, "_", "<Plug>(expand_region_shrink)")
		-- }}}
		-- {{{ vim-mundo
		-- Enable persistent undo so that undo history persists across vim sessions
		-- set undofile
		vim.opt.undofile = true
		vim.keymap.set({ "n" }, "yeu", "<cmd>MundoToggle<cr>")
		-- }}}
		-- {{{ vim-cool
		-- Show number of matches in command-line
		vim.g["CoolTotalMatches"] = 1
		-- }}}
		vim.api.nvim_create_autocmd({ "FileType" }, {
			pattern = { "help", "man" },
			callback = function()
				vim.cmd([[
				nnoremap <silent> <buffer> q :close<CR>
		nnoremap <silent> <buffer> <esc> :close<CR>
		set nobuflisted
		]])
			end,
		})
		-- }}}
		vim.api.nvim_create_autocmd({ "FileType" }, {
			pattern = { "markdown" },
			callback = function()
				vim.cmd([[
				hi MyStrikethrough gui=strikethrough
call matchadd('MyStrikethrough', '\~\~\zs.\+\ze\~\~')
call matchadd('Conceal',  '\~\~\ze.\+\~\~', 10, -1, {'conceal':''})
call matchadd('Conceal',  '\~\~.\+\zs\~\~\ze', 10, -1, {'conceal':''})

hi MyUnderlineMatch gui=underline
call matchadd('MyUnderlineMatch', '__\zs[^X]\+\ze__')
call matchadd('Conceal',  '__\ze[^X]\+__', 10, -1, {'conceal':''})
call matchadd('Conceal',  '__[^X]\+\zs__\ze', 10, -1, {'conceal':''})

		]])
			end,
		})

		-- {{{ vim-maximizer
		vim.keymap.set("n", "<C-w>m", "<cmd>MaximizerToggle!<CR>")
		-- }}}
		vim.api.nvim_exec(
			[[
		xnoremap gcc :Commentary<cr>

              augroup TMP_FILES
              autocmd!
              autocmd BufRead,BufNewFile tmp.* inoremap <C-c><C-c> <esc>:q<cr>
              autocmd BufRead,BufNewFile tmp.* set noswapfile
              autocmd ExitPre tmp.* :w
              augroup END
              let g:python3_host_prog  = '/bin/python3.10'
              let g:ipython_cell_run_command	= '%run -t "{filepath}"'


              " {{{ vim-ipython-cell / vim-slime
              " Slime
              " always use tmux
              let g:slime_target = 'tmux'

              " https://github.com/jpalardy/vim-slime/tree/main/ftplugin/python
              let g:slime_bracketed_ipython = 1

              " always send text to the top-right pane in the current tmux tab without asking
              let g:slime_default_config = {
              \ 'socket_name': get(split($TMUX, ','), 0),
              \ 'target_pane': ':{next}.1' }

              let g:slime_dont_ask_default = 1

              " Override the comment that makes a cell take "##", this will cause a problem if
              " there's a string having "##"
              let g:ipython_cell_tag = ['# %%']

              " }}}
              "  {{{ markdown-preview.nvim
              let g:mkdp_command_for_global = 1
              let g:mkdp_page_title = '${name}'
              let g:mkdp_auto_close = 0
              nmap yem <Plug>MarkdownPreviewToggle

              " open page in new window
              function! OpenNewBrowserWindow(url)
              execute "silent ! qutebrowser --target window " . a:url
              endfunction

              let g:mkdp_browserfunc = 'OpenNewBrowserWindow'
              " }}}
              " {{{ vimtex
              let g:vimtex_compiler_silent = 1
              " Use nabla.nvim
              " let g:vimtex_syntax_conceal_disable=1
              let g:tex_conceal = 'abdg'
              let g:vimtex_syntax_conceal = {
              \ 'accents': 1,
              \ 'ligatures': 1,
              \ 'cites': 1,
              \ 'fancy': 1,
              \ 'greek': 0,
              \ 'math_bounds': 0,
              \ 'math_delimiters': 0,
              \ 'math_fracs': 0,
              \ 'math_super_sub': 0,
              \ 'math_symbols': 0,
              \ 'sections': 0,
              \ 'styles': 1,
              \}
              let g:vimtex_view_method = 'zathura'
              let g:vimtex_fold_enabled = 1
              let g:vimtex_compiler_latexmk = {
              \ 'build_dir' : './tex_output',
              \ 'options' : [
              \   '-verbose',
              \   '-file-line-error',
              \   '-shell-escape',
              \   '-synctex=1',
              \   '-interaction=nonstopmode',
              \ ],
              \}

              vnoremap <silent> <leader>lu <ESC>:set nohlsearch<CR>:set textwidth=1000<CR>`>a#<ESC>`<i#<ESC> <bar>
              \ :s/#\(\_[^#]*\)#/\=trim(system("latex_to_unicode '".trim(submatch(1))."'"))
              \ <CR> `<
              \ :let @/ = "" <bar> set hlsearch<CR>:set textwidth=80<CR>
              " Surround capital characters with $
              vnoremap <silent> <leader>l$ <ESC>:set nohlsearch<CR>gv :substitute:\(\u\)\(\s\\|\.\\|,\\|(\):$\1$\2:gc <bar>
              \ :let @/ = "" <bar> set hlsearch<CR>
              " }}}
      	      ]],
			true
		)
	end,
}

return config
