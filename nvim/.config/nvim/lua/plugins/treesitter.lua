if true then return {} end -- WARN: REMOVE THIS LINE TO ACTIVATE THIS FILE

-- Customize Treesitter

---@type LazySpec
return {
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
 			context_commentstring = {
 				enable = true,
 				commentary_integration = { CommentaryLine = false },
 			},
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

}
