require('mini.align').setup()
require('mini.bufremove').setup()
require("indent_blankline").setup()
require('gitsigns').setup()

vim.keymap.set('n', '<leader>c', MiniBufremove.delete)
vim.keymap.set('n', '<leader>C', '<cmd>bdelete<cr>')

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
