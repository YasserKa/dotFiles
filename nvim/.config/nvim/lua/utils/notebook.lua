local M = {}

-- A buffer counts as a notebook if one of its first 30 lines is a
-- "# %%" cell marker or a jupytext header that marks it as py:percent.
function M.is_notebook(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, 30, false)) do
    if line:match "^#%s*%%%%" or line:match "^#%s*format_name:%s*percent" or line:match "py:percent" then
      return true
    end
  end
  return false
end

return M
