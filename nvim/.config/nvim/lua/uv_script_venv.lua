-- uv_script_venv.lua
-- Auto-detect PEP 723 "uv run --script" headers and wire up the venv
-- Requires uv >= 0.6.10 (for `uv python find --script`)
--
-- Drop this in ~/.config/nvim/lua/uv_script_venv.lua and
-- require("uv_script_venv") from init.lua, or paste the body
-- directly into init.lua.

local M = {}

local function first_line(bufnr) return (vim.api.nvim_buf_get_lines(bufnr, 0, 1, false) or { "" })[1] or "" end

local function is_uv_script(bufnr) return first_line(bufnr):match "uv run %-%-script" ~= nil end

-- Resolve the interpreter path for a uv script asynchronously.
-- `on_done(python_path)` is called with nil on failure.
local function resolve_uv_python(path, on_done)
  vim.system({ "uv", "python", "find", "--script", path }, { text = true }, function(res)
    vim.schedule(function()
      if res.code ~= 0 then
        on_done(nil, res.stderr)
        return
      end
      on_done(vim.trim(res.stdout))
    end)
  end)
end

local function activate(bufnr, python_path)
  -- venv root = parent of parent of the python binary (…/venv/bin/python)
  local venv_root = vim.fn.fnamemodify(python_path, ":h:h")

  -- 1. Session-wide env vars: affects :terminal, :!, jobstart, etc.
  vim.env.VIRTUAL_ENV = venv_root
  local sep = package.config:sub(1, 1) == "\\" and ";" or ":"
  if not vim.env.PATH:find(venv_root, 1, true) then vim.env.PATH = venv_root .. "/bin" .. sep .. vim.env.PATH end

  -- 2. Buffer-local var if you want to reference it elsewhere
  vim.b[bufnr].uv_script_python = python_path
  vim.b[bufnr].uv_script_venv = venv_root

  -- 3. Point already-attached LSP clients at the right interpreter
  for _, client in ipairs(vim.lsp.get_clients { bufnr = bufnr }) do
    if client.name == "pyright" or client.name == "basedpyright" then
      client.config.settings = vim.tbl_deep_extend("force", client.config.settings or {}, {
        python = { pythonPath = python_path },
      })
      client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
    elseif client.name == "pylsp" then
      client.config.settings = vim.tbl_deep_extend("force", client.config.settings or {}, {
        pylsp = { plugins = { jedi = { environment = venv_root } } },
      })
      client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
    end
  end

  vim.notify("uv script venv active: " .. venv_root, vim.log.levels.INFO, { title = "uv" })
end
local function handle_buffer(args)
  if not is_uv_script(args.buf) then return end
  local path = args.file
  resolve_uv_python(path, function(python_path, err)
    if not python_path or python_path == "" then
      -- Env probably doesn't exist yet (never run). Materialize it once,
      -- then resolve again.
      vim.notify("Creating uv script env (first run)...", vim.log.levels.INFO, { title = "uv" })
      vim.system({ "uv", "sync", "--script", path }, { text = true }, function(sync_res)
        vim.schedule(function()
          if sync_res.code ~= 0 then
            vim.notify("uv sync --script failed: " .. (sync_res.stderr or ""), vim.log.levels.WARN)
            return
          end
          resolve_uv_python(path, function(p2)
            if p2 and p2 ~= "" then activate(args.buf, p2) end
          end)
        end)
      end)
      return
    end
    activate(args.buf, python_path)
  end)
end

vim.api.nvim_create_autocmd({ "BufReadPost" }, {
  pattern = "*.py",
  callback = handle_buffer,
})

-- Manual re-sync/activate, e.g. after editing the [tool.uv] / deps in the header
vim.api.nvim_create_user_command(
  "UvScriptActivate",
  function() handle_buffer { buf = vim.api.nvim_get_current_buf(), file = vim.api.nvim_buf_get_name(0) } end,
  {}
)

return M
