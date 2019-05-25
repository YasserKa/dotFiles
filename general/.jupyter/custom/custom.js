// Configure CodeMirror Keymap
require([
  'nbextensions/vim_binding/vim_binding',   // depends your installation
], function() {
  CodeMirror.Vim.mapCommand(":", "motion", "repeatLastCharacterSearch", args = { forward: true });
  CodeMirror.Vim.mapCommand(";", "ex");
});

// Configure Jupyter Keymap
require([
  'nbextensions/vim_binding/vim_binding',
  'base/js/namespace',
], function(vim_binding, ns) {
  // Add post callback
  vim_binding.on_ready_callbacks.push(function(){
    var km = ns.keyboard_manager;
    // Allow Ctrl-2 to change the cell mode into Markdown in Vim normal mode
      km.edit_shortcuts.add_shortcut('ctrl-[', CodeMirror.prototype.leaveInsertMode, true);
      km.edit_shortcuts.add_shortcut('shift-ctrl-[', CodeMirror.prototype.leaveNormalMode, true);
      // Update Help
      km.edit_shortcuts.events.trigger('rebuild.QuickHelp');

      // CodeMirror.Vim.defineOperator("comment_op", function(cm, ranges = []) {
      //      cm.toggleComment();
      // });
      //  CodeMirror.Vim.mapCommand("gc", "operator", "comment_op", {});

      CodeMirror.Vim.mapCommand(",", "operator", "yank");
      CodeMirror.Vim.defineAction("my_yank", function(cm) {
             document.execCommand("copy");
             CodeMirror.Vim.handleKey(cm, ',', 'mapping');
      });
       CodeMirror.Vim.mapCommand("y", "action", "my_yank", {});

  });
});
