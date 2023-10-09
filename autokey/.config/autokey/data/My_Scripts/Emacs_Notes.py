from  i3ipc import Connection

# Window title
NAME = "emacs_org"

# Get workspace number for the window 
i3 = Connection()
containers = i3.get_tree().leaves()
# Check the workspaces that contains the window
workspace_list = [x.workspace().num for x in containers if x.window_title == f"{NAME}"]

# Open Emacs if it's not open
if len(workspace_list) == 0:
    system.exec_command(f"emacs --title={NAME} --file=$_NOTES_ORG_HOME/capture.org")
    window.wait_for_exist(NAME, timeOut=10)
    workspace_num = 3
else:
    workspace_num = workspace_list[0]
    
system.exec_command(f"i3-msg \"[title=\"{NAME}\"] floating enable, move container workspace current move position center\"")
window.activate(NAME, True)
window.wait_for_focus(NAME, timeOut=5)

keyboard.send_keys("<alt>+<shift>+;(org-capture-full-screen)<enter>")

retCode = keyboard.wait_for_keypress('c',modifiers=['<ctrl>'],timeOut=999)
retCode = keyboard.wait_for_keypress('c',modifiers=['<ctrl>'],timeOut=2)

# Place the window to it's previous workspace
if retCode:
    system.exec_command(f"i3-msg \"[title=\"{NAME}\"] move --no-auto-back-and-forth container workspace {workspace_num}, floating disable\"")




