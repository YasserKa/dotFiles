from  i3ipc import Connection

# Get workspace number for the window 
i3 = Connection()
containers = i3.get_tree().leaves()
workspace_num = [x.workspace().num for x in containers if x.window_title == "emacs_org_name"][0]

NAME = "emacs_org_name"
COMMAND_TO_RUN=f"emacs --title={NAME} --file=$NOTES_ORG_HOME/general.org"

window_exists = window.wait_for_exist(NAME, timeOut=0.01)

if not window_exists:
    system.exec_command(COMMAND_TO_RUN)
    window.wait_for_exist(NAME, timeOut=5)
    
system.exec_command(f"i3-msg \"[title=\"{NAME}\"] floating enable, move container workspace current\"")
window.activate(NAME, True)
window.wait_for_focus(NAME, timeOut=5)

keyboard.send_keys("<alt>+<shift>+;(org-capture-full-screen)<enter>")

retCode = keyboard.wait_for_keypress('c',modifiers=['<ctrl>'],timeOut=999)
retCode = keyboard.wait_for_keypress('c',modifiers=['<ctrl>'],timeOut=2)

# Place the window to it's previous workspace
if retCode:
    system.exec_command(f"i3-msg \"[title=\"{NAME}\"] move --no-auto-back-and-forth container workspace {workspace_num}, floating disable\"")




