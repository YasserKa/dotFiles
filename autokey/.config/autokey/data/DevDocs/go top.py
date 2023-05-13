# Use <C-[> as <C-c> to get out of follow mode
retCode = keyboard.wait_for_keypress('g',timeOut=1)
if retCode:
    keyboard.send_keys("<ctrl>+<up>")