import os
import os.path
import readline
import atexit
import rlcompleter

history_file = os.path.expanduser('~/.python_history')
if not os.path.exists(history_file):
    open(history_file, "wb").close()

readline.read_history_file(history_file)
atexit.register(readline.write_history_file, history_file)

readline.parse_and_bind('tab: complete')
