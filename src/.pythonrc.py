import os
import readline
import atexit
import rlcompleter

history_file = os.path.expanduser('~/.python_history')
readline.read_history_file(history_file)
atexit.register(readline.write_history_file, history_file)

readline.parse_and_bind('tab: complete')
