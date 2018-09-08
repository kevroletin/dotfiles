python
import sys
import os
sys.path.insert(0, os.environ['HOME'] + "/.config/gdb/python")
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers (None)
end
