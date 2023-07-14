import sys
from unittest.mock import Mock

sys.modules['ctypes.windll'] = Mock()
sys.modules['ctypes.WINFUNCTYPE'] = Mock()
sys.modules['pyauto'] = Mock()
sys.modules['keyhac'] = Mock()
sys.modules['keyhac_hook'] = Mock()
