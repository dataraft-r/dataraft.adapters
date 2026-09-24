"""Fail if the REST catalog does not become ready."""
import json
import time
import urllib.request

for attempt in range(90):
    try:
        with urllib.request.urlopen("http://127.0.0.1:8181/v1/config", timeout=2) as response:
            config = json.load(response)
        assert isinstance(config, dict)
        break
    except Exception:
        if attempt == 89:
            raise
        time.sleep(1)
