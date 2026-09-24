"""Create the disposable object bucket once MinIO accepts requests."""
import os
import time

from minio import Minio

client = Minio(
    "127.0.0.1:9000", os.environ["AWS_ACCESS_KEY_ID"],
    os.environ["AWS_SECRET_ACCESS_KEY"], secure=False,
)
for attempt in range(60):
    try:
        if not client.bucket_exists("warehouse"):
            client.make_bucket("warehouse")
        break
    except Exception:
        if attempt == 59:
            raise
        time.sleep(1)
