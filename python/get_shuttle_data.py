import pandas as pd
import requests
import os
from urllib.parse import urlparse

# set file path
df = pd.read_csv("your_file.csv")

urls = df["download_link"].dropna()

outdir = "downloads"
os.makedirs(outdir, exist_ok=True)

for url in urls:
    try:
        clean_path = urlparse(url).path
        filename = os.path.basename(clean_path)
        filepath = os.path.join(outdir, filename)

        r = requests.get(url, stream=True)
        r.raise_for_status()

        with open(filepath, "wb") as f:
            for chunk in r.iter_content(chunk_size=8192):
                f.write(chunk)

        print("Downloaded:", filename)

    except Exception as e:
        print("Failed:", url, "| error:", e)
