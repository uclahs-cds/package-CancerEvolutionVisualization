#!/usr/bin/env python3
"""Build a before/after review page for the ManuscriptFigures renders.

Reads $OUTROOT/{baseline,current}/figures/*.png and writes $OUTROOT/REVIEW.html
plus downscaled side-by-side comparisons. Sorted by mean pixel difference so the
figures that actually moved are at the top.
"""
import os
import glob
import warnings

warnings.filterwarnings('ignore')
from PIL import Image, ImageChops, ImageStat, ImageDraw, ImageFont

# Manuscript figures are rendered at publication size (1D is ~221 megapixels).
Image.MAX_IMAGE_PIXELS = None

OUTROOT = os.environ.get('OUTROOT', '.')
BASELINE = os.environ.get('BASELINE', 'origin/main')
MAX_W, MAX_H = 900, 700


def font(size):
    for path in ('/System/Library/Fonts/Supplemental/Arial Bold.ttf',
                 '/System/Library/Fonts/Helvetica.ttc'):
        if os.path.exists(path):
            try:
                return ImageFont.truetype(path, size)
            except Exception:
                pass
    return ImageFont.load_default()


def main():
    compare_dir = os.path.join(OUTROOT, 'compare')
    os.makedirs(compare_dir, exist_ok=True)
    rows = []

    for cur_path in sorted(glob.glob(os.path.join(OUTROOT, 'current', 'figures', '*.png'))):
        name = os.path.basename(cur_path)
        base_path = os.path.join(OUTROOT, 'baseline', 'figures', name)
        if not os.path.exists(base_path):
            rows.append((name, float('inf'), None, 'NEW - no baseline'))
            continue

        before = Image.open(base_path).convert('RGB')
        after = Image.open(cur_path).convert('RGB')
        if before.size != after.size:
            rows.append((name, float('inf'), None,
                         f'SIZE CHANGED {before.size} -> {after.size}'))
            continue

        mean = ImageStat.Stat(ImageChops.difference(before, after).convert('L')).mean[0]
        scale = min(MAX_W / before.width, MAX_H / before.height, 1.0)
        size = (max(1, int(before.width * scale)), max(1, int(before.height * scale)))
        left = before.resize(size, Image.LANCZOS)
        right = after.resize(size, Image.LANCZOS)

        header, gap = 44, 18
        canvas = Image.new('RGB', (size[0] * 2 + gap, size[1] + header), 'white')
        canvas.paste(left, (0, header))
        canvas.paste(right, (size[0] + gap, header))
        draw = ImageDraw.Draw(canvas)
        label = font(22)
        draw.text((4, 10), f'BEFORE ({BASELINE})', fill=(180, 0, 0), font=label)
        draw.text((size[0] + gap + 4, 10), 'AFTER (working tree)', fill=(0, 120, 0), font=label)
        draw.line([(size[0] + gap // 2, header), (size[0] + gap // 2, size[1] + header)],
                  fill=(200, 200, 200), width=2)
        canvas.save(os.path.join(compare_dir, name))
        rows.append((name, mean, os.path.join('compare', name),
                     'identical' if mean == 0 else f'mean pixel diff {mean:.4f}'))

    rows.sort(key=lambda r: -r[1])

    with open(os.path.join(OUTROOT, 'REVIEW.html'), 'w') as out:
        out.write("<html><head><meta charset='utf-8'>"
                  "<title>ManuscriptFigures review</title><style>"
                  "body{font-family:-apple-system,sans-serif;margin:24px;background:#fafafa}"
                  "h2{margin:28px 0 4px;font-size:16px}"
                  "img{max-width:100%;border:1px solid #ddd;background:#fff}"
                  ".m{color:#666;font-size:13px;margin-bottom:6px}</style></head><body>")
        out.write(f'<h1>ManuscriptFigures.Rmd &mdash; {BASELINE} vs working tree</h1>')
        out.write('<p class=m>Sorted by mean pixel difference, largest first. '
                  'Downscaled for review; full-resolution PNGs are under '
                  '<code>baseline/figures/</code> and <code>current/figures/</code>.</p>')
        for name, _, img, note in rows:
            out.write(f'<h2>{name}</h2><div class=m>{note}</div>')
            if img:
                out.write(f"<img src='{img}'>")
    print(f'  {len(rows)} figures compared')


if __name__ == '__main__':
    main()
