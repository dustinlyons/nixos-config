#!/usr/bin/env python3
"""
Garage Camera Image Analysis

Analyzes snapshots from a garage camera to detect:
- Lighting state (on/off)
- Garage door state (open/closed)
- Vehicle presence
- Motion (compared to previous snapshot)

Regions must be calibrated for your camera. Create a config file at
the path specified by --config (default: /var/lib/hass/garage-analysis-config.json)
with content like:

{
  "brightness_threshold": 80,
  "motion_threshold": 15.0,
  "door_edge_threshold": 8.0,
  "car_edge_threshold": 5.0,
  "door_region": [100, 300, 200, 600],
  "parking_region": [300, 500, 100, 500],
  "max_snapshots_days": 7
}

Regions are [y1, y2, x1, x2] in pixels. To calibrate:
1. Open a snapshot in an image editor
2. Note the pixel coordinates of the garage door area -> door_region
3. Note the pixel coordinates of the parking spot -> parking_region
4. Adjust thresholds by reviewing analysis output against known states
"""

import argparse
import glob
import json
import os
import shutil
import sys
from datetime import datetime

import cv2
import numpy as np

DEFAULT_CONFIG = {
    "snapshot_dir": "/var/lib/hass/garage-snapshots",
    "results_file": "/var/lib/hass/garage-snapshots/latest_analysis.json",
    "previous_image_path": "/var/lib/hass/garage-snapshots/previous.jpg",
    "brightness_threshold": 80,
    "motion_threshold": 15.0,
    "door_edge_threshold": 8.0,
    "car_edge_threshold": 5.0,
    "door_region": None,
    "parking_region": None,
    "max_snapshots_days": 7,
}


def load_config(config_path):
    config = DEFAULT_CONFIG.copy()
    if config_path and os.path.exists(config_path):
        with open(config_path) as f:
            config.update(json.load(f))
    return config


def edge_density(gray_roi):
    """Compute the percentage of edge pixels in a grayscale region."""
    edges = cv2.Canny(gray_roi, 50, 150)
    return float(np.count_nonzero(edges)) / edges.size * 100


def analyze_brightness(gray, threshold):
    """Detect if garage lights are on based on mean luminance."""
    mean = float(np.mean(gray))
    return {
        "lights_on": mean > threshold,
        "mean_brightness": round(mean, 2),
    }


def analyze_door(gray, region, threshold):
    """Detect garage door state using edge density.

    When the door is closed, the region shows a smooth door surface (low edge
    density).  When open, you see through to the interior or exterior which
    typically has more visual complexity (higher edge density).
    """
    if region is None:
        return {"enabled": False}

    y1, y2, x1, x2 = region
    roi = gray[y1:y2, x1:x2]
    density = edge_density(roi)

    return {
        "enabled": True,
        "door_open": density > threshold,
        "edge_density": round(density, 2),
    }


def analyze_car(gray, region, threshold):
    """Detect vehicle presence using edge density in the parking region.

    An empty floor is relatively uniform (low edges).  A parked car introduces
    many edges from body panels, wheels, shadows, etc.
    """
    if region is None:
        return {"enabled": False}

    y1, y2, x1, x2 = region
    roi = gray[y1:y2, x1:x2]
    density = edge_density(roi)

    return {
        "enabled": True,
        "car_present": density > threshold,
        "edge_density": round(density, 2),
    }


def analyze_motion(gray, previous_path, threshold):
    """Detect motion by comparing with the previous snapshot."""
    if not os.path.exists(previous_path):
        return {
            "enabled": True,
            "motion_detected": False,
            "mean_diff": 0.0,
            "note": "no previous image",
        }

    prev = cv2.imread(previous_path, cv2.IMREAD_GRAYSCALE)
    if prev is None or prev.shape != gray.shape:
        return {
            "enabled": True,
            "motion_detected": False,
            "mean_diff": 0.0,
            "note": "previous image incompatible",
        }

    diff = cv2.absdiff(gray, prev)
    mean_diff = float(np.mean(diff))

    return {
        "enabled": True,
        "motion_detected": mean_diff > threshold,
        "mean_diff": round(mean_diff, 2),
    }


def cleanup_old_snapshots(snapshot_dir, max_days):
    """Remove archived snapshots older than *max_days*."""
    cutoff = datetime.now().timestamp() - (max_days * 86400)
    for path in glob.glob(os.path.join(snapshot_dir, "garage_*.jpg")):
        if os.path.getmtime(path) < cutoff:
            os.remove(path)


def analyze_image(image_path, config=None, config_path=None):
    """Run all analyses on *image_path* and return a results dict."""
    if config is None:
        config = load_config(config_path)

    img = cv2.imread(image_path)
    if img is None:
        raise ValueError(f"Could not load image: {image_path}")

    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

    results = {
        "timestamp": datetime.now().isoformat(),
        "image_path": image_path,
        "image_size": {"width": img.shape[1], "height": img.shape[0]},
        "brightness": analyze_brightness(gray, config["brightness_threshold"]),
        "door": analyze_door(gray, config.get("door_region"), config["door_edge_threshold"]),
        "car": analyze_car(gray, config.get("parking_region"), config["car_edge_threshold"]),
        "motion": analyze_motion(
            gray, config["previous_image_path"], config["motion_threshold"]
        ),
    }

    # Save current grayscale as the reference for next motion comparison
    cv2.imwrite(config["previous_image_path"], gray)

    # Archive the snapshot with a timestamp
    snapshot_dir = config["snapshot_dir"]
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_path = os.path.join(snapshot_dir, f"garage_{ts}.jpg")
    shutil.copy2(image_path, archive_path)

    # Persist results
    results_path = config["results_file"]
    os.makedirs(os.path.dirname(results_path), exist_ok=True)
    with open(results_path, "w") as f:
        json.dump(results, f, indent=2)

    # Housekeeping
    cleanup_old_snapshots(snapshot_dir, config["max_snapshots_days"])

    return results


def main():
    parser = argparse.ArgumentParser(description="Analyze a garage camera snapshot")
    parser.add_argument("image_path", help="Path to the JPEG snapshot")
    parser.add_argument(
        "--config",
        default="/var/lib/hass/garage-analysis-config.json",
        help="Path to JSON config file (default: %(default)s)",
    )
    args = parser.parse_args()

    try:
        results = analyze_image(args.image_path, config_path=args.config)
        print(json.dumps(results, indent=2))
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
