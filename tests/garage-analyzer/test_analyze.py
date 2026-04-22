#!/usr/bin/env python3
"""
Tests for the garage camera image analysis script.

Generates synthetic images to validate each detection mode:
- Brightness / lights detection
- Garage door state (edge density)
- Vehicle presence (edge density)
- Motion detection (frame-to-frame diff)
- Snapshot archival and cleanup
"""

import json
import os
import sys
import tempfile
import time
import unittest

import cv2
import numpy as np

# Allow importing the analysis script from the repo
SCRIPT_DIR = os.path.join(
    os.path.dirname(__file__), "..", "..", "modules", "nixos", "scripts"
)
sys.path.insert(0, os.path.abspath(SCRIPT_DIR))
import analyze_garage as ag


def make_config(tmpdir, **overrides):
    """Return a config dict pointing at *tmpdir* for all file paths."""
    cfg = {
        "snapshot_dir": tmpdir,
        "results_file": os.path.join(tmpdir, "latest_analysis.json"),
        "previous_image_path": os.path.join(tmpdir, "previous.jpg"),
        "brightness_threshold": 80,
        "motion_threshold": 15.0,
        "door_edge_threshold": 8.0,
        "car_edge_threshold": 5.0,
        "door_region": None,
        "parking_region": None,
        "max_snapshots_days": 7,
    }
    cfg.update(overrides)
    return cfg


def save_image(path, img):
    """Write a BGR numpy array as a JPEG."""
    cv2.imwrite(path, img)


def uniform_image(width, height, brightness):
    """Create a solid-gray BGR image at the given brightness (0-255)."""
    img = np.full((height, width, 3), brightness, dtype=np.uint8)
    return img


def noisy_image(width, height, seed=42):
    """Create a random-noise image (lots of edges)."""
    rng = np.random.RandomState(seed)
    return rng.randint(0, 256, (height, width, 3), dtype=np.uint8)


def image_with_region(width, height, region, region_brightness, bg_brightness=40):
    """Create an image that is *bg_brightness* everywhere except *region*."""
    img = uniform_image(width, height, bg_brightness)
    y1, y2, x1, x2 = region
    img[y1:y2, x1:x2] = region_brightness
    return img


def image_with_edges_in_region(width, height, region, bg_brightness=40):
    """Image with strong edges (grid lines) drawn inside *region*."""
    img = uniform_image(width, height, bg_brightness)
    y1, y2, x1, x2 = region
    # Draw a grid of white lines every 10 pixels inside the region
    for y in range(y1, y2, 10):
        cv2.line(img, (x1, y), (x2, y), (255, 255, 255), 1)
    for x in range(x1, x2, 10):
        cv2.line(img, (x, y1), (x, y2), (255, 255, 255), 1)
    return img


class TestBrightness(unittest.TestCase):
    def test_bright_image_lights_on(self):
        gray = np.full((100, 100), 200, dtype=np.uint8)
        result = ag.analyze_brightness(gray, threshold=80)
        self.assertTrue(result["lights_on"])
        self.assertGreater(result["mean_brightness"], 80)

    def test_dark_image_lights_off(self):
        gray = np.full((100, 100), 20, dtype=np.uint8)
        result = ag.analyze_brightness(gray, threshold=80)
        self.assertFalse(result["lights_on"])
        self.assertLess(result["mean_brightness"], 80)


class TestDoorDetection(unittest.TestCase):
    def test_disabled_when_no_region(self):
        gray = np.zeros((100, 100), dtype=np.uint8)
        result = ag.analyze_door(gray, region=None, threshold=8.0)
        self.assertFalse(result["enabled"])

    def test_smooth_region_door_closed(self):
        """A uniform region should have very low edge density -> door closed."""
        gray = np.full((400, 600), 128, dtype=np.uint8)
        region = [100, 300, 100, 500]
        result = ag.analyze_door(gray, region, threshold=8.0)
        self.assertTrue(result["enabled"])
        self.assertFalse(result["door_open"])
        self.assertLess(result["edge_density"], 1.0)

    def test_textured_region_door_open(self):
        """A noisy / textured region should have high edge density -> door open."""
        rng = np.random.RandomState(0)
        gray = rng.randint(0, 256, (400, 600), dtype=np.uint8)
        region = [100, 300, 100, 500]
        result = ag.analyze_door(gray, region, threshold=8.0)
        self.assertTrue(result["enabled"])
        self.assertTrue(result["door_open"])
        self.assertGreater(result["edge_density"], 8.0)


class TestCarDetection(unittest.TestCase):
    def test_disabled_when_no_region(self):
        gray = np.zeros((100, 100), dtype=np.uint8)
        result = ag.analyze_car(gray, region=None, threshold=5.0)
        self.assertFalse(result["enabled"])

    def test_empty_floor(self):
        gray = np.full((500, 600), 100, dtype=np.uint8)
        region = [300, 480, 100, 500]
        result = ag.analyze_car(gray, region, threshold=5.0)
        self.assertTrue(result["enabled"])
        self.assertFalse(result["car_present"])

    def test_car_present(self):
        rng = np.random.RandomState(1)
        gray = np.full((500, 600), 100, dtype=np.uint8)
        # Fill parking region with noise (simulating a car)
        region = [300, 480, 100, 500]
        gray[300:480, 100:500] = rng.randint(0, 256, (180, 400), dtype=np.uint8)
        result = ag.analyze_car(gray, region, threshold=5.0)
        self.assertTrue(result["enabled"])
        self.assertTrue(result["car_present"])


class TestMotionDetection(unittest.TestCase):
    def test_no_previous_image(self):
        gray = np.full((100, 100), 128, dtype=np.uint8)
        result = ag.analyze_motion(gray, "/nonexistent/path.jpg", threshold=15.0)
        self.assertFalse(result["motion_detected"])
        self.assertIn("no previous image", result.get("note", ""))

    def test_no_motion(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            prev_path = os.path.join(tmpdir, "prev.jpg")
            gray = np.full((100, 100), 128, dtype=np.uint8)
            cv2.imwrite(prev_path, gray)

            result = ag.analyze_motion(gray, prev_path, threshold=15.0)
            self.assertFalse(result["motion_detected"])
            self.assertAlmostEqual(result["mean_diff"], 0.0, places=1)

    def test_motion_detected(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            prev_path = os.path.join(tmpdir, "prev.jpg")
            prev_gray = np.full((100, 100), 50, dtype=np.uint8)
            cv2.imwrite(prev_path, prev_gray)

            curr_gray = np.full((100, 100), 200, dtype=np.uint8)
            result = ag.analyze_motion(curr_gray, prev_path, threshold=15.0)
            self.assertTrue(result["motion_detected"])
            self.assertGreater(result["mean_diff"], 100)


class TestIntegration(unittest.TestCase):
    """End-to-end test of analyze_image()."""

    def test_full_pipeline_no_regions(self):
        """Run analysis with regions disabled (default config)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            cfg = make_config(tmpdir)
            img_path = os.path.join(tmpdir, "latest.jpg")
            save_image(img_path, uniform_image(640, 480, brightness=150))

            results = ag.analyze_image(img_path, config=cfg)

            self.assertIn("brightness", results)
            self.assertTrue(results["brightness"]["lights_on"])
            self.assertFalse(results["door"]["enabled"])
            self.assertFalse(results["car"]["enabled"])
            self.assertFalse(results["motion"]["motion_detected"])

            # Results file should exist
            self.assertTrue(os.path.exists(cfg["results_file"]))
            with open(cfg["results_file"]) as f:
                saved = json.load(f)
            self.assertEqual(saved["brightness"]["lights_on"], True)

            # Previous image should be saved for next motion comparison
            self.assertTrue(os.path.exists(cfg["previous_image_path"]))

    def test_full_pipeline_with_regions(self):
        """Run analysis with door and parking regions configured."""
        with tempfile.TemporaryDirectory() as tmpdir:
            door_region = [50, 150, 100, 300]
            park_region = [200, 350, 100, 400]
            cfg = make_config(
                tmpdir,
                door_region=door_region,
                parking_region=park_region,
            )

            # Create image with edges in both regions
            img = image_with_edges_in_region(640, 480, door_region, bg_brightness=120)
            # Also add edges in parking region
            y1, y2, x1, x2 = park_region
            for y in range(y1, y2, 10):
                cv2.line(img, (x1, y), (x2, y), (255, 255, 255), 1)
            for x in range(x1, x2, 10):
                cv2.line(img, (x, y1), (x, y2), (255, 255, 255), 1)

            img_path = os.path.join(tmpdir, "latest.jpg")
            save_image(img_path, img)

            results = ag.analyze_image(img_path, config=cfg)

            self.assertTrue(results["door"]["enabled"])
            self.assertTrue(results["door"]["door_open"])
            self.assertTrue(results["car"]["enabled"])
            self.assertTrue(results["car"]["car_present"])

    def test_motion_across_two_runs(self):
        """Two sequential analyses — first has no motion, second detects change."""
        with tempfile.TemporaryDirectory() as tmpdir:
            cfg = make_config(tmpdir)

            # First run — dark image
            img1_path = os.path.join(tmpdir, "latest.jpg")
            save_image(img1_path, uniform_image(640, 480, brightness=30))
            r1 = ag.analyze_image(img1_path, config=cfg)
            self.assertFalse(r1["motion"]["motion_detected"])

            # Second run — bright image (big change)
            save_image(img1_path, uniform_image(640, 480, brightness=220))
            r2 = ag.analyze_image(img1_path, config=cfg)
            self.assertTrue(r2["motion"]["motion_detected"])

    def test_snapshot_archival(self):
        """analyze_image should archive the snapshot with a timestamp."""
        with tempfile.TemporaryDirectory() as tmpdir:
            cfg = make_config(tmpdir)
            img_path = os.path.join(tmpdir, "latest.jpg")
            save_image(img_path, uniform_image(640, 480, 100))

            ag.analyze_image(img_path, config=cfg)

            archives = [f for f in os.listdir(tmpdir) if f.startswith("garage_")]
            self.assertEqual(len(archives), 1)
            self.assertTrue(archives[0].endswith(".jpg"))


class TestCleanup(unittest.TestCase):
    def test_old_snapshots_removed(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            old_file = os.path.join(tmpdir, "garage_20200101_000000.jpg")
            new_file = os.path.join(tmpdir, "garage_20991231_235959.jpg")
            for f in [old_file, new_file]:
                save_image(f, uniform_image(10, 10, 100))

            # Backdate the old file
            old_ts = time.time() - 30 * 86400  # 30 days ago
            os.utime(old_file, (old_ts, old_ts))

            ag.cleanup_old_snapshots(tmpdir, max_days=7)

            self.assertFalse(os.path.exists(old_file))
            self.assertTrue(os.path.exists(new_file))


if __name__ == "__main__":
    unittest.main()
