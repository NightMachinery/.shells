#!/usr/bin/env python3
#: [[https://jupyter-server.readthedocs.io/en/latest/other/full-config.html][Config file and command line options — Jupyter Server documentation]]
##
import os
import json
import argparse
import logging


def main():
    parser = argparse.ArgumentParser(
        description="Update Jupyter config for data rate limits."
    )
    parser.add_argument(
        "-r",
        "--iopub-rate",
        type=int,
        # default=100000,
        default=10000,
        help="Set iopub_data_rate_limit",
    )
    parser.add_argument(
        "-w",
        "--limit-window",
        type=int,
        default=3,
        help="Set rate_limit_window",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose logging"
    )
    args = parser.parse_args()

    # Set up logging
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO)
    logger = logging.getLogger("JupyterConfigUpdater")

    # Locate Jupyter config directory and file
    jupyter_config_dir = os.path.join(os.path.expanduser("~"), ".jupyter")
    config_file = os.path.join(jupyter_config_dir, "jupyter_config.json")

    logger.debug(f"Jupyter config directory: {jupyter_config_dir}")
    logger.debug(f"Config file path: {config_file}")

    # Create config directory if it doesn't exist
    if not os.path.isdir(jupyter_config_dir):
        logger.info(f"Creating Jupyter config directory at {jupyter_config_dir}")
        os.makedirs(jupyter_config_dir)

    # Load existing config or create a new one
    if os.path.isfile(config_file):
        logger.info(f"Loading existing config file: {config_file}")
        with open(config_file, "r") as f:
            try:
                config = json.load(f)
            except json.JSONDecodeError as e:
                logger.error(f"Error decoding JSON: {e}")
                config = {}
    else:
        logger.info(f"No existing config file found. Creating new one at {config_file}")
        config = {}

    # Update or add rate limit settings
    config["iopub_data_rate_limit"] = args.iopub_rate
    config["rate_limit_window"] = args.limit_window

    logger.debug(f"Set iopub_data_rate_limit = {args.iopub_rate}")
    logger.debug(f"Set rate_limit_window = {args.limit_window}")

    # Write updated config back to file
    with open(config_file, "w") as f:
        json.dump(config, f, indent=2)
        logger.info(f"Updated config file saved to {config_file}")


if __name__ == "__main__":
    main()
