#!/bin/bash

# Really simple shitty script to git-pull and backup before every run.
# To be run from a local clone of the repo. Compatible with having a "stable" branch later. (just switch)
# This should *not* be used in actual production at some point.

set -o errexit

cd $(dirname "$0")

# Configuration
SOURCE_FILE="srtd.json"
BACKUP_DIR="$HOME/data/srtd_backups"

# Currently unused:
MAX_BACKUPS=50  # Maximum number of backups to keep

# Ensure the backup directory exists
# mkdir -p "$BACKUP_DIR"

# Create a timestamped backup
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_FILE="$BACKUP_DIR/srtd_$TIMESTAMP.json"

# Copy the file
if [ -f "$SOURCE_FILE" ]; then
  cp "$SOURCE_FILE" "$BACKUP_FILE"
fi

# Remove old backups if exceeding the limit
# Currently not used.
# NUM_BACKUPS=$(ls -1 "$BACKUP_DIR" | wc -l)
# if [ "$NUM_BACKUPS" -gt "$MAX_BACKUPS" ]; then
#   # Remove the oldest file(s)
#   ls -1t "$BACKUP_DIR" | tail -n +$(($MAX_BACKUPS + 1)) | xargs -I {} rm -- "$BACKUP_DIR/{}"
# fi

git pull

exec cabal run srtd -- "$@"

