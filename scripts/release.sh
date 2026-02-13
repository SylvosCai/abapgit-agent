#!/bin/bash

# Release script for abapgit-agent
# Usage: ./scripts/release.sh

set -e

echo "=== abapgit-agent Release Script ==="
echo

# Get current version
CURRENT_VERSION=$(node -p "require('./package.json').version")
echo "Current version: $CURRENT_VERSION"
echo

# Ask for version bump type
echo "Select version bump type:"
echo "  1) patch (${CURRENT_VERSION} -> ${CURRENT_VERSION%.*}.$((${CURRENT_VERSION##*.}+1)))"
echo "  2) minor (${CURRENT_VERSION} -> $(((${CURRENT_VERSION%%.*})+1)).0)"
echo "  3) major ($(((${CURRENT_VERSION%%.*})+1)).0.0)"
echo "  4) custom"
read -p "Enter choice (1-4): " CHOICE

case $CHOICE in
  1) npm version patch ;;
  2) npm version minor ;;
  3) npm version major ;;
  4)
    read -p "Enter new version: " NEW_VERSION
    npm version $NEW_VERSION
    ;;
  *)
    echo "Invalid choice"
    exit 1
    ;;
esac

# Get the new version tag
NEW_TAG="v$(node -p "require('./package.json').version')"
echo
echo "Created tag: $NEW_TAG"
echo

# Push to trigger workflow
read -p "Push to GitHub to trigger release? (y/n): " CONFIRM
if [ "$CONFIRM" = "y" ]; then
  echo "Pushing to GitHub..."
  git push public --follow-tags
  echo
  echo "=== Release triggered! ==="
  echo "Tag: $NEW_TAG"
  echo
  echo "Next steps:"
  echo "1. Go to https://github.com/SylvosCai/abapgit-agent/releases"
  echo "2. Edit the release to add 'What's New' information"
  echo "3. Publish the release"
else
  echo "Aborted. Tag $NEW_TAG created locally."
  echo "To push manually: git push public --follow-tags"
fi
