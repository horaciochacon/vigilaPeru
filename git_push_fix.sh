#!/bin/bash
# Script to stage, commit and push the DESCRIPTION fix

echo "Current directory:"
pwd

echo -e "\nGit status before:"
git status

echo -e "\nAdding DESCRIPTION file..."
git add DESCRIPTION

echo -e "\nCommitting changes..."
git commit -m "fix: Remove ckanr dependency and update package URLs"

echo -e "\nPushing to GitHub..."
git push

echo -e "\nDone! Check GitHub Actions for build status."