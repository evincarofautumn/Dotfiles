# Source general bash settings.
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# Ensure all cloned repositories are up to date.
CLONED_REPOSITORIES=~/Projects/Cloned/*
for repository in $CLONED_REPOSITORIES; do
  echo "Pulling repository $repository..."
  (cd "$repository"; git pull)
done
