#!/bin/bash
# Clean and tidy MUMPS project structure
# Removes build artifacts, cache files, and temporary files

echo "=== Cleaning MUMPS Project ==="

# Count before cleaning
echo ""
echo "Before cleanup:"
echo "  Object files (.o): $(find . -name "*.o" | wc -l)"
echo "  Module files (.mod): $(find . -name "*.mod" | wc -l)"
echo "  Dependency files (.d): $(find . -name "*.d" | wc -l)"
echo "  __pycache__ dirs: $(find . -path "./.venv" -prune -o -type d -name "__pycache__" -print | wc -l)"
echo "  .pytest_cache dirs: $(find . -path "./.venv" -prune -o -type d -name ".pytest_cache" -print | wc -l)"
echo "  .egg-info dirs: $(find . -path "./.venv" -prune -o -type d -name "*.egg-info" -print | wc -l)"

echo ""
echo "Cleaning..."

# Remove object files
find . -type f -name "*.o" -delete
find . -type f -name "*.mod" -delete
find . -type f -name "*.d" -delete

# Remove Python cache directories (excluding .venv)
find . -path "./.venv" -prune -o -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null
find . -path "./.venv" -prune -o -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null
find . -path "./.venv" -prune -o -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null

# Remove build cache
rm -rf .build-cache

# Remove backup files
find . -type f -name "*~" -delete
find . -type f -name "*.backup" -delete
find . -type f -name "*.bak" -delete

# Clean MATLAB/SCILAB generated files
find MATLAB -type f -name "*.mex*" -delete 2>/dev/null
find SCILAB -type f -name "*.so" -delete 2>/dev/null

echo ""
echo "After cleanup:"
echo "  Object files (.o): $(find . -name "*.o" | wc -l)"
echo "  Module files (.mod): $(find . -name "*.mod" | wc -l)"
echo "  Dependency files (.d): $(find . -name "*.d" | wc -l)"
echo "  __pycache__ dirs: $(find . -path "./.venv" -prune -o -type d -name "__pycache__" -print | wc -l)"
echo "  .pytest_cache dirs: $(find . -path "./.venv" -prune -o -type d -name ".pytest_cache" -print | wc -l)"
echo "  .egg-info dirs: $(find . -path "./.venv" -prune -o -type d -name "*.egg-info" -print | wc -l)"

echo ""
echo "✓ Cleanup complete!"
