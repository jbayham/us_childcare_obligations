find . -size +70M | sed 's|^\./||g' | cat >> .gitignore; awk '!NF || !seen[$0]++' .gitignore > ignore.tmp && mv -f ignore.tmp .gitignore