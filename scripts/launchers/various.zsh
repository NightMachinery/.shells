tmux new -d -s serve-dl 'cd ~/Downloads && http-server'
tmux new -d -s splash 'docker run -it -p 8050:8050 scrapinghub/splash'
tmux new -d -s julia 'zsh -c "python3 ~/code/betterborg/stdborg.py"'
