run:
    love .

setup:
    sudo apt install fennel love
    mkdir -p $HOME/.local/share/fennel-ls/docsets/
    curl -o $HOME/.local/share/fennel-ls/docsets/love2d.lua https://p.hagelb.org/docsets/love2d.lua
