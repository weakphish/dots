# Update packages
sudo pacman -Syyu

# Get yay
sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay-bin.git && cd yay-bin && makepkg -si

# Install packages me like
sudo pacman -S hyprpaper waybar neovim ghostty stow fzf fd ripgrep lazygit lazydocker bat lsd git-delta

# Link dotfiles w/ stow
stow config --dotfiles
