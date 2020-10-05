sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs27
git config credential.helper store
read -p "git email" var_email
read -p "git username" var_username
git config --global user.email var_email
git config --global user.name var_username

read -p "Will you use C? y or n: " is_c

if [[ "$is_c" == "y" ]]; then
    sudo apt install build-essential
    sudo apt install clangd    
fi
