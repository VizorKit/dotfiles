# Install Maven, Gvim, and Git first
git clone https://github.com/georgewfraser/java-language-server $HOME/jls

cd $HOME/jdt
./scripts/link_windows.sh
mvn package -DskipTests

cd $HOME
iwr -useb https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim |`
    ni $HOME/vimfiles/autoload/plug.vim -Force