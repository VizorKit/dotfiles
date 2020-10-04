unameVAR="$(uname -s)"
if [[ "$unameVAR" == *"Linux"* ]]; then
    sudo add-apt-repository ppa:kelleyk/emacs
    sudo apt update
    sudo apt install emacs27
    read -p "Will you use C? y or n: " is_c
fi
if [[ "$unameVAR" == *"MINGW"* ]]; then
    read -p "Make sure you install emacs separately" emacs
fi

read -p "Will you use Java? y or n: " is_java
read -p "Will you use CSharp? y or n: " is_csharp
read -p "Will you use Angular? y or n: " is_ng

if [[ "$is_java" == "y" ]]; then
    echo 'Ensure you install JDK and MVN'
    echo 'export IS_JAVA=1' >> ~/.bash_profile
fi    

if [[ "$is_c" == "y" ]]; then
    sudo apt install build-essential
    sudo apt install clangd    
    echo 'export IS_C=1' >> ~/.bash_profile
fi

if [[ "$is_csharp" == "y" ]]; then
    echo 'Ensure you install .NetCore'
    echo 'export IS_CSHARP=1' >> ~/.bash_profile
fi

if [[ "$is_ng" == "y" ]]; then
    echo 'Ensure you install node and the angular language service'
    echo 'export IS_NG=1' >> ~/.bash_profile
fi
