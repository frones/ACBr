#include <iostream>
#include "ACBrNFe.h"

using namespace std;

int main() 
{
    std::shared_ptr<ACBrNFe> nfe = std::make_shared<ACBrNFe>(); 
    cout << nfe->nome() + " " + nfe->versao() << std::endl;
    return 0;
}