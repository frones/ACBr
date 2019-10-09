#include <iostream>
#include "ACBrNFe.h"

using namespace std;

int main() 
{
    std::shared_ptr<ACBrNFe> nfe = std::make_shared<ACBrNFe>(); 
    cout << nfe->Nome() + " " + nfe->Versao() << std::endl;
    return 0;
}