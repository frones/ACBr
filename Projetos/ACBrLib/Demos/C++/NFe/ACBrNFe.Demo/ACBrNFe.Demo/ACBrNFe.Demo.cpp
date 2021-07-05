// ACBrNFe.Demo.cpp : Este arquivo contém a função 'main'. A execução do programa começa e termina ali.
//

#include <iostream>
#include <istream>
#include "../../ACBrNFe.h"

using namespace std;

int main()
{
	const auto nfe = std::make_shared<ACBrNFe>();
	cout << nfe->Nome() + " " + nfe->Versao() << std::endl;

	std::cout << "Press ENTER to continue...";
	std::cin.ignore();

	return 0;
}