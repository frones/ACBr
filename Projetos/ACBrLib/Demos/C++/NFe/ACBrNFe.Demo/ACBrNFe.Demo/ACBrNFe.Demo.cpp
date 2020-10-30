// ACBrNFe.Demo.cpp : Este arquivo contém a função 'main'. A execução do programa começa e termina ali.
//

#include <iostream>
#include <istream>
#include "../../ACBrNFe.h"

using namespace std;

int main()
{
	auto* const nfe = new ACBrNFe();
	cout << nfe->nome() + " " + nfe->versao() << std::endl;

	std::cout << "Press ENTER to continue...";
	std::cin.ignore();

	return 0;
}