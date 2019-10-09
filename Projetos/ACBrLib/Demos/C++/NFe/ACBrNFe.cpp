#include <cstdint>
#include <string>
#include "ACBrNFe.h"
#include <algorithm>
#include <functional>
#if defined(ISWINDOWS)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

ACBrNFe::ACBrNFe(const std::string eArqConfig, const std::string e_chave_crypt) {
#if defined(ISWINDOWS)
#if defined(ENVIRONMENT32)
	nHandler = LoadLibraryW(L"ACBrNFe32.dll");
#else
	nHandler = LoadLibraryW(L"ACBrNFe64.dll");
#endif
#else
#if defined(ENVIRONMENT32)
	nHandler = dlopen("libacbrnfe32.so");
#else
	nHandler = dlopen("libacbrnfe64.so");
#endif
#endif

	NFE_Inicializar method;

#if defined(ISWINDOWS)
	method = (NFE_Inicializar)GetProcAddress(nHandler, "NFE_Inicializar");
#else
	method = (NFE_Inicializar)dlsym(nHandler, "NFE_Inicializar");
#endif

	const int ret = method(eArqConfig.c_str(), e_chave_crypt.c_str());
	check_result(ret);
}

ACBrNFe::~ACBrNFe() {
	NFE_Finalizar method;

#if defined(ISWINDOWS)
	method = (NFE_Finalizar)GetProcAddress(nHandler, "NFE_Finalizar");
#else
	method = (NFE_Finalizar)dlsym(nHandler, "NFE_Finalizar");
#endif

	const int ret = method();
	check_result(ret);

#if defined(ISWINDOWS)
	FreeLibrary(nHandler);
#else
	dlclose(nHandler);
#endif
}

std::string ACBrNFe::nome() {
	NFE_Nome method;

#if defined(ISWINDOWS)
	method = (NFE_Nome)GetProcAddress(nHandler, "NFE_Nome");
#else
	method = (NFE_Nome)dlsym(nHandler, "NFE_Nome");
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	method(buffer.c_str(), &bufferLen);
	return process_result(buffer, bufferLen);
}

std::string ACBrNFe::versao() {
	NFE_Versao method;

#if defined(ISWINDOWS)
	method = (NFE_Versao)GetProcAddress(nHandler, "NFE_Versao");
#else
	method = (NFE_Versao)dlsym(nHandler, "NFE_Versao");
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	method(buffer.c_str(), &bufferLen);
	return process_result(buffer, bufferLen);
}

std::string ACBrNFe::trim(std::string& buffer) const
{
	buffer.erase(buffer.begin(), std::find_if(buffer.begin(), buffer.end(),
		std::not1(std::ptr_fun<int, int>(std::isspace))));

	buffer.erase(std::find_if(buffer.rbegin(), buffer.rend(),
		std::not1(std::ptr_fun<int, int>(std::isspace))).base(), buffer.end());

	return buffer;
}

void ACBrNFe::check_result(int ret) const
{
	if (ret >= 0) return;

	std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	NFE_UltimoRetorno method;

#if defined(ISWINDOWS)
	method = (NFE_UltimoRetorno)GetProcAddress(nHandler, "NFE_UltimoRetorno");
#else
	method = (NFE_UltimoRetorno)dlsym(nHandler, "NFE_method");
#endif

	method(buffer.c_str(), &bufferLen);

	if (bufferLen <= BUFFER_LEN) throw(std::exception(trim(buffer).c_str()));

	buffer.clear();
	buffer.resize(bufferLen, ' ');
	method(buffer.c_str(), &bufferLen);
	throw(std::exception(trim(buffer).c_str()));
}

std::string ACBrNFe::process_result(std::string buffer, int buffer_len) {
	if (buffer_len > BUFFER_LEN) {
		NFE_UltimoRetorno method;

#if defined(ISWINDOWS)
		method = (NFE_UltimoRetorno)GetProcAddress(nHandler, "NFE_UltimoRetorno");
#else
		method = (NFE_UltimoRetorno)dlsym(nHandler, "NFE_UltimoRetorno");
#endif

		buffer.clear();
		buffer.resize(buffer_len, ' ');
		method(buffer.c_str(), &buffer_len);
	}

	return trim(buffer);
}