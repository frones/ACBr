#include <cstdint>
#include <string>
#include <algorithm>
#include <functional>
#include <stdexcept>
#include "ACBrNFe.h"

#if defined(ISWINDOWS)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

ACBrNFe::ACBrNFe(std::string eArqConfig, std::string eChaveCrypt) {
#if defined(ISWINDOWS)
#if defined(ENVIRONMENT32)
	nHandler = LoadLibraryW(L"ACBrNFe32.dll");
#else
	nHandler = LoadLibraryW(L"ACBrNFe64.dll");
#endif
#else
#if defined(ENVIRONMENT32)
	std::string path = "/usr/lib/libacbrnfe32.so";
	nHandler = dlopen(path.c_str(), RTLD_LAZY);
#else
	std::string path = "/usr/lib/libacbrnfe64.so";
	nHandler = dlopen(path.c_str(), RTLD_LAZY);
#endif
#endif

	NFE_Inicializar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Inicializar>(GetProcAddress(nHandler, "NFE_Inicializar"));
#else
	method = reinterpret_cast<NFE_Inicializar>(dlsym(nHandler, "NFE_Inicializar"));
#endif

	const int ret = method(&this->libHandler, eArqConfig.c_str(), eChaveCrypt.c_str());
	CheckResult(ret);
}

ACBrNFe::~ACBrNFe() {
	NFE_Finalizar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Finalizar>(GetProcAddress(nHandler, "NFE_Finalizar"));
#else
	method = reinterpret_cast<NFE_Finalizar>(dlsym(nHandler, "NFE_Finalizar"));
#endif

	method(this->libHandler);

#if defined(ISWINDOWS)
	FreeLibrary(nHandler);
#else
	dlclose(nHandler);
#endif
}

std::string ACBrNFe::Nome() const
{
	NFE_Nome method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Nome>(GetProcAddress(nHandler, "NFE_Nome"));
#else
	method = reinterpret_cast<NFE_Nome>(dlsym(nHandler, "NFE_Nome"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	method(this->libHandler, buffer.c_str(), &bufferLen);
	return ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Versao() const
{
	NFE_Versao method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Versao>(GetProcAddress(nHandler, "NFE_Versao"));
#else
	method = reinterpret_cast<NFE_Versao>(dlsym(nHandler, "NFE_Versao"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	method(this->libHandler, buffer.c_str(), &bufferLen);
	return ProcessResult(buffer, bufferLen);
}

void ACBrNFe::ConfigLer(std::string eArqConfig) const
{
	NFE_ConfigLer method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigLer>(GetProcAddress(nHandler, "NFE_ConfigLer"));
#else
	method = reinterpret_cast<NFE_ConfigLer>(dlsym(nHandler, "NFE_ConfigLer"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

void ACBrNFe::ConfigGravar(std::string eArqConfig) const
{
	NFE_ConfigGravar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigGravar>(GetProcAddress(nHandler, "NFE_ConfigGravar"));
#else
	method = reinterpret_cast<NFE_ConfigGravar>(dlsym(nHandler, "NFE_ConfigGravar"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

void ACBrNFe::ConfigGravarValor(std::string eSessao, std::string eChave, std::string sValor) const
{
	NFE_ConfigGravarValor method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigGravarValor>(GetProcAddress(nHandler, "NFE_ConfigGravarValor"));
#else
	method = reinterpret_cast<NFE_ConfigGravarValor>(dlsym(nHandler, "NFE_ConfigGravarValor"));
#endif

	const int ret = method(this->libHandler, eSessao.c_str(), eChave.c_str(), sValor.c_str());
	CheckResult(ret);
}

std::string ACBrNFe::ConfigLerValor(std::string eSessao, std::string eChave) const
{
	NFE_ConfigLerValor method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigLerValor>(GetProcAddress(nHandler, "NFE_ConfigLerValor"));
#else
	method = reinterpret_cast<NFE_ConfigLerValor>(dlsym(nHandler, "NFE_ConfigLerValor"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, eSessao.c_str(), eChave.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrNFe::ConfigImportar(std::string eArqConfig) const
{
	NFE_ConfigImportar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigImportar>(GetProcAddress(nHandler, "NFE_ConfigImportar"));
#else
	method = reinterpret_cast<NFE_ConfigImportar>(dlsym(nHandler, "NFE_ConfigImportar"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

std::string ACBrNFe::ConfigExportar() const
{
	NFE_ConfigExportar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConfigExportar>(GetProcAddress(nHandler, "NFE_ConfigExportar"));
#else
	method = reinterpret_cast<NFE_ConfigExportar>(dlsym(nHandler, "NFE_ConfigExportar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrNFe::CarregarINI(std::string eArquivoOuINI) const
{
	NFE_CarregarINI method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_CarregarINI>(GetProcAddress(nHandler, "NFE_CarregarINI"));
#else
	method = reinterpret_cast<NFE_CarregarINI>(dlsym(nHandler, "NFE_CarregarINI"));
#endif

	const int ret = method(this->libHandler, eArquivoOuINI.c_str());
	CheckResult(ret);
}

void ACBrNFe::CarregarXML(std::string eArquivoOuXML) const
{
	NFE_CarregarXML method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_CarregarXML>(GetProcAddress(nHandler, "NFE_CarregarXML"));
#else
	method = reinterpret_cast<NFE_CarregarXML>(dlsym(nHandler, "NFE_CarregarXML"));
#endif

	const int ret = method(this->libHandler, eArquivoOuXML.c_str());
	CheckResult(ret);
}

std::string ACBrNFe::ObterXml(std::int32_t AIndex) const
{
	NFE_ObterXml method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ObterXml>(GetProcAddress(nHandler, "NFE_ObterXml"));
#else
	method = reinterpret_cast<NFE_ObterXml>(dlsym(nHandler, "NFE_ObterXml"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	std::int32_t bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, AIndex, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrNFe::GravarXml(std::int32_t AIndex, std::string eNomeArquivo, std::string ePathArquivo) const
{
	NFE_GravarXml method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_GravarXml>(GetProcAddress(nHandler, "NFE_GravarXml"));
#else
	method = reinterpret_cast<NFE_GravarXml>(dlsym(nHandler, "NFE_GravarXml"));
#endif

	const int ret = method(this->libHandler, AIndex, eNomeArquivo.c_str(), ePathArquivo.c_str());
	CheckResult(ret);
}

void ACBrNFe::CarregarEventoINI(std::string eArquivoOuINI) const
{
	NFE_CarregarEventoINI method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_CarregarEventoINI>(GetProcAddress(nHandler, "NFE_CarregarEventoINI"));
#else
	method = reinterpret_cast<NFE_CarregarEventoINI>(dlsym(nHandler, "NFE_CarregarEventoINI"));
#endif

	const int ret = method(this->libHandler, eArquivoOuINI.c_str());
	CheckResult(ret);
}

void ACBrNFe::CarregarEventoXML(std::string eArquivoOuXML) const
{
	NFE_CarregarEventoXML method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_CarregarEventoXML>(GetProcAddress(nHandler, "NFE_CarregarEventoXML"));
#else
	method = reinterpret_cast<NFE_CarregarEventoXML>(dlsym(nHandler, "NFE_CarregarEventoXML"));
#endif

	const int ret = method(this->libHandler, eArquivoOuXML.c_str());
	CheckResult(ret);
}

void ACBrNFe::LimparLista() const
{
	NFE_LimparLista method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_LimparLista>(GetProcAddress(nHandler, "NFE_LimparLista"));
#else
	method = reinterpret_cast<NFE_LimparLista>(dlsym(nHandler, "NFE_LimparLista"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrNFe::LimparListaEventos() const
{
	NFE_LimparListaEventos method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_LimparListaEventos>(GetProcAddress(nHandler, "NFE_LimparListaEventos"));
#else
	method = reinterpret_cast<NFE_LimparListaEventos>(dlsym(nHandler, "NFE_LimparListaEventos"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrNFe::Assinar() const
{
	NFE_Assinar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Assinar>(GetProcAddress(nHandler, "NFE_Assinar"));
#else
	method = reinterpret_cast<NFE_Assinar>(dlsym(nHandler, "NFE_Assinar"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrNFe::Validar() const
{
	NFE_Validar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Validar>(GetProcAddress(nHandler, "NFE_Validar"));
#else
	method = reinterpret_cast<NFE_Validar>(dlsym(nHandler, "NFE_Validar"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

std::string ACBrNFe::ValidarRegrasdeNegocios() const
{
	NFE_ValidarRegrasdeNegocios method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ValidarRegrasdeNegocios>(GetProcAddress(nHandler, "NFE_ValidarRegrasdeNegocios"));
#else
	method = reinterpret_cast<NFE_ValidarRegrasdeNegocios>(dlsym(nHandler, "NFE_ValidarRegrasdeNegocios"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::VerificarAssinatura() const
{
	NFE_VerificarAssinatura method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_VerificarAssinatura>(GetProcAddress(nHandler, "NFE_VerificarAssinatura"));
#else
	method = reinterpret_cast<NFE_VerificarAssinatura>(dlsym(nHandler, "NFE_VerificarAssinatura"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::GerarChave(std::int32_t ACodigoUF, std::int32_t ACodigoNumerico, std::int32_t AModelo,
	std::int32_t ASerie, std::int32_t ANumero, std::int32_t ATpEmi,
	std::string AEmissao, std::string ACNPJCPF) const
{
	NFE_GerarChave method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_GerarChave>(GetProcAddress(nHandler, "NFE_GerarChave"));
#else
	method = reinterpret_cast<NFE_GerarChave>(dlsym(nHandler, "NFE_GerarChave"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero,
		ATpEmi, AEmissao.c_str(), ACNPJCPF.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::vector<std::string> ACBrNFe::ObterCertificados() const
{
	NFE_ObterCertificados method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ObterCertificados>(GetProcAddress(nHandler, "NFE_ObterCertificados"));
#else
	method = reinterpret_cast<NFE_ObterCertificados>(dlsym(nHandler, "NFE_ObterCertificados"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return Split(this->ProcessResult(buffer, bufferLen), "|");
}

std::string ACBrNFe::GetPath(std::int32_t ATipo) const
{
	NFE_GetPath method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_GetPath>(GetProcAddress(nHandler, "NFE_GetPath"));
#else
	method = reinterpret_cast<NFE_GetPath>(dlsym(nHandler, "NFE_GetPath"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ATipo, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::GetPathEvento(std::string ACodEvento) const
{
	NFE_GetPathEvento method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_GetPathEvento>(GetProcAddress(nHandler, "NFE_GetPathEvento"));
#else
	method = reinterpret_cast<NFE_GetPathEvento>(dlsym(nHandler, "NFE_GetPathEvento"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ACodEvento.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::StatusServico() const
{
	NFE_StatusServico method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_StatusServico>(GetProcAddress(nHandler, "NFE_StatusServico"));
#else
	method = reinterpret_cast<NFE_StatusServico>(dlsym(nHandler, "NFE_StatusServico"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Consultar(std::string eChaveOuNFe) const
{
	NFE_Consultar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Consultar>(GetProcAddress(nHandler, "NFE_Consultar"));
#else
	method = reinterpret_cast<NFE_Consultar>(dlsym(nHandler, "NFE_Consultar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, eChaveOuNFe.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Inutilizar(std::string ACNPJ, std::string AJustificativa, std::int32_t Ano, std::int32_t Modelo,
	std::int32_t Serie, std::int32_t NumeroInicial, std::int32_t NumeroFinal) const
{
	NFE_Inutilizar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Inutilizar>(GetProcAddress(nHandler, "NFE_Inutilizar"));
#else
	method = reinterpret_cast<NFE_Inutilizar>(dlsym(nHandler, "NFE_Inutilizar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ACNPJ.c_str(), AJustificativa.c_str(), Ano, Modelo, Serie,
		NumeroInicial, NumeroFinal, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Enviar(std::int32_t ALote, bool Imprimir, bool Sincrono, bool Zipado) const
{
	NFE_Enviar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Enviar>(GetProcAddress(nHandler, "NFE_Enviar"));
#else
	method = reinterpret_cast<NFE_Enviar>(dlsym(nHandler, "NFE_Enviar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ALote, Imprimir, Sincrono, Zipado, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::ConsultarRecibo(std::string ARecibo) const
{
	NFE_ConsultarRecibo method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConsultarRecibo>(GetProcAddress(nHandler, "NFE_ConsultarRecibo"));
#else
	method = reinterpret_cast<NFE_ConsultarRecibo>(dlsym(nHandler, "NFE_ConsultarRecibo"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, ARecibo.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::Cancelar(std::string eChave, std::string eJustificativa, std::string eCNPJ, std::int32_t ALote) const
{
	NFE_Cancelar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Cancelar>(GetProcAddress(nHandler, "NFE_Cancelar"));
#else
	method = reinterpret_cast<NFE_Cancelar>(dlsym(nHandler, "NFE_Cancelar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, eChave.c_str(), eJustificativa.c_str(),
		eCNPJ.c_str(), ALote, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::EnviarEvento(std::int32_t idLote) const
{
	NFE_EnviarEvento method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_EnviarEvento>(GetProcAddress(nHandler, "NFE_EnviarEvento"));
#else
	method = reinterpret_cast<NFE_EnviarEvento>(dlsym(nHandler, "NFE_EnviarEvento"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, idLote, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::ConsultaCadastro(std::string cUF, std::string nDocumento, bool IsIE) const
{
	NFE_ConsultaCadastro method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ConsultaCadastro>(GetProcAddress(nHandler, "NFE_ConsultaCadastro"));
#else
	method = reinterpret_cast<NFE_ConsultaCadastro>(dlsym(nHandler, "NFE_ConsultaCadastro"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, cUF.c_str(), nDocumento.c_str(), IsIE, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::DistribuicaoDFePorUltNSU(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string eultNSU) const
{
	NFE_DistribuicaoDFePorUltNSU method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_DistribuicaoDFePorUltNSU>(GetProcAddress(nHandler, "NFE_DistribuicaoDFePorUltNSU"));
#else
	method = reinterpret_cast<NFE_DistribuicaoDFePorUltNSU>(dlsym(nHandler, "NFE_DistribuicaoDFePorUltNSU"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, AcUFAutor, eCNPJCPF.c_str(), eultNSU.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::DistribuicaoDFePorNSU(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string eNSU) const
{
	NFE_DistribuicaoDFePorNSU method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_DistribuicaoDFePorNSU>(GetProcAddress(nHandler, "NFE_DistribuicaoDFePorNSU"));
#else
	method = reinterpret_cast<NFE_DistribuicaoDFePorNSU>(dlsym(nHandler, "NFE_DistribuicaoDFePorNSU"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, AcUFAutor, eCNPJCPF.c_str(), eNSU.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

std::string ACBrNFe::DistribuicaoDFePorChave(std::int32_t AcUFAutor, std::string eCNPJCPF, std::string echNFe) const
{
	NFE_DistribuicaoDFePorChave method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_DistribuicaoDFePorChave>(GetProcAddress(nHandler, "NFE_DistribuicaoDFePorChave"));
#else
	method = reinterpret_cast<NFE_DistribuicaoDFePorChave>(dlsym(nHandler, "NFE_DistribuicaoDFePorChave"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, AcUFAutor, eCNPJCPF.c_str(), echNFe.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrNFe::EnviarEmail(std::string ePara, std::string eXmlNFe, bool AEnviaPDF, std::string eAssunto, std::string eCC,
	std::string eAnexos, std::string eMensagem) const
{
	NFE_EnviarEmail method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_EnviarEmail>(GetProcAddress(nHandler, "NFE_EnviarEmail"));
#else
	method = reinterpret_cast<NFE_EnviarEmail>(dlsym(nHandler, "NFE_EnviarEmail"));
#endif

	const int ret = method(this->libHandler, ePara.c_str(), eXmlNFe.c_str(), AEnviaPDF, eAssunto.c_str(),
		eCC.c_str(), eAnexos.c_str(), eMensagem.c_str());
	CheckResult(ret);
}

void ACBrNFe::EnviarEmailEvento(std::string ePara, std::string eXmlEvento, std::string eXmlNFe, bool AEnviaPDF,
	std::string eAssunto, std::string eCC, std::string eAnexos, std::string eMensagem) const
{
	NFE_EnviarEmailEvento method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_EnviarEmailEvento>(GetProcAddress(nHandler, "NFE_EnviarEmailEvento"));
#else
	method = reinterpret_cast<NFE_EnviarEmailEvento>(dlsym(nHandler, "NFE_EnviarEmailEvento"));
#endif

	const int ret = method(this->libHandler, ePara.c_str(), eXmlEvento.c_str(), eXmlNFe.c_str(), AEnviaPDF,
		eAssunto.c_str(), eCC.c_str(), eAnexos.c_str(), eMensagem.c_str());
	CheckResult(ret);
}

void ACBrNFe::Imprimir(std::string cImpressora, int nNumCopias, std::string cProtocolo, std::optional<bool> bMostrarPreview,
	std::optional<bool> cMarcaDagua, std::optional<bool> bViaConsumidor, std::optional<bool> bSimplificado) const
{
	NFE_Imprimir method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_Imprimir>(GetProcAddress(nHandler, "NFE_Imprimir"));
#else
	method = reinterpret_cast<NFE_Imprimir>(dlsym(nHandler, "NFE_Imprimir"));
#endif

	const std::string mPreview = bMostrarPreview.has_value() ? bMostrarPreview.value() ? "1" : "0" : "";
	const std::string mMarca = cMarcaDagua.has_value() ? cMarcaDagua.value() ? "1" : "0" : "";
	const std::string mViaConsumido = bViaConsumidor.has_value() ? bViaConsumidor.value() ? "1" : "0" : "";
	const std::string mSimplificado = bSimplificado.has_value() ? bSimplificado.value() ? "1" : "0" : "";

	const int ret = method(this->libHandler, cImpressora.c_str(), nNumCopias, cProtocolo.c_str(), mPreview.c_str(),
		mMarca.c_str(), mViaConsumido.c_str(), mSimplificado.c_str());
	CheckResult(ret);
}

void ACBrNFe::ImprimirPDF() const
{
	NFE_ImprimirPDF method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ImprimirPDF>(GetProcAddress(nHandler, "NFE_ImprimirPDF"));
#else
	method = reinterpret_cast<NFE_ImprimirPDF>(dlsym(nHandler, "NFE_ImprimirPDF"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrNFe::ImprimirEvento(std::string eXmlNFe, std::string eXmlEvento) const
{
	NFE_ImprimirEvento method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ImprimirEvento>(GetProcAddress(nHandler, "NFE_ImprimirEvento"));
#else
	method = reinterpret_cast<NFE_ImprimirEvento>(dlsym(nHandler, "NFE_ImprimirEvento"));
#endif

	const int ret = method(this->libHandler, eXmlNFe.c_str(), eXmlEvento.c_str());
	CheckResult(ret);
}

void ACBrNFe::ImprimirEventoPDF(std::string eXmlNFe, std::string eXmlEvento) const
{
	NFE_ImprimirEventoPDF method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ImprimirEventoPDF>(GetProcAddress(nHandler, "NFE_ImprimirEventoPDF"));
#else
	method = reinterpret_cast<NFE_ImprimirEventoPDF>(dlsym(nHandler, "NFE_ImprimirEventoPDF"));
#endif

	const int ret = method(this->libHandler, eXmlNFe.c_str(), eXmlEvento.c_str());
	CheckResult(ret);
}

void ACBrNFe::ImprimirInutilizacao(std::string eXmlInutilizacao) const
{
	NFE_ImprimirInutilizacao method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ImprimirInutilizacao>(GetProcAddress(nHandler, "NFE_ImprimirInutilizacao"));
#else
	method = reinterpret_cast<NFE_ImprimirInutilizacao>(dlsym(nHandler, "NFE_ImprimirInutilizacao"));
#endif

	const int ret = method(this->libHandler, eXmlInutilizacao.c_str());
	CheckResult(ret);
}

void ACBrNFe::ImprimirInutilizacaoPDF(std::string eXmlInutilizacao) const
{
	NFE_ImprimirInutilizacaoPDF method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_ImprimirInutilizacaoPDF>(GetProcAddress(nHandler, "NFE_ImprimirInutilizacaoPDF"));
#else
	method = reinterpret_cast<NFE_ImprimirInutilizacaoPDF>(dlsym(nHandler, "NFE_ImprimirInutilizacaoPDF"));
#endif

	const int ret = method(this->libHandler, eXmlInutilizacao.c_str());
	CheckResult(ret);
}

std::string ACBrNFe::Trim(std::string& buffer) const
{
	const auto wsfront = std::find_if_not(buffer.begin(), buffer.end(), [](const int c) {return std::isspace(c); });
	const auto wsback = std::find_if_not(buffer.rbegin(), buffer.rend(), [](const int c) {return std::isspace(c); }).base();
	return (wsback <= wsfront ? std::string() : std::string(wsfront, wsback));
}

std::vector<std::string> ACBrNFe::Split(std::string str, std::string token)
{
	std::vector<std::string> result;
	while (!str.empty()) {
		if (const std::size_t index = str.find(token); index != std::string::npos) {
			result.push_back(str.substr(0, index));
			str = str.substr(index + token.size());
			if (str.empty()) result.push_back(str);
		}
		else {
			result.push_back(str);
			str = "";
		}
	}
	return result;
}

void ACBrNFe::CheckResult(int ret) const
{
	if (ret >= 0) return;

	std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	NFE_UltimoRetorno method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<NFE_UltimoRetorno>(GetProcAddress(nHandler, "NFE_UltimoRetorno"));
#else
	method = reinterpret_cast<NFE_UltimoRetorno>(dlsym(nHandler, "NFE_UltimoRetorno"));
#endif

	method(this->libHandler, buffer.c_str(), &bufferLen);

	if (bufferLen <= BUFFER_LEN) throw new std::runtime_error(this->Trim(buffer).c_str());

	buffer.clear();
	buffer.resize(bufferLen, ' ');
	method(this->libHandler, buffer.c_str(), &bufferLen);
	throw new std::runtime_error(this->Trim(buffer).c_str());
}

std::string ACBrNFe::ProcessResult(std::string buffer, int buffer_len) const
{
	if (buffer_len > BUFFER_LEN) {
		NFE_UltimoRetorno method;

#if defined(ISWINDOWS)
		method = reinterpret_cast<NFE_UltimoRetorno>(GetProcAddress(nHandler, "NFE_UltimoRetorno"));
#else
		method = reinterpret_cast<NFE_UltimoRetorno>(dlsym(nHandler, "NFE_UltimoRetorno"));
#endif

		buffer.clear();
		buffer.resize(buffer_len, ' ');
		method(this->libHandler, buffer.c_str(), &buffer_len);
	}

	return this->Trim(buffer);
}