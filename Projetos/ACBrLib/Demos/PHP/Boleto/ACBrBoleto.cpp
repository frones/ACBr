#include <cstdint>
#include <string>
#include <algorithm>
#include <functional>
#include <stdexcept>
#include "ACBrBoleto.h"

#if defined(ISWINDOWS)
#include <windows.h>
#else
#include <dlfcn.h>
#endif

ACBrBoleto::ACBrBoleto(std::string eArqConfig, std::string eChaveCrypt) {
#if defined(ISWINDOWS)
#if defined(ENVIRONMENT32)
	nHandler = LoadLibraryW(L"ACBrBoleto32.dll");
#else
	nHandler = LoadLibraryW(L"ACBrBoleto64.dll");
#endif
#else
#if defined(ENVIRONMENT32)
	std::string path = "/usr/lib/libacbrboleto32.so";
	nHandler = dlopen(path.c_str(), RTLD_LAZY);
#else
	std::string path = "/usr/lib/libacbrboleto64.so";
	nHandler = dlopen(path.c_str(), RTLD_LAZY);
#endif
#endif

	Boleto_Inicializar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_Inicializar>(GetProcAddress(nHandler, "Boleto_Inicializar"));
#else
	method = reinterpret_cast<Boleto_Inicializar>(dlsym(nHandler, "Boleto_Inicializar"));
#endif

	const int ret = method(&this->libHandler, eArqConfig.c_str(), eChaveCrypt.c_str());
	CheckResult(ret);
}

ACBrBoleto::~ACBrBoleto() {
	Boleto_Finalizar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_Finalizar>(GetProcAddress(nHandler, "Boleto_Finalizar"));
#else
	method = reinterpret_cast<Boleto_Finalizar>(dlsym(nHandler, "Boleto_Finalizar"));
#endif

	method(this->libHandler);

#if defined(ISWINDOWS)
	FreeLibrary(nHandler);
#else
	dlclose(nHandler);
#endif
}

std::string ACBrBoleto::Nome() const
{
	Boleto_Nome method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_Nome>(GetProcAddress(nHandler, "Boleto_Nome"));
#else
	method = reinterpret_cast<Boleto_Nome>(dlsym(nHandler, "Boleto_Nome"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

std::string ACBrBoleto::Versao() const
{
	Boleto_Versao method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_Versao>(GetProcAddress(nHandler, "Boleto_Versao"));
#else
	method = reinterpret_cast<Boleto_Versao>(dlsym(nHandler, "Boleto_Versao"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

void ACBrBoleto::ConfigLer(std::string eArqConfig) const
{
	Boleto_ConfigLer method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigLer>(GetProcAddress(nHandler, "Boleto_ConfigLer"));
#else
	method = reinterpret_cast<Boleto_ConfigLer>(dlsym(nHandler, "Boleto_ConfigLer"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

void ACBrBoleto::ConfigGravar(std::string eArqConfig) const
{
	Boleto_ConfigGravar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigGravar>(GetProcAddress(nHandler, "Boleto_ConfigGravar"));
#else
	method = reinterpret_cast<Boleto_ConfigGravar>(dlsym(nHandler, "Boleto_ConfigGravar"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

void ACBrBoleto::ConfigGravarValor(std::string eSessao, std::string eChave, std::string sValor) const
{
	Boleto_ConfigGravarValor method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigGravarValor>(GetProcAddress(nHandler, "Boleto_ConfigGravarValor"));
#else
	method = reinterpret_cast<Boleto_ConfigGravarValor>(dlsym(nHandler, "Boleto_ConfigGravarValor"));
#endif

	const int ret = method(this->libHandler, eSessao.c_str(), eChave.c_str(), sValor.c_str());
	CheckResult(ret);
}

std::string ACBrBoleto::ConfigLerValor(std::string eSessao, std::string eChave) const
{
	Boleto_ConfigLerValor method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigLerValor>(GetProcAddress(nHandler, "Boleto_ConfigLerValor"));
#else
	method = reinterpret_cast<Boleto_ConfigLerValor>(dlsym(nHandler, "Boleto_ConfigLerValor"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, eSessao.c_str(), eChave.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrBoleto::ConfigImportar(std::string eArqConfig) const
{
	Boleto_ConfigImportar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigImportar>(GetProcAddress(nHandler, "Boleto_ConfigImportar"));
#else
	method = reinterpret_cast<Boleto_ConfigImportar>(dlsym(nHandler, "Boleto_ConfigImportar"));
#endif

	const int ret = method(this->libHandler, eArqConfig.c_str());
	CheckResult(ret);
}

std::string ACBrBoleto::ConfigExportar() const
{
	Boleto_ConfigExportar method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigExportar>(GetProcAddress(nHandler, "Boleto_ConfigExportar"));
#else
	method = reinterpret_cast<Boleto_ConfigExportar>(dlsym(nHandler, "Boleto_ConfigExportar"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return this->ProcessResult(buffer, bufferLen);
}

void ACBrBoleto::ConfigurarDados(std::string eArquivoIni) const
{
	Boleto_ConfigurarDados method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ConfigurarDados>(GetProcAddress(nHandler, "Boleto_ConfigurarDados"));
#else
	method = reinterpret_cast<Boleto_ConfigurarDados>(dlsym(nHandler, "Boleto_ConfigurarDados"));
#endif

	const int ret = method(this->libHandler, eArquivoIni.c_str());
	CheckResult(ret);
}

void ACBrBoleto::IncluirTitulos(std::string eArquivoIni, std::optional<TpSaida> eTpSaida) const
{
	Boleto_IncluirTitulos method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_IncluirTitulos>(GetProcAddress(nHandler, "Boleto_IncluirTitulos"));
#else
	method = reinterpret_cast<Boleto_IncluirTitulos>(dlsym(nHandler, "Boleto_IncluirTitulos"));
#endif

	std::string saida = "";
	if(eTpSaida.has_value()){
		switch (eTpSaida.value())
		{
			case PDF:
				saida = "P";
				break;
				
			case Impressora:
				saida = "I";
				break;
				
			case EMail:
				saida = "E";
				break;
				
			default:
				saida = "";
				break;
		}
	}

	

	const int ret = method(this->libHandler, eArquivoIni.c_str(), saida.c_str());
	CheckResult(ret);
}

void ACBrBoleto::LimparLista() const
{
	Boleto_LimparLista method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_LimparLista>(GetProcAddress(nHandler, "Boleto_LimparLista"));
#else
	method = reinterpret_cast<Boleto_LimparLista>(dlsym(nHandler, "Boleto_LimparLista"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

std::int32_t ACBrBoleto::TotalTitulosLista() const
{
	Boleto_TotalTitulosLista method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_TotalTitulosLista>(GetProcAddress(nHandler, "Boleto_TotalTitulosLista"));
#else
	method = reinterpret_cast<Boleto_TotalTitulosLista>(dlsym(nHandler, "Boleto_TotalTitulosLista"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);

	return ret;
}

void ACBrBoleto::Imprimir() const
{
	Imprimir("");
}

void ACBrBoleto::Imprimir(std::string eNomeImpressora) const
{
	Boleto_Imprimir method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_Imprimir>(GetProcAddress(nHandler, "Boleto_Imprimir"));
#else
	method = reinterpret_cast<Boleto_Imprimir>(dlsym(nHandler, "Boleto_Imprimir"));
#endif

	const int ret = method(this->libHandler, eNomeImpressora.c_str());
	CheckResult(ret);
}

void ACBrBoleto::ImprimirBoleto(std::int32_t idx) const
{
	ImprimirBoleto(idx, "");
}

void ACBrBoleto::ImprimirBoleto(std::int32_t idx, std::string eNomeImpressora) const
{
	Boleto_ImprimirBoleto method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ImprimirBoleto>(GetProcAddress(nHandler, "Boleto_ImprimirBoleto"));
#else
	method = reinterpret_cast<Boleto_ImprimirBoleto>(dlsym(nHandler, "Boleto_ImprimirBoleto"));
#endif

	const int ret = method(this->libHandler, idx, eNomeImpressora.c_str());
	CheckResult(ret);
}

void ACBrBoleto::GerarPDF() const
{
	Boleto_GerarPDF method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_GerarPDF>(GetProcAddress(nHandler, "Boleto_GerarPDF"));
#else
	method = reinterpret_cast<Boleto_GerarPDF>(dlsym(nHandler, "Boleto_GerarPDF"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrBoleto::GerarPDFBoleto(std::int32_t idx) const
{
	Boleto_GerarPDFBoleto method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_GerarPDFBoleto>(GetProcAddress(nHandler, "Boleto_GerarPDFBoleto"));
#else
	method = reinterpret_cast<Boleto_GerarPDFBoleto>(dlsym(nHandler, "Boleto_GerarPDFBoleto"));
#endif

	const int ret = method(this->libHandler, idx);
	CheckResult(ret);
}

void ACBrBoleto::GerarHTML() const
{
	Boleto_GerarHTML method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_GerarHTML>(GetProcAddress(nHandler, "Boleto_GerarHTML"));
#else
	method = reinterpret_cast<Boleto_GerarHTML>(dlsym(nHandler, "Boleto_GerarHTML"));
#endif

	const int ret = method(this->libHandler);
	CheckResult(ret);
}

void ACBrBoleto::GerarRemessa(std::string eDir, std::int32_t eNumArquivo, std::string eNomeArq) const
{
	Boleto_GerarRemessa method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_GerarRemessa>(GetProcAddress(nHandler, "Boleto_GerarRemessa"));
#else
	method = reinterpret_cast<Boleto_GerarRemessa>(dlsym(nHandler, "Boleto_GerarRemessa"));
#endif

	const int ret = method(this->libHandler, eDir.c_str(), eNumArquivo, eNomeArq.c_str());
	CheckResult(ret);
}

void ACBrBoleto::LerRetorno(std::string eDir, std::string eNomeArq) const
{
	Boleto_LerRetorno method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_LerRetorno>(GetProcAddress(nHandler, "Boleto_LerRetorno"));
#else
	method = reinterpret_cast<Boleto_LerRetorno>(dlsym(nHandler, "Boleto_LerRetorno"));
#endif

	const int ret = method(this->libHandler, eDir.c_str(), eNomeArq.c_str());
	CheckResult(ret);
}

std::string ACBrBoleto::ObterRetorno(std::string eDir, std::string eNomeArq) const
{
	Boleto_ObterRetorno method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ObterRetorno>(GetProcAddress(nHandler, "Boleto_ObterRetorno"));
#else
	method = reinterpret_cast<Boleto_ObterRetorno>(dlsym(nHandler, "Boleto_ObterRetorno"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, eDir.c_str(), eNomeArq.c_str(), buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

void ACBrBoleto::EnviarEmail(std::string ePara, std::string eNomeArq, std::string eMensagem, std::vector<std::string> eCC) const
{
	Boleto_EnviarEmail method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_EnviarEmail>(GetProcAddress(nHandler, "Boleto_EnviarEmail"));
#else
	method = reinterpret_cast<Boleto_EnviarEmail>(dlsym(nHandler, "Boleto_EnviarEmail"));
#endif
	std::string CC;
	for (const auto &piece : eCC) CC += piece;

	const int ret = method(this->libHandler, ePara.c_str(), eNomeArq.c_str(), eMensagem.c_str(), CC.c_str());
	CheckResult(ret);
}

void ACBrBoleto::EnviarEmailBoleto(std::int32_t idx, std::string ePara, std::string eNomeArq, std::string eMensagem, std::vector<std::string> eCC) const
{
	Boleto_EnviarEmailBoleto method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_EnviarEmailBoleto>(GetProcAddress(nHandler, "Boleto_EnviarEmailBoleto"));
#else
	method = reinterpret_cast<Boleto_EnviarEmailBoleto>(dlsym(nHandler, "Boleto_EnviarEmailBoleto"));
#endif
	std::string CC;
	for (const auto &piece : eCC) CC += piece;

	const int ret = method(this->libHandler, idx, ePara.c_str(), eNomeArq.c_str(), eMensagem.c_str(), CC.c_str());
	CheckResult(ret);
}

void ACBrBoleto::SetDiretorioArquivo(std::string eDir, std::string eNomeArq) const
{
	Boleto_SetDiretorioArquivo method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_SetDiretorioArquivo>(GetProcAddress(nHandler, "Boleto_SetDiretorioArquivo"));
#else
	method = reinterpret_cast<Boleto_SetDiretorioArquivo>(dlsym(nHandler, "Boleto_SetDiretorioArquivo"));
#endif

	const int ret = method(this->libHandler, eDir.c_str(), eNomeArq.c_str());
	CheckResult(ret);
}

std::vector<std::string> ACBrBoleto::ListaBancos() const
{
	Boleto_ListaBancos method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ListaBancos>(GetProcAddress(nHandler, "Boleto_ListaBancos"));
#else
	method = reinterpret_cast<Boleto_ListaBancos>(dlsym(nHandler, "Boleto_ListaBancos"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return Split(ProcessResult(buffer, bufferLen), "|");
}

std::vector<std::string> ACBrBoleto::ListaCaractTitulo() const
{
	Boleto_ListaCaractTitulo method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ListaCaractTitulo>(GetProcAddress(nHandler, "Boleto_ListaCaractTitulo"));
#else
	method = reinterpret_cast<Boleto_ListaCaractTitulo>(dlsym(nHandler, "Boleto_ListaCaractTitulo"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return Split(ProcessResult(buffer, bufferLen), "|");
}

std::vector<std::string> ACBrBoleto::ListaOcorrencias() const
{
	Boleto_ListaOcorrencias method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ListaOcorrencias>(GetProcAddress(nHandler, "Boleto_ListaOcorrencias"));
#else
	method = reinterpret_cast<Boleto_ListaOcorrencias>(dlsym(nHandler, "Boleto_ListaOcorrencias"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return Split(ProcessResult(buffer, bufferLen), "|");
}

std::vector<std::string> ACBrBoleto::ListaOcorrenciasEX() const
{
	Boleto_ListaOcorrenciasEX method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_ListaOcorrenciasEX>(GetProcAddress(nHandler, "Boleto_ListaOcorrenciasEX"));
#else
	method = reinterpret_cast<Boleto_ListaOcorrenciasEX>(dlsym(nHandler, "Boleto_ListaOcorrenciasEX"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return Split(ProcessResult(buffer, bufferLen), "|");
}

std::int32_t ACBrBoleto::TamNossoNumero(std::string eCarteira, std::string enossoNumero, std::string eConvenio) const
{
	Boleto_TamNossoNumero method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_TamNossoNumero>(GetProcAddress(nHandler, "Boleto_TamNossoNumero"));
#else
	method = reinterpret_cast<Boleto_TamNossoNumero>(dlsym(nHandler, "Boleto_TamNossoNumero"));
#endif

	const int ret = method(this->libHandler, eCarteira.c_str(), enossoNumero.c_str(), eConvenio.c_str());
	CheckResult(ret);

	return ret;
}

std::string ACBrBoleto::CodigosMoraAceitos() const
{
	Boleto_CodigosMoraAceitos method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_CodigosMoraAceitos>(GetProcAddress(nHandler, "Boleto_CodigosMoraAceitos"));
#else
	method = reinterpret_cast<Boleto_CodigosMoraAceitos>(dlsym(nHandler, "Boleto_CodigosMoraAceitos"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

void ACBrBoleto::SelecionaBanco(std::string eCodBanco) const
{
	Boleto_SelecionaBanco method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_SelecionaBanco>(GetProcAddress(nHandler, "Boleto_SelecionaBanco"));
#else
	method = reinterpret_cast<Boleto_SelecionaBanco>(dlsym(nHandler, "Boleto_SelecionaBanco"));
#endif

	const int ret = method(this->libHandler, eCodBanco.c_str());
	CheckResult(ret);
}

std::string ACBrBoleto::MontarNossoNumero(std::int32_t idx) const
{
	Boleto_MontarNossoNumero method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_MontarNossoNumero>(GetProcAddress(nHandler, "Boleto_MontarNossoNumero"));
#else
	method = reinterpret_cast<Boleto_MontarNossoNumero>(dlsym(nHandler, "Boleto_MontarNossoNumero"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, idx, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

std::string ACBrBoleto::RetornaLinhaDigitavel(std::int32_t idx) const
{
	Boleto_RetornaLinhaDigitavel method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_RetornaLinhaDigitavel>(GetProcAddress(nHandler, "Boleto_RetornaLinhaDigitavel"));
#else
	method = reinterpret_cast<Boleto_RetornaLinhaDigitavel>(dlsym(nHandler, "Boleto_RetornaLinhaDigitavel"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, idx, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

std::string ACBrBoleto::RetornaCodigoBarras(std::int32_t idx) const
{
	Boleto_RetornaCodigoBarras method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_RetornaCodigoBarras>(GetProcAddress(nHandler, "Boleto_RetornaCodigoBarras"));
#else
	method = reinterpret_cast<Boleto_RetornaCodigoBarras>(dlsym(nHandler, "Boleto_RetornaCodigoBarras"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, idx, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

std::string ACBrBoleto::EnviarBoleto() const
{
	Boleto_EnviarBoleto method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_EnviarBoleto>(GetProcAddress(nHandler, "Boleto_EnviarBoleto"));
#else
	method = reinterpret_cast<Boleto_EnviarBoleto>(dlsym(nHandler, "Boleto_EnviarBoleto"));
#endif

	const std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	const int ret = method(this->libHandler, buffer.c_str(), &bufferLen);
	CheckResult(ret);

	return ProcessResult(buffer, bufferLen);
}

std::string ACBrBoleto::Trim(std::string& buffer) const
{
	const auto wsfront = std::find_if_not(buffer.begin(), buffer.end(), [](const int c) {return std::isspace(c); });
	const auto wsback = std::find_if_not(buffer.rbegin(), buffer.rend(), [](const int c) {return std::isspace(c); }).base();
	return (wsback <= wsfront ? std::string() : std::string(wsfront, wsback));
}

std::vector<std::string> ACBrBoleto::Split(std::string str, std::string token)
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

void ACBrBoleto::CheckResult(int ret) const
{
	if (ret >= 0) return;

	std::string buffer(BUFFER_LEN, ' ');
	int bufferLen = BUFFER_LEN;

	Boleto_UltimoRetorno method;

#if defined(ISWINDOWS)
	method = reinterpret_cast<Boleto_UltimoRetorno>(GetProcAddress(nHandler, "Boleto_UltimoRetorno"));
#else
	method = reinterpret_cast<Boleto_UltimoRetorno>(dlsym(nHandler, "Boleto_UltimoRetorno"));
#endif

	method(this->libHandler, buffer.c_str(), &bufferLen);

	if (bufferLen <= BUFFER_LEN) throw new std::runtime_error(this->Trim(buffer).c_str());

	buffer.clear();
	buffer.resize(bufferLen, ' ');
	method(this->libHandler, buffer.c_str(), &bufferLen);
	throw new std::runtime_error(this->Trim(buffer).c_str());
}

std::string ACBrBoleto::ProcessResult(std::string buffer, int buffer_len) const
{
	if (buffer_len > BUFFER_LEN) {
		Boleto_UltimoRetorno method;

#if defined(ISWINDOWS)
		method = reinterpret_cast<Boleto_UltimoRetorno>(GetProcAddress(nHandler, "Boleto_UltimoRetorno"));
#else
		method = reinterpret_cast<Boleto_UltimoRetorno>(dlsym(nHandler, "Boleto_UltimoRetorno"));
#endif

		buffer.clear();
		buffer.resize(buffer_len, ' ');
		method(this->libHandler, buffer.c_str(), &buffer_len);
	}

	return this->Trim(buffer);
}