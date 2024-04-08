using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using static ACBrLib.CTe.ACBrCTe;

namespace ACBrLib.CTe
{
    public sealed partial class ACBrCTe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_CarregarXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_CarregarINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ObterXml(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_GravarXml(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ObterIni(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_GravarIni(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_CarregarEventoXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_CarregarEventoINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_LimparLista();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_LimparListaEventos();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Assinar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Validar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
            int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ObterCertificados(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_GetPath(int tipo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_GetPathEvento(string aCodEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_StatusServico(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Consultar(string eChaveOuCTe, bool AExtrairEventos, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConsultaCadastro(string cUF, string nDocumento, bool nIE, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Enviar(int aLote, bool imprimir, bool sincrono, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_DistribuicaoDFe(int acUFAutor, string eCnpjcpf, string eultNsu, string ArquivoOuXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_EnviarEmail(string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_Imprimir(string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ImprimirPDF();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ImprimirEvento(string eArquivoXmlCTe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ImprimirEventoPDF(string eArquivoXmlCTe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ImprimirInutilizacao(string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CTE_ImprimirInutilizacaoPDF(string eArquivoXml);

        protected override void InitializeMethods()
        {
            AddMethod<CTE_Inicializar>("CTE_Inicializar");
            AddMethod<CTE_Finalizar>("CTE_Finalizar");
            AddMethod<CTE_Nome>("CTE_Nome");
            AddMethod<CTE_Versao>("CTE_Versao");
            AddMethod<CTE_OpenSSLInfo>("CTE_OpenSSLInfo");
            AddMethod<CTE_UltimoRetorno>("CTE_UltimoRetorno");
            AddMethod<CTE_ConfigImportar>("CTE_ConfigImportar");
            AddMethod<CTE_ConfigExportar>("CTE_ConfigExportar");
            AddMethod<CTE_ConfigLer>("CTE_ConfigLer");
            AddMethod<CTE_ConfigGravar>("CTE_ConfigGravar");
            AddMethod<CTE_ConfigLerValor>("CTE_ConfigLerValor");
            AddMethod<CTE_ConfigGravarValor>("CTE_ConfigGravarValor");
            AddMethod<CTE_CarregarXML>("CTE_CarregarXML");
            AddMethod<CTE_CarregarINI>("CTE_CarregarINI");
            AddMethod<CTE_CarregarEventoXML>("CTE_CarregarEventoXML");
            AddMethod<CTE_CarregarEventoINI>("CTE_CarregarEventoINI");
            AddMethod<CTE_ObterXml>("CTE_ObterXml");
            AddMethod<CTE_GravarXml>("CTE_GravarXml");
            AddMethod<CTE_ObterIni>("CTE_ObterIni");
            AddMethod<CTE_GravarIni>("CTE_GravarIni");
            AddMethod<CTE_LimparLista>("CTE_LimparLista");
            AddMethod<CTE_LimparListaEventos>("CTE_LimparListaEventos");
            AddMethod<CTE_Assinar>("CTE_Assinar");
            AddMethod<CTE_Validar>("CTE_Validar");
            AddMethod<CTE_ValidarRegrasdeNegocios>("CTE_ValidarRegrasdeNegocios");
            AddMethod<CTE_VerificarAssinatura>("CTE_VerificarAssinatura");
            AddMethod<CTE_GerarChave>("CTE_GerarChave");
            AddMethod<CTE_ObterCertificados>("CTE_ObterCertificados");
            AddMethod<CTE_GetPath>("CTE_GetPath");
            AddMethod<CTE_GetPathEvento>("CTE_GetPathEvento");
            AddMethod<CTE_StatusServico>("CTE_StatusServico");
            AddMethod<CTE_Consultar>("CTE_Consultar");
            AddMethod<CTE_ConsultaCadastro>("CTE_ConsultaCadastro");
            AddMethod<CTE_Inutilizar>("CTE_Inutilizar");
            AddMethod<CTE_Enviar>("CTE_Enviar");
            AddMethod<CTE_ConsultarRecibo>("CTE_ConsultarRecibo");
            AddMethod<CTE_Cancelar>("CTE_Cancelar");
            AddMethod<CTE_EnviarEvento>("CTE_EnviarEvento");
            AddMethod<CTE_DistribuicaoDFePorUltNSU>("CTE_DistribuicaoDFePorUltNSU");
            AddMethod<CTE_DistribuicaoDFe>("CTE_DistribuicaoDFe");
            AddMethod<CTE_DistribuicaoDFePorNSU>("CTE_DistribuicaoDFePorNSU");
            AddMethod<CTE_DistribuicaoDFePorChave>("CTE_DistribuicaoDFePorChave");
            AddMethod<CTE_EnviarEmail>("CTE_EnviarEmail");
            AddMethod<CTE_EnviarEmailEvento>("CTE_EnviarEmailEvento");
            AddMethod<CTE_Imprimir>("CTE_Imprimir");
            AddMethod<CTE_ImprimirPDF>("CTE_ImprimirPDF");
            AddMethod<CTE_ImprimirEvento>("CTE_ImprimirEvento");
            AddMethod<CTE_ImprimirEventoPDF>("CTE_ImprimirEventoPDF");
            AddMethod<CTE_ImprimirInutilizacao>("CTE_ImprimirInutilizacao");
            AddMethod<CTE_ImprimirInutilizacaoPDF>("CTE_ImprimirInutilizacaoPDF");
        }
    }
}