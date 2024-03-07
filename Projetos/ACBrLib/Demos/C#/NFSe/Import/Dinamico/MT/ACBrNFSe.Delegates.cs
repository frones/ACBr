using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.NFSe
{
    public sealed partial class ACBrNFSe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_CarregarXML(IntPtr handle, string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_CarregarINI(IntPtr handle, string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ObterXml(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_GravarXml(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ObterIni(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_GravarIni(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_LimparLista(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Emitir(IntPtr handle, string aLote, int aModoEnvio, bool aImprimir, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Cancelar(IntPtr handle, string aInfCancelamento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_SubstituirNFSe(IntPtr handle, string aNumeroNFSe, string aSerieNFSe, string aCodigoCancelamento, string aMotivoCancelamento, string aNumeroLote, string aCodigoVerificacao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_LinkNFSe(IntPtr handle, string aNumeroNFSe, string aCodigoVerificacao, string aChaveAcesso, string aValorServico, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_GerarLote(IntPtr handle, string aLote, int aQtdMaximaRps, int aModoEnvio, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_GerarToken(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarSituacao(IntPtr handle, string aProtocolo, string aNumeroLote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarLoteRps(IntPtr handle, string aProcotolo, string aNumLote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSePorRps(IntPtr handle, string aNumeroRps, string aSerie, string aTipo, string aCodigoVerificacao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSePorNumero(IntPtr handle, string aNumero, int aPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSePorPeriodo(IntPtr handle, DateTime aDataInicial, DateTime aDataFinal, int aPagina, string aNumeroLote, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSePorFaixa(IntPtr handle, string aNumeroInicial, string aNumeroFinal, int aPagina, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeGenerico(IntPtr handle, string aInfConsultaNFSe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarLinkNFSe(IntPtr handle, string aInfConsultaLinkNFSe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_EnviarEmail(IntPtr handle, string ePara, string eXmlNFSe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_Imprimir(IntPtr handle, string cImpressora, int nNumCopias, string bGerarPDF, string bMostrarPreview, string cCancelada);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ImprimirPDF(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_SalvarPDF(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoPrestadoPorNumero(IntPtr handle, string aNumero, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(IntPtr handle, DateTime aDataInicial, DateTime aDataFinal, int aPagina, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoPrestadoPorTomador(IntPtr handle, string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(IntPtr handle, string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoTomadoPorNumero(IntPtr handle, string aNumero, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoTomadoPorPrestador(IntPtr handle, string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoTomadoPorTomador(IntPtr handle, string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoTomadoPorPeriodo(IntPtr handle, DateTime aDataInicial, DateTime aDataFinal, int aPagina, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSeServicoTomadoPorIntermediario(IntPtr handle, string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_EnviarEvento(IntPtr handle, string aInfEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarDPSPorChave(IntPtr handle, string aChaveDPS, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarNFSePorChave(IntPtr handle, string aChaveNFSe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarEvento(IntPtr handle, string aChave, int aTipoEvento, int aNumSeq, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarDFe(IntPtr handle, int aNSU, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ObterDANFSE(IntPtr handle, string aChaveNFSe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFSE_ConsultarParametros(IntPtr handle, int aTipoParametroMunicipio, string aCodigoServico, DateTime aCompetencia, string aNumeroBeneficio, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<NFSE_Inicializar>("NFSE_Inicializar");
            AddMethod<NFSE_Finalizar>("NFSE_Finalizar");
            AddMethod<NFSE_Nome>("NFSE_Nome");
            AddMethod<NFSE_Versao>("NFSE_Versao");
            AddMethod<NFSE_UltimoRetorno>("NFSE_UltimoRetorno");
            AddMethod<NFSE_ConfigImportar>("NFSE_ConfigImportar");
            AddMethod<NFSE_ConfigExportar>("NFSE_ConfigExportar");
            AddMethod<NFSE_ConfigLer>("NFSE_ConfigLer");
            AddMethod<NFSE_ConfigGravar>("NFSE_ConfigGravar");
            AddMethod<NFSE_ConfigLerValor>("NFSE_ConfigLerValor");
            AddMethod<NFSE_ConfigGravarValor>("NFSE_ConfigGravarValor");
            AddMethod<NFSE_CarregarXML>("NFSE_CarregarXML");
            AddMethod<NFSE_CarregarINI>("NFSE_CarregarINI");
            AddMethod<NFSE_ObterXml>("NFSE_ObterXml");
            AddMethod<NFSE_GravarXml>("NFSE_GravarXml");
            AddMethod<NFSE_ObterIni>("NFSE_ObterIni");
            AddMethod<NFSE_GravarIni>("NFSE_GravarIni");
            AddMethod<NFSE_LimparLista>("NFSE_LimparLista");
            AddMethod<NFSE_ObterCertificados>("NFSE_ObterCertificados");
            AddMethod<NFSE_Emitir>("NFSE_Emitir");
            AddMethod<NFSE_Cancelar>("NFSE_Cancelar");
            AddMethod<NFSE_SubstituirNFSe>("NFSE_SubstituirNFSe");
            AddMethod<NFSE_LinkNFSe>("NFSE_LinkNFSe");
            AddMethod<NFSE_GerarLote>("NFSE_GerarLote");
            AddMethod<NFSE_GerarToken>("NFSE_GerarToken");
            AddMethod<NFSE_ConsultarSituacao>("NFSE_ConsultarSituacao");
            AddMethod<NFSE_ConsultarLoteRps>("NFSE_ConsultarLoteRps");
            AddMethod<NFSE_ConsultarNFSePorRps>("NFSE_ConsultarNFSePorRps");
            AddMethod<NFSE_ConsultarNFSePorNumero>("NFSE_ConsultarNFSePorNumero");
            AddMethod<NFSE_ConsultarNFSePorPeriodo>("NFSE_ConsultarNFSePorPeriodo");
            AddMethod<NFSE_ConsultarNFSePorFaixa>("NFSE_ConsultarNFSePorFaixa");
            AddMethod<NFSE_ConsultarNFSeGenerico>("NFSE_ConsultarNFSeGenerico");
            AddMethod<NFSE_ConsultarLinkNFSe>("NFSE_ConsultarLinkNFSe");
            AddMethod<NFSE_EnviarEmail>("NFSE_EnviarEmail");
            AddMethod<NFSE_Imprimir>("NFSE_Imprimir");
            AddMethod<NFSE_ImprimirPDF>("NFSE_ImprimirPDF");
            AddMethod<NFSE_SalvarPDF>("NFSE_SalvarPDF");
            AddMethod<NFSE_ConsultarNFSeServicoPrestadoPorNumero>("NFSE_ConsultarNFSeServicoPrestadoPorNumero");
            AddMethod<NFSE_ConsultarNFSeServicoPrestadoPorPeriodo>("NFSE_ConsultarNFSeServicoPrestadoPorPeriodo");
            AddMethod<NFSE_ConsultarNFSeServicoPrestadoPorTomador>("NFSE_ConsultarNFSeServicoPrestadoPorTomador");
            AddMethod<NFSE_ConsultarNFSeServicoPrestadoPorIntermediario>("NFSE_ConsultarNFSeServicoPrestadoPorIntermediario");
            AddMethod<NFSE_ConsultarNFSeServicoTomadoPorNumero>("NFSE_ConsultarNFSeServicoTomadoPorNumero");
            AddMethod<NFSE_ConsultarNFSeServicoTomadoPorPrestador>("NFSE_ConsultarNFSeServicoTomadoPorPrestador");
            AddMethod<NFSE_ConsultarNFSeServicoTomadoPorTomador>("NFSE_ConsultarNFSeServicoTomadoPorTomador");
            AddMethod<NFSE_ConsultarNFSeServicoTomadoPorPeriodo>("NFSE_ConsultarNFSeServicoTomadoPorPeriodo");
            AddMethod<NFSE_ConsultarNFSeServicoTomadoPorIntermediario>("NFSE_ConsultarNFSeServicoTomadoPorIntermediario");
            AddMethod<NFSE_EnviarEvento>("NFSE_EnviarEvento");
            AddMethod<NFSE_ConsultarDPSPorChave>("NFSE_ConsultarDPSPorChave");
            AddMethod<NFSE_ConsultarNFSePorChave>("NFSE_ConsultarNFSePorChave");
            AddMethod<NFSE_ConsultarEvento>("NFSE_ConsultarEvento");
            AddMethod<NFSE_ConsultarDFe>("NFSE_ConsultarDFe");
            AddMethod<NFSE_ObterDANFSE>("NFSE_ObterDANFSE");
            AddMethod<NFSE_ConsultarParametros>("NFSE_ConsultarParametros");
        }
    }
}