using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.NFe
{
    public sealed partial class ACBrNFe
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterXml(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GravarXml(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterIni(int AIndex, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GravarIni(int AIndex, string eNomeArquivo, string ePathArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarEventoXML(string eArquivoOuXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_CarregarEventoINI(string eArquivoOuIni);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_LimparLista();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_LimparListaEventos();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Assinar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Validar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
            int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ObterCertificados(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GetPath(int tipo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_GetPathEvento(string aCodEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_StatusServico(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Consultar(string eChaveOuNFe, bool AExtrairEventos, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConsultaCadastro(string cUF, string nDocumento, bool nIE, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Enviar(int aLote, bool imprimir, bool sincrono, bool zipado, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
            StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFe(int acUFAutor, string eCnpjcpf, string eultNsu, string ArquivoOuXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_Imprimir(string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview, string cMarcaDagua, string bViaConsumidor, string bSimplificado);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirPDF();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_SalvarPDF(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_SalvarEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirInutilizacao(string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_ImprimirInutilizacaoPDF(string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int NFE_SalvarInutilizacaoPDF(string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<NFE_Inicializar>("NFE_Inicializar");
            AddMethod<NFE_Finalizar>("NFE_Finalizar");
            AddMethod<NFE_Nome>("NFE_Nome");
            AddMethod<NFE_Versao>("NFE_Versao");
            AddMethod<NFE_OpenSSLInfo>("NFE_OpenSSLInfo");
            AddMethod<NFE_UltimoRetorno>("NFE_UltimoRetorno");
            AddMethod<NFE_ConfigImportar>("NFE_ConfigImportar");
            AddMethod<NFE_ConfigExportar>("NFE_ConfigExportar");
            AddMethod<NFE_ConfigLer>("NFE_ConfigLer");
            AddMethod<NFE_ConfigGravar>("NFE_ConfigGravar");
            AddMethod<NFE_ConfigLerValor>("NFE_ConfigLerValor");
            AddMethod<NFE_ConfigGravarValor>("NFE_ConfigGravarValor");
            AddMethod<NFE_CarregarXML>("NFE_CarregarXML");
            AddMethod<NFE_CarregarINI>("NFE_CarregarINI");
            AddMethod<NFE_ObterXml>("NFE_ObterXml");
            AddMethod<NFE_GravarXml>("NFE_GravarXml");
            AddMethod<NFE_ObterIni>("NFE_ObterIni");
            AddMethod<NFE_GravarIni>("NFE_GravarIni");
            AddMethod<NFE_CarregarEventoXML>("NFE_CarregarEventoXML");
            AddMethod<NFE_CarregarEventoINI>("NFE_CarregarEventoINI");
            AddMethod<NFE_LimparLista>("NFE_LimparLista");
            AddMethod<NFE_LimparListaEventos>("NFE_LimparListaEventos");
            AddMethod<NFE_Assinar>("NFE_Assinar");
            AddMethod<NFE_Validar>("NFE_Validar");
            AddMethod<NFE_ValidarRegrasdeNegocios>("NFE_ValidarRegrasdeNegocios");
            AddMethod<NFE_VerificarAssinatura>("NFE_VerificarAssinatura");
            AddMethod<NFE_GerarChave>("NFE_GerarChave");
            AddMethod<NFE_ObterCertificados>("NFE_ObterCertificados");
            AddMethod<NFE_GetPath>("NFE_GetPath");
            AddMethod<NFE_GetPathEvento>("NFE_GetPathEvento");
            AddMethod<NFE_StatusServico>("NFE_StatusServico");
            AddMethod<NFE_Consultar>("NFE_Consultar");
            AddMethod<NFE_ConsultaCadastro>("NFE_ConsultaCadastro");
            AddMethod<NFE_Inutilizar>("NFE_Inutilizar");
            AddMethod<NFE_Enviar>("NFE_Enviar");
            AddMethod<NFE_ConsultarRecibo>("NFE_ConsultarRecibo");
            AddMethod<NFE_Cancelar>("NFE_Cancelar");
            AddMethod<NFE_EnviarEvento>("NFE_EnviarEvento");
            AddMethod<NFE_DistribuicaoDFePorUltNSU>("NFE_DistribuicaoDFePorUltNSU");
            AddMethod<NFE_DistribuicaoDFe>("NFE_DistribuicaoDFe");
            AddMethod<NFE_DistribuicaoDFePorNSU>("NFE_DistribuicaoDFePorNSU");
            AddMethod<NFE_DistribuicaoDFePorChave>("NFE_DistribuicaoDFePorChave");
            AddMethod<NFE_EnviarEmail>("NFE_EnviarEmail");
            AddMethod<NFE_EnviarEmailEvento>("NFE_EnviarEmailEvento");
            AddMethod<NFE_Imprimir>("NFE_Imprimir");
            AddMethod<NFE_ImprimirPDF>("NFE_ImprimirPDF");
            AddMethod<NFE_SalvarPDF>("NFE_SalvarPDF");
            AddMethod<NFE_ImprimirEvento>("NFE_ImprimirEvento");
            AddMethod<NFE_ImprimirEventoPDF>("NFE_ImprimirEventoPDF");
            AddMethod<NFE_SalvarEventoPDF>("NFE_SalvarEventoPDF");
            AddMethod<NFE_ImprimirInutilizacao>("NFE_ImprimirInutilizacao");
            AddMethod<NFE_ImprimirInutilizacaoPDF>("NFE_ImprimirInutilizacaoPDF");
            AddMethod<NFE_SalvarInutilizacaoPDF>("NFE_SalvarInutilizacaoPDF");
        }
    }
}