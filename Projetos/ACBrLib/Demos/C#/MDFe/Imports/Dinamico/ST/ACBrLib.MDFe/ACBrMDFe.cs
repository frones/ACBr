using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.MDFe;

namespace ACBrLib.MDFe
{
    public sealed class ACBrMDFe : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Inicializar(string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Finalizar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Nome(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Versao(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigLer(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigGravar(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConfigGravarValor(string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarXML(string eArquivoOuXml);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarINI(string eArquivoOuIni);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ObterXml(int AIndex, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_GravarXml(int AIndex, string eNomeArquivo, string ePathArquivo);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ObterIni(int AIndex, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_GravarIni(int AIndex, string eNomeArquivo, string ePathArquivo);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarEventoXML(string eArquivoOuXml);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_CarregarEventoINI(string eArquivoOuIni);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_LimparLista();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_LimparListaEventos();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Assinar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Validar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ValidarRegrasdeNegocios(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_VerificarAssinatura(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_GerarChave(int ACodigoUF, int ACodigoNumerico, int AModelo, int ASerie, int ANumero,
                int ATpEmi, string AEmissao, string CPFCNPJ, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ObterCertificados(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_GetPath(int tipo, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_GetPathEvento(string aCodEvento, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_StatusServico(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Consultar(string eChaveOuCTe, bool aExtrairEventos, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Enviar(int aLote, bool imprimir, bool sincrono, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConsultarRecibo(string aRecibo, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote,
                StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEvento(int alote, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_EncerrarMDFe(string eChaveOuMDFe, string eDtEnc, string cMunicipioDescarga, string nCNPJ, string nProtocolo, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ConsultaMDFeNaoEnc(string nCNPJ, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEmail(string ePara, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveCTe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_Imprimir(string cImpressora, int nNumCopias, string cProtocolo, string bMostrarPreview);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirPDF();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MDFE_ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrMDFe(string eArqConfig = "", string eChaveCrypt = "") :
            base("ACBrMDFe64.dll", "ACBrMDFe32.dll")
        {
            var inicializar = GetMethod<Delegates.MDFE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Delegates.MDFE_Nome>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public string Versao
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Delegates.MDFE_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "ACBrLib.ini")
        {
            var gravarIni = GetMethod<Delegates.MDFE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
        {
            var lerIni = GetMethod<Delegates.MDFE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.MDFE_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.MDFE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.MDFE_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.MDFE_CarregarINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ObterXml>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.MDFE_GravarXml>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ObterIni>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.MDFE_GravarIni>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.MDFE_CarregarEventoXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.MDFE_CarregarEventoINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<Delegates.MDFE_LimparLista>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<Delegates.MDFE_LimparListaEventos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<Delegates.MDFE_Assinar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<Delegates.MDFE_Validar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_GerarChave>();
            var ret = ExecuteMethod(() => method(aCodigoUf, aCodigoNumerico, aModelo, aSerie, aNumero,
                aTpEmi, aEmissao.Date.ToString("dd/MM/yyyy"), ToUTF8(acpfcnpj),
                buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string GetPath(TipoPathMDFe tipo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_GetPath>();
            var ret = ExecuteMethod(() => method((int)tipo, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPathEvento(string evento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_GetPathEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(evento), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_StatusServico>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Consultar(string eChaveOuNFe, bool AExtrairEventos = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_Consultar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuNFe), AExtrairEventos, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Enviar(int aLote, bool imprimir = false, bool sincrono = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_Enviar>();
            var ret = ExecuteMethod(() => method(aLote, imprimir, sincrono, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ConsultarRecibo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_Cancelar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EncerrarMDFe(string eChaveOuMDFe, DateTime eDtEnc, string cMunicipioDescarga, string nCNPJ = "", string nProtocolo = "")
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_EncerrarMDFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuMDFe), ToUTF8(eDtEnc.ToString("dd/MM/yyyy")), ToUTF8(cMunicipioDescarga),
                                                 ToUTF8(nCNPJ), ToUTF8(nProtocolo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultaMDFeNaoEnc(string cnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_ConsultaMDFeNaoEnc>();
            var ret = ExecuteMethod(() => method(ToUTF8(cnpj), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.MDFE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echNFe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.MDFE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.MDFE_EnviarEmailEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveEvento), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<Delegates.MDFE_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<Delegates.MDFE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.MDFE_ImprimirEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.MDFE_ImprimirEventoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.MDFE_Inicializar>("MDFE_Inicializar");
            AddMethod<Delegates.MDFE_Finalizar>("MDFE_Finalizar");
            AddMethod<Delegates.MDFE_Nome>("MDFE_Nome");
            AddMethod<Delegates.MDFE_Versao>("MDFE_Versao");
            AddMethod<Delegates.MDFE_UltimoRetorno>("MDFE_UltimoRetorno");
            AddMethod<Delegates.MDFE_ConfigLer>("MDFE_ConfigLer");
            AddMethod<Delegates.MDFE_ConfigGravar>("MDFE_ConfigGravar");
            AddMethod<Delegates.MDFE_ConfigLerValor>("MDFE_ConfigLerValor");
            AddMethod<Delegates.MDFE_ConfigGravarValor>("MDFE_ConfigGravarValor");
            AddMethod<Delegates.MDFE_CarregarXML>("MDFE_CarregarXML");
            AddMethod<Delegates.MDFE_CarregarINI>("MDFE_CarregarINI");
            AddMethod<Delegates.MDFE_ObterXml>("MDFE_ObterXml");
            AddMethod<Delegates.MDFE_GravarXml>("MDFE_GravarXml");
            AddMethod<Delegates.MDFE_ObterIni>("MDFE_ObterIni");
            AddMethod<Delegates.MDFE_GravarIni>("MDFE_GravarIni");
            AddMethod<Delegates.MDFE_CarregarEventoXML>("MDFE_CarregarEventoXML");
            AddMethod<Delegates.MDFE_CarregarEventoINI>("MDFE_CarregarEventoINI");
            AddMethod<Delegates.MDFE_LimparLista>("MDFE_LimparLista");
            AddMethod<Delegates.MDFE_LimparListaEventos>("MDFE_LimparListaEventos");
            AddMethod<Delegates.MDFE_Assinar>("MDFE_Assinar");
            AddMethod<Delegates.MDFE_Validar>("MDFE_Validar");
            AddMethod<Delegates.MDFE_ValidarRegrasdeNegocios>("MDFE_ValidarRegrasdeNegocios");
            AddMethod<Delegates.MDFE_VerificarAssinatura>("MDFE_VerificarAssinatura");
            AddMethod<Delegates.MDFE_GerarChave>("MDFE_GerarChave");
            AddMethod<Delegates.MDFE_ObterCertificados>("MDFE_ObterCertificados");
            AddMethod<Delegates.MDFE_GetPath>("MDFE_GetPath");
            AddMethod<Delegates.MDFE_GetPathEvento>("MDFE_GetPathEvento");
            AddMethod<Delegates.MDFE_StatusServico>("MDFE_StatusServico");
            AddMethod<Delegates.MDFE_Consultar>("MDFE_Consultar");
            AddMethod<Delegates.MDFE_Enviar>("MDFE_Enviar");
            AddMethod<Delegates.MDFE_ConsultarRecibo>("MDFE_ConsultarRecibo");
            AddMethod<Delegates.MDFE_Cancelar>("MDFE_Cancelar");
            AddMethod<Delegates.MDFE_EnviarEvento>("MDFE_EnviarEvento");
            AddMethod<Delegates.MDFE_EncerrarMDFe>("MDFE_EncerrarMDFe");
            AddMethod<Delegates.MDFE_ConsultaMDFeNaoEnc>("MDFE_ConsultaMDFeNaoEnc");
            AddMethod<Delegates.MDFE_DistribuicaoDFePorUltNSU>("MDFE_DistribuicaoDFePorUltNSU");
            AddMethod<Delegates.MDFE_DistribuicaoDFePorNSU>("MDFE_DistribuicaoDFePorNSU");
            AddMethod<Delegates.MDFE_DistribuicaoDFePorChave>("MDFE_DistribuicaoDFePorChave");
            AddMethod<Delegates.MDFE_EnviarEmail>("MDFE_EnviarEmail");
            AddMethod<Delegates.MDFE_EnviarEmailEvento>("MDFE_EnviarEmailEvento");
            AddMethod<Delegates.MDFE_Imprimir>("MDFE_Imprimir");
            AddMethod<Delegates.MDFE_ImprimirPDF>("MDFE_ImprimirPDF");
            AddMethod<Delegates.MDFE_ImprimirEvento>("MDFE_ImprimirEvento");
            AddMethod<Delegates.MDFE_ImprimirEventoPDF>("MDFE_ImprimirEventoPDF");
        }

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.MDFE_Finalizar>();
            var ret = ExecuteMethod(() => finalizar());
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.MDFE_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}