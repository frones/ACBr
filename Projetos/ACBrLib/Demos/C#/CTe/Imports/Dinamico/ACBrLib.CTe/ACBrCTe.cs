using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.CTe;
using ACBrLib.Core.DFe;

namespace ACBrLib.CTe
{
    public sealed class ACBrCTe : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
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
            public delegate int CTE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

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
            public delegate int CTE_Consultar(string eChaveOuCTe, StringBuilder buffer, ref int bufferSize);

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
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrCTe(string eArqConfig = "", string eChaveCrypt = "") :
            base("ACBrCTe64.dll", "ACBrCTe32.dll")
        {
            var inicializar = GetMethod<Delegates.CTE_Inicializar>();
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

                var method = GetMethod<Delegates.CTE_Nome>();
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

                var method = GetMethod<Delegates.CTE_Versao>();
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
            var gravarIni = GetMethod<Delegates.CTE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
        {
            var lerIni = GetMethod<Delegates.CTE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.CTE_ConfigLerValor>();

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

            var method = GetMethod<Delegates.CTE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.CTE_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.CTE_CarregarINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_ObterXml>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.CTE_GravarXml>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_ObterIni>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.CTE_GravarIni>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.CTE_CarregarEventoXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.CTE_CarregarEventoINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<Delegates.CTE_LimparLista>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<Delegates.CTE_LimparListaEventos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<Delegates.CTE_Assinar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<Delegates.CTE_Validar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_GerarChave>();
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

            var method = GetMethod<Delegates.CTE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string GetPath(TipoPathCTe tipo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_GetPath>();
            var ret = ExecuteMethod(() => method((int)tipo, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPathEvento(string evento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_GetPathEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(evento), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_StatusServico>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Consultar(string eChaveOuCTe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_Consultar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuCTe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultaCadastro(string cUF, string nDocumento, bool nIE)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_ConsultaCadastro>();
            var ret = ExecuteMethod(() => method(ToUTF8(cUF), ToUTF8(nDocumento), nIE, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_Inutilizar>();
            var ret = ExecuteMethod(() => method(ToUTF8(acnpj), ToUTF8(aJustificativa), ano, modelo, serie, numeroInicial, numeroFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Enviar(int aLote, bool imprimir = false, bool sincrono = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_Enviar>();
            var ret = ExecuteMethod(() => method(aLote, imprimir, sincrono, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_ConsultarRecibo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_Cancelar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.CTE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echCTe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eArquivoCTe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.CTE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eArquivoCTe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eArquivoEvento, string eArquivoCTe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.CTE_EnviarEmailEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eArquivoEvento), ToUTF8(eArquivoCTe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<Delegates.CTE_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<Delegates.CTE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlCTe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.CTE_ImprimirEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlCTe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlCTe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.CTE_ImprimirEventoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlCTe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacao(string eArquivoXml)
        {
            var method = GetMethod<Delegates.CTE_ImprimirInutilizacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacaoPDF(string eArquivoXml)
        {
            var method = GetMethod<Delegates.CTE_ImprimirInutilizacaoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.CTE_Inicializar>("CTE_Inicializar");
            AddMethod<Delegates.CTE_Finalizar>("CTE_Finalizar");
            AddMethod<Delegates.CTE_Nome>("CTE_Nome");
            AddMethod<Delegates.CTE_Versao>("CTE_Versao");
            AddMethod<Delegates.CTE_UltimoRetorno>("CTE_UltimoRetorno");
            AddMethod<Delegates.CTE_ConfigLer>("CTE_ConfigLer");
            AddMethod<Delegates.CTE_ConfigGravar>("CTE_ConfigGravar");
            AddMethod<Delegates.CTE_ConfigLerValor>("CTE_ConfigLerValor");
            AddMethod<Delegates.CTE_ConfigGravarValor>("CTE_ConfigGravarValor");
            AddMethod<Delegates.CTE_CarregarXML>("CTE_CarregarXML");
            AddMethod<Delegates.CTE_CarregarINI>("CTE_CarregarINI");
            AddMethod<Delegates.CTE_CarregarEventoXML>("CTE_CarregarEventoXML");
            AddMethod<Delegates.CTE_CarregarEventoINI>("CTE_CarregarEventoINI");
            AddMethod<Delegates.CTE_ObterXml>("CTE_ObterXml");
            AddMethod<Delegates.CTE_GravarXml>("CTE_GravarXml");
            AddMethod<Delegates.CTE_ObterIni>("CTE_ObterIni");
            AddMethod<Delegates.CTE_GravarIni>("CTE_GravarIni");
            AddMethod<Delegates.CTE_LimparLista>("CTE_LimparLista");
            AddMethod<Delegates.CTE_LimparListaEventos>("CTE_LimparListaEventos");
            AddMethod<Delegates.CTE_Assinar>("CTE_Assinar");
            AddMethod<Delegates.CTE_Validar>("CTE_Validar");
            AddMethod<Delegates.CTE_ValidarRegrasdeNegocios>("CTE_ValidarRegrasdeNegocios");
            AddMethod<Delegates.CTE_VerificarAssinatura>("CTE_VerificarAssinatura");
            AddMethod<Delegates.CTE_GerarChave>("CTE_GerarChave");
            AddMethod<Delegates.CTE_ObterCertificados>("CTE_ObterCertificados");
            AddMethod<Delegates.CTE_GetPath>("CTE_GetPath");
            AddMethod<Delegates.CTE_GetPathEvento>("CTE_GetPathEvento");
            AddMethod<Delegates.CTE_StatusServico>("CTE_StatusServico");
            AddMethod<Delegates.CTE_Consultar>("CTE_Consultar");
            AddMethod<Delegates.CTE_ConsultaCadastro>("CTE_ConsultaCadastro");
            AddMethod<Delegates.CTE_Inutilizar>("CTE_Inutilizar");
            AddMethod<Delegates.CTE_Enviar>("CTE_Enviar");
            AddMethod<Delegates.CTE_ConsultarRecibo>("CTE_ConsultarRecibo");
            AddMethod<Delegates.CTE_Cancelar>("CTE_Cancelar");
            AddMethod<Delegates.CTE_EnviarEvento>("CTE_EnviarEvento");
            AddMethod<Delegates.CTE_DistribuicaoDFePorUltNSU>("CTE_DistribuicaoDFePorUltNSU");
            AddMethod<Delegates.CTE_DistribuicaoDFePorNSU>("CTE_DistribuicaoDFePorNSU");
            AddMethod<Delegates.CTE_DistribuicaoDFePorChave>("CTE_DistribuicaoDFePorChave");
            AddMethod<Delegates.CTE_EnviarEmail>("CTE_EnviarEmail");
            AddMethod<Delegates.CTE_EnviarEmailEvento>("CTE_EnviarEmailEvento");
            AddMethod<Delegates.CTE_Imprimir>("CTE_Imprimir");
            AddMethod<Delegates.CTE_ImprimirPDF>("CTE_ImprimirPDF");
            AddMethod<Delegates.CTE_ImprimirEvento>("CTE_ImprimirEvento");
            AddMethod<Delegates.CTE_ImprimirEventoPDF>("CTE_ImprimirEventoPDF");
            AddMethod<Delegates.CTE_ImprimirInutilizacao>("CTE_ImprimirInutilizacao");
            AddMethod<Delegates.CTE_ImprimirInutilizacaoPDF>("CTE_ImprimirInutilizacaoPDF");
        }

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.CTE_Finalizar>();
            var ret = ExecuteMethod(() => finalizar());
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.CTE_UltimoRetorno>();

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