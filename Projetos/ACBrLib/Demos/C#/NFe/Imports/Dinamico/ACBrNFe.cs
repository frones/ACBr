using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.NFe
{
    public sealed class ACBrNFe : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
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
            public delegate int NFE_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

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
            public delegate int NFE_StatusServico(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int NFE_Consultar(string eChaveOuNFe, StringBuilder buffer, ref int bufferSize);

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
            public delegate int NFE_ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int NFE_ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int NFE_ImprimirInutilizacao(string eArquivoXml);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int NFE_ImprimirInutilizacaoPDF(string eArquivoXml);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrNFe(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrNFe64.dll" : "ACBrNFe32.dll")
        {
            var inicializar = GetMethod<Delegates.NFE_Inicializar>();
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

                var method = GetMethod<Delegates.NFE_Nome>();
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

                var method = GetMethod<Delegates.NFE_Versao>();
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
            var gravarIni = GetMethod<Delegates.NFE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
        {
            var lerIni = GetMethod<Delegates.NFE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.NFE_ConfigLerValor>();

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

            var method = GetMethod<Delegates.NFE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.NFE_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.NFE_CarregarINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_ObterXml>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.NFE_GravarXml>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.NFE_CarregarEventoXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.NFE_CarregarEventoINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<Delegates.NFE_LimparLista>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<Delegates.NFE_LimparListaEventos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<Delegates.NFE_Assinar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<Delegates.NFE_Validar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_GerarChave>();
            var ret = ExecuteMethod(() => method(aCodigoUf, aCodigoNumerico, aModelo, aSerie, aNumero,
                                                 aTpEmi, aEmissao.Date.ToString("dd/MM/yyyy"), ToUTF8(acpfcnpj),
                                                 buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_StatusServico>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Consultar(string eChaveOuNFe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_Consultar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuNFe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_Inutilizar>();
            var ret = ExecuteMethod(() => method(ToUTF8(acnpj), ToUTF8(aJustificativa), ano, modelo, serie, numeroInicial, numeroFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Enviar(int aLote, bool imprimir = false, bool sincrono = false, bool zipado = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_Enviar>();
            var ret = ExecuteMethod(() => method(aLote, imprimir, sincrono, zipado, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_ConsultarRecibo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_Cancelar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.NFE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echNFe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.NFE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.NFE_EnviarEmailEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveEvento), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null, bool? cMarcaDagua = null,
            bool? bViaConsumidor = null, bool? bSimplificado = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;
            var marcaDagua = cMarcaDagua.HasValue ? $"{Convert.ToInt32(cMarcaDagua.Value)}" : string.Empty;
            var viaConsumidor = bViaConsumidor.HasValue ? $"{Convert.ToInt32(bViaConsumidor.Value)}" : string.Empty;
            var simplificado = bSimplificado.HasValue ? $"{Convert.ToInt32(bSimplificado.Value)}" : string.Empty;

            var method = GetMethod<Delegates.NFE_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview),
                ToUTF8(marcaDagua), ToUTF8(viaConsumidor), ToUTF8(simplificado)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<Delegates.NFE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.NFE_ImprimirEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<Delegates.NFE_ImprimirEventoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacao(string eArquivoXml)
        {
            var method = GetMethod<Delegates.NFE_ImprimirInutilizacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacaoPDF(string eArquivoXml)
        {
            var method = GetMethod<Delegates.NFE_ImprimirInutilizacaoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.NFE_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.NFE_Inicializar>("NFE_Inicializar");
            AddMethod<Delegates.NFE_Finalizar>("NFE_Finalizar");
            AddMethod<Delegates.NFE_Nome>("NFE_Nome");
            AddMethod<Delegates.NFE_Versao>("NFE_Versao");
            AddMethod<Delegates.NFE_UltimoRetorno>("NFE_UltimoRetorno");
            AddMethod<Delegates.NFE_ConfigLer>("NFE_ConfigLer");
            AddMethod<Delegates.NFE_ConfigGravar>("NFE_ConfigGravar");
            AddMethod<Delegates.NFE_ConfigLerValor>("NFE_ConfigLerValor");
            AddMethod<Delegates.NFE_ConfigGravarValor>("NFE_ConfigGravarValor");
            AddMethod<Delegates.NFE_CarregarXML>("NFE_CarregarXML");
            AddMethod<Delegates.NFE_CarregarINI>("NFE_CarregarINI");
            AddMethod<Delegates.NFE_ObterXml>("NFE_ObterXml");
            AddMethod<Delegates.NFE_GravarXml>("NFE_GravarXml");
            AddMethod<Delegates.NFE_CarregarEventoXML>("NFE_CarregarEventoXML");
            AddMethod<Delegates.NFE_CarregarEventoINI>("NFE_CarregarEventoINI");
            AddMethod<Delegates.NFE_LimparLista>("NFE_LimparLista");
            AddMethod<Delegates.NFE_LimparListaEventos>("NFE_LimparListaEventos");
            AddMethod<Delegates.NFE_Assinar>("NFE_Assinar");
            AddMethod<Delegates.NFE_Validar>("NFE_Validar");
            AddMethod<Delegates.NFE_ValidarRegrasdeNegocios>("NFE_ValidarRegrasdeNegocios");
            AddMethod<Delegates.NFE_VerificarAssinatura>("NFE_VerificarAssinatura");
            AddMethod<Delegates.NFE_GerarChave>("NFE_GerarChave");
            AddMethod<Delegates.NFE_StatusServico>("NFE_StatusServico");
            AddMethod<Delegates.NFE_Consultar>("NFE_Consultar");
            AddMethod<Delegates.NFE_Inutilizar>("NFE_Inutilizar");
            AddMethod<Delegates.NFE_Enviar>("NFE_Enviar");
            AddMethod<Delegates.NFE_ConsultarRecibo>("NFE_ConsultarRecibo");
            AddMethod<Delegates.NFE_Cancelar>("NFE_Cancelar");
            AddMethod<Delegates.NFE_EnviarEvento>("NFE_EnviarEvento");
            AddMethod<Delegates.NFE_DistribuicaoDFePorUltNSU>("NFE_DistribuicaoDFePorUltNSU");
            AddMethod<Delegates.NFE_DistribuicaoDFePorNSU>("NFE_DistribuicaoDFePorNSU");
            AddMethod<Delegates.NFE_DistribuicaoDFePorChave>("NFE_DistribuicaoDFePorChave");
            AddMethod<Delegates.NFE_EnviarEmail>("NFE_EnviarEmail");
            AddMethod<Delegates.NFE_EnviarEmailEvento>("NFE_EnviarEmailEvento");
            AddMethod<Delegates.NFE_Imprimir>("NFE_Imprimir");
            AddMethod<Delegates.NFE_ImprimirPDF>("NFE_ImprimirPDF");
            AddMethod<Delegates.NFE_ImprimirEvento>("NFE_ImprimirEvento");
            AddMethod<Delegates.NFE_ImprimirEventoPDF>("NFE_ImprimirEventoPDF");
            AddMethod<Delegates.NFE_ImprimirInutilizacao>("NFE_ImprimirInutilizacao");
            AddMethod<Delegates.NFE_ImprimirInutilizacaoPDF>("NFE_ImprimirInutilizacaoPDF");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.NFE_UltimoRetorno>();

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