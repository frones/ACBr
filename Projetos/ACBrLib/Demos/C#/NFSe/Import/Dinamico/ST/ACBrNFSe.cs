using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.NFSe;

namespace ACBrLib.NFSe
{
    /// <inheritdoc />
    public sealed partial class ACBrNFSe : ACBrLibHandle
    {
        #region Constructors

        public ACBrNFSe(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrNFSe64.dll" : "libacbrnfse64.so",
                                                                                      IsWindows ? "ACBrNFSe32.dll" : "libacbrnfse32.so")
        {
            var inicializar = GetMethod<NFSE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new ACBrNFSeConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<NFSE_Nome>();
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

                var method = GetMethod<NFSE_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public ACBrNFSeConfig Config { get; }

        #endregion Properties

        #region Metodos

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<NFSE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<NFSE_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConfigExportar>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<NFSE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<NFSE_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public override void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<NFSE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Diversos

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<NFSE_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<NFSE_CarregarINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ObterXml>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<NFSE_GravarXml>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ObterIni>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<NFSE_GravarIni>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<NFSE_LimparLista>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string Emitir(string aLote, int aModoEnvio, bool aImprimir)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_Emitir>();
            var ret = ExecuteMethod(() => method(ToUTF8(aLote), aModoEnvio, aImprimir, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Cancelar(string aInfCancelamento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_Cancelar>();
            var ret = ExecuteMethod(() => method(ToUTF8(aInfCancelamento), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string SubstituirNFSe(string aNumeroNFSe, string aSerieNFSe, string aCodigoCancelamento, string aMotivoCancelamento, string aNumeroLote, string aCodigoVerificacao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_SubstituirNFSe>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumeroNFSe), ToUTF8(aSerieNFSe), ToUTF8(aCodigoCancelamento), ToUTF8(aMotivoCancelamento), ToUTF8(aNumeroLote), ToUTF8(aCodigoVerificacao), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string LinkNFSe(string aNumeroNFSe, string aCodigoVerificacao, string aChaveAcesso, string aValorServico)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_LinkNFSe>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumeroNFSe), ToUTF8(aCodigoVerificacao), ToUTF8(aChaveAcesso), ToUTF8(aValorServico), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarLote(string aLote, int aQtdMaximaRps, int aModoEnvio)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_GerarLote>();
            var ret = ExecuteMethod(() => method(ToUTF8(aLote), aQtdMaximaRps, aModoEnvio, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GerarToken()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_GerarToken>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);
        }

        public string ConsultarSituacao(string aProtocolo, string aNumeroLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarSituacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(aProtocolo), ToUTF8(aNumeroLote), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarLoteRps(string aProcotolo, string aNumLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarLoteRps>();
            var ret = ExecuteMethod(() => method(ToUTF8(aProcotolo), ToUTF8(aNumLote), buffer, ref bufferLen));

            CheckResult(ret);
            
            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSePorRps(string aNumeroRps, string aSerie, string aTipo, string aCodigoVerificacao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSePorRps>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumeroRps), ToUTF8(aSerie), ToUTF8(aTipo), ToUTF8(aCodigoVerificacao), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSePorNumero(string aNumero, int aPagina)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSePorNumero>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumero), aPagina, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSePorPeriodo(DateTime aDataInicial, DateTime aDataFinal, int aPagina, string aNumeroLote, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSePorPeriodo>();
            var ret = ExecuteMethod(() => method(aDataInicial, aDataFinal, aPagina, ToUTF8(aNumeroLote), aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSePorFaixa(string aNumeroInicial, string aNumeroFinal, int aPagina)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSePorFaixa>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumeroInicial), ToUTF8(aNumeroFinal), aPagina, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeGenerico(string aInfConsultaNFSe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeGenerico>();
            var ret = ExecuteMethod(() => method(ToUTF8(aInfConsultaNFSe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eXmlNFSe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem)
        {
            var method = GetMethod<NFSE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eXmlNFSe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc), ToUTF8(eAnexos), ToUTF8(eMensagem)));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, bool? bGerarPDF = null, bool? bMostrarPreview = null, string cCancelada = "")
        {
            var gerarPDF = bGerarPDF.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<NFSE_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(cImpressora), nNumCopias, gerarPDF, mostrarPreview, ToUTF8(cCancelada)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<NFSE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string ConsultarNFSeServicoPrestadoPorNumero(string aNumero, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoPrestadoPorNumero>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoPrestadoPorPeriodo(DateTime aDataInicial, DateTime aDataFinal, int aPagina, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoPrestadoPorPeriodo>();
            var ret = ExecuteMethod(() => method(aDataFinal, aDataFinal, aPagina, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoPrestadoPorTomador(string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoPrestadoPorTomador>();
            var ret = ExecuteMethod(() => method(ToUTF8(aCNPJ), ToUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }
        
        public string ConsultarNFSeServicoPrestadoPorIntermediario(string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoPrestadoPorIntermediario>();
            var ret = ExecuteMethod(() => method(ToUTF8(aCNPJ), ToUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoTomadoPorNumero(string aNumero, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoTomadoPorNumero>();
            var ret = ExecuteMethod(() => method(ToUTF8(aNumero), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoTomadoPorPrestador(string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoTomadoPorPrestador>();
            var ret = ExecuteMethod(() => method(ToUTF8(aCNPJ), ToUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoTomadoPorTomador(string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoTomadoPorTomador>();
            var ret = ExecuteMethod(() => method(ToUTF8(aCNPJ), ToUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoTomadoPorPeriodo(DateTime aDataInicial, DateTime aDataFinal, int aPagina, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoTomadoPorPeriodo>();
            var ret = ExecuteMethod(() => method(aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSeServicoTomadoPorIntermediario(string aCNPJ, string aInscMun, int aPagina, DateTime aDataInicial, DateTime aDataFinal, int aTipoPeriodo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSeServicoTomadoPorIntermediario>();
            var ret = ExecuteMethod(() => method(ToUTF8(aCNPJ), ToUTF8(aInscMun), aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

		public string EnviarEvento(string aInfEvento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(aInfEvento), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarDPSPorChave(string aChaveDPS)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarDPSPorChave>();
            var ret = ExecuteMethod(() => method(ToUTF8(aChaveDPS), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNFSePorChave(string aChaveNFSe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarNFSePorChave>();
            var ret = ExecuteMethod(() => method(ToUTF8(aChaveNFSe), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarEvento(string aChave, int aTipoEvento, int aNumSeq)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(aChave), aTipoEvento, aNumSeq, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarDFe(int aNSU)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarDFe>();
            var ret = ExecuteMethod(() => method(aNSU, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ObterDANFSE(string aChaveNFSe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ObterDANFSE>();
            var ret = ExecuteMethod(() => method(ToUTF8(aChaveNFSe), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarParametros(int aTipoParametroMunicipio, string aCodigoServico, DateTime aCompetencia, string aNumeroBeneficio)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ConsultarParametros>();
            var ret = ExecuteMethod(() => method(aTipoParametroMunicipio, ToUTF8(aCodigoServico), aCompetencia, ToUTF8(aNumeroBeneficio), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string ObterInformacoesProvedor()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<NFSE_ObterInformacoesProvedor>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }
        #endregion Diversos

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<NFSE_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<NFSE_UltimoRetorno>();

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

        #endregion Metodos
    }
}