using System;
using System.Linq;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.CTe;
using ACBrLib.Core.DFe;

namespace ACBrLib.CTe
{
    public sealed partial class ACBrCTe : ACBrLibHandle
    {
        #region Constructors

        public ACBrCTe(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrCTe64.dll" : "libacbrcte64.so",
                                                                               IsWindows ? "ACBrCTe32.dll" : "libacbrcte32.so")
        {
            var inicializar = GetMethod<CTE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new CTeConfig(this);
        }

        public CTeConfig Config { get; }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<CTE_Nome>();
                var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

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

                var method = GetMethod<CTE_Versao>();
                var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<CTE_ConfigGravar>();
            var ret = ExecuteMethod<int>(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<CTE_ConfigLer>();
            var ret = ExecuteMethod<int>(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<CTE_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public override void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<CTE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod<int>(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<CTE_ConfigImportar>();
            var ret = ExecuteMethod<int>(() => importarConfig(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ConfigExportar>();
            var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void CarregarCTe(CTe cte) => CarregarINI(cte.ToString());

        public CTe ObterCTe(int aIndex) => CTe.Load(ObterIni(aIndex));
        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<CTE_CarregarXML>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<CTE_CarregarINI>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ObterXml>();
            var ret = ExecuteMethod<int>(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<CTE_GravarXml>();
            var ret = ExecuteMethod<int>(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ObterIni>();
            var ret = ExecuteMethod<int>(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<CTE_GravarIni>();
            var ret = ExecuteMethod<int>(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<CTE_CarregarEventoXML>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<CTE_CarregarEventoINI>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<CTE_LimparLista>();
            var ret = ExecuteMethod<int>(() => method());

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<CTE_LimparListaEventos>();
            var ret = ExecuteMethod<int>(() => method());

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<CTE_Assinar>();
            var ret = ExecuteMethod<int>(() => method());

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<CTE_Validar>();
            var ret = ExecuteMethod<int>(() => method());

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_VerificarAssinatura>();
            var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_GerarChave>();
            var ret = ExecuteMethod<int>(() => method(aCodigoUf, aCodigoNumerico, aModelo, aSerie, aNumero,
                aTpEmi, aEmissao.Date.ToString("dd/MM/yyyy"), ToUTF8(acpfcnpj),
                buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ObterCertificados>();
            var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string OpenSSLInfo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_OpenSSLInfo>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPath(TipoPathCTe tipo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_GetPath>();
            var ret = ExecuteMethod<int>(() => method((int)tipo, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPathEvento(string evento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_GetPathEvento>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(evento), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_StatusServico>();
            var ret = ExecuteMethod<int>(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public ConsultaCTeResposta Consultar(string eChaveOuCTe, bool AExtrairEventos = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_Consultar>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eChaveOuCTe), AExtrairEventos, buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultaCTeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string ConsultaCadastro(string cUF, string nDocumento, bool nIE)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ConsultaCadastro>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(cUF), ToUTF8(nDocumento), nIE, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InutilizacaoCTeResposta Inutilizar(string acnpj, string aJustificativa, int ano, int modelo,
            int serie, int numeroInicial, int numeroFinal)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_Inutilizar>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(acnpj), ToUTF8(aJustificativa), ano, modelo, serie, numeroInicial, numeroFinal, buffer, ref bufferLen));

            CheckResult(ret);

            return InutilizacaoCTeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EnvioRetornoResposta Enviar(int aLote, bool imprimir = false, bool sincrono = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_Enviar>();
            var ret = ExecuteMethod<int>(() => method(aLote, imprimir, sincrono, buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioRetornoResposta.LerResposta(ProcessResult(buffer, bufferLen), "CTe");
        }

        public string ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_ConsultarRecibo>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public CancelamentoCTeResposta Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_Cancelar>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return CancelamentoCTeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_EnviarEvento>();
            var ret = ExecuteMethod<int>(() => method(aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod<int>(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFe(int acUFAutor, string eCnpjcpf, string eultNsu, string ArquivoOuXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_DistribuicaoDFe>();
            var ret = ExecuteMethod<int>(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), ToUTF8(ArquivoOuXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod<int>(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echCTe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<CTE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod<int>(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echCTe), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eArquivoCTe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<CTE_EnviarEmail>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(ePara), ToUTF8(eArquivoCTe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eArquivoEvento, string eArquivoCTe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<CTE_EnviarEmailEvento>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(ePara), ToUTF8(eArquivoEvento), ToUTF8(eArquivoCTe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<CTE_Imprimir>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<CTE_ImprimirPDF>();
            var ret = ExecuteMethod<int>(() => method());

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlCTe, string eArquivoXmlEvento)
        {
            var method = GetMethod<CTE_ImprimirEvento>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoXmlCTe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlCTe, string eArquivoXmlEvento)
        {
            var method = GetMethod<CTE_ImprimirEventoPDF>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoXmlCTe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacao(string eArquivoXml)
        {
            var method = GetMethod<CTE_ImprimirInutilizacao>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public void ImprimirInutilizacaoPDF(string eArquivoXml)
        {
            var method = GetMethod<CTE_ImprimirInutilizacaoPDF>();
            var ret = ExecuteMethod<int>(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<CTE_Finalizar>();
            var ret = ExecuteMethod<int>(() => finalizar());
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<CTE_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod<int>(() => ultimoRetorno(buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod<int>(() => ultimoRetorno(buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}