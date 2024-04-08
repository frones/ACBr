using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Extensions;
using ACBrLib.Core.MDFe;

namespace ACBrLib.MDFe
{
    public sealed partial class ACBrMDFe : ACBrLibHandle
    {
        #region Constructors

        public ACBrMDFe(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrMDFe64.dll" : "libacbrmdfe64.so",
                                                                                IsWindows ? "ACBrMDFe32.dll" : "libacbrmdfe32.so")
        {
            var inicializar = GetMethod<MDFE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new MDFeConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<MDFE_Nome>();
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

                var method = GetMethod<MDFE_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public MDFeConfig Config { get; }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<MDFE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<MDFE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<MDFE_ConfigLerValor>();

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

            var method = GetMethod<MDFE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<MDFE_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ConfigExportar>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void CarregarManifesto(Manifesto manifesto) => CarregarINI(manifesto.ToString());

        public Manifesto ObterManifesto(int aIndex) => Manifesto.Load(ObterIni(aIndex));

        public void CarregarEvento(EventoMDFeBase evento) => CarregarEventoINI(evento.ToString());

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<MDFE_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<MDFE_CarregarINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ObterXml>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<MDFE_GravarXml>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public string ObterIni(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ObterIni>();
            var ret = ExecuteMethod(() => method(aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarIni(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<MDFE_GravarIni>();
            var ret = ExecuteMethod(() => method(aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarEventoXML(string eArquivoOuXml)
        {
            var method = GetMethod<MDFE_CarregarEventoXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarEventoINI(string eArquivoOuIni)
        {
            var method = GetMethod<MDFE_CarregarEventoINI>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<MDFE_LimparLista>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void LimparListaEventos()
        {
            var method = GetMethod<MDFE_LimparListaEventos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<MDFE_Assinar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<MDFE_Validar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string ValidarRegrasdeNegocios()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ValidarRegrasdeNegocios>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarChave(int aCodigoUf, int aCodigoNumerico, int aModelo, int aSerie, int aNumero,
            int aTpEmi, DateTime aEmissao, string acpfcnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_GerarChave>();
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

            var method = GetMethod<MDFE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string OpenSSLInfo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_OpenSSLInfo>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPath(TipoPathMDFe tipo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_GetPath>();
            var ret = ExecuteMethod(() => method((int)tipo, buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public string GetPathEvento(TipoEventoMDFe evento)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_GetPathEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(evento.GetEnumValueOrInt()), buffer, ref bufferLen));

            return ProcessResult(buffer, bufferLen);
        }

        public StatusServicoResposta StatusServico()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_StatusServico>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return StatusServicoResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultaMDFeResposta Consultar(string eChaveOuNFe, bool AExtrairEventos = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_Consultar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuNFe), AExtrairEventos, buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultaMDFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EnvioRetornoResposta Enviar(int aLote, bool imprimir = false, bool sincrono = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_Enviar>();
            var ret = ExecuteMethod(() => method(aLote, imprimir, sincrono, buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioRetornoResposta.LerResposta(ProcessResult(buffer, bufferLen), "MDFe");
        }

        public RetornoResposta ConsultarRecibo(string aRecibo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ConsultarRecibo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aRecibo), buffer, ref bufferLen));

            CheckResult(ret);

            return RetornoResposta.LerResposta(ProcessResult(buffer, bufferLen), "MDFe");
        }

        public CancelamentoMDFeResposta Cancelar(string eChave, string eJustificativa, string eCNPJ, int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_Cancelar>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChave), ToUTF8(eJustificativa), ToUTF8(eCNPJ), aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return CancelamentoMDFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EventoResposta EnviarEvento(int aLote)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_EnviarEvento>();
            var ret = ExecuteMethod(() => method(aLote, buffer, ref bufferLen));

            CheckResult(ret);

            return EventoResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EncerramentoResposta EncerrarMDFe(string eChaveOuMDFe, DateTime eDtEnc, string cMunicipioDescarga, string nCNPJ = "", string nProtocolo = "")
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_EncerrarMDFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eChaveOuMDFe), ToUTF8(eDtEnc.ToString("dd/MM/yyyy")), ToUTF8(cMunicipioDescarga),
                                                 ToUTF8(nCNPJ), ToUTF8(nProtocolo), buffer, ref bufferLen));

            CheckResult(ret);

            return EncerramentoResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public NaoEncerradosResposta ConsultaMDFeNaoEnc(string cnpj)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_ConsultaMDFeNaoEnc>();
            var ret = ExecuteMethod(() => method(ToUTF8(cnpj), buffer, ref bufferLen));

            CheckResult(ret);

            return NaoEncerradosResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public DistribuicaoDFeResposta<TipoEventoMDFe> DistribuicaoDFePorUltNSU(int acUFAutor, string eCnpjcpf, string eultNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_DistribuicaoDFePorUltNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eultNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return DistribuicaoDFeResposta<TipoEventoMDFe>.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public DistribuicaoDFeResposta<TipoEventoMDFe> DistribuicaoDFePorNSU(int acUFAutor, string eCnpjcpf, string eNsu)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_DistribuicaoDFePorNSU>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(eNsu), buffer, ref bufferLen));

            CheckResult(ret);

            return DistribuicaoDFeResposta<TipoEventoMDFe>.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public DistribuicaoDFeResposta<TipoEventoMDFe> DistribuicaoDFePorChave(int acUFAutor, string eCnpjcpf, string echNFe)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MDFE_DistribuicaoDFePorChave>();
            var ret = ExecuteMethod(() => method(acUFAutor, ToUTF8(eCnpjcpf), ToUTF8(echNFe), buffer, ref bufferLen));

            CheckResult(ret);

            return DistribuicaoDFeResposta<TipoEventoMDFe>.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<MDFE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                                                 ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void EnviarEmailEvento(string ePara, string eChaveEvento, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<MDFE_EnviarEmailEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePara), ToUTF8(eChaveEvento), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string cImpressora = "", int nNumCopias = 1, string cProtocolo = "", bool? bMostrarPreview = null)
        {
            var mostrarPreview = bMostrarPreview.HasValue ? $"{Convert.ToInt32(bMostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<MDFE_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(cImpressora), nNumCopias, ToUTF8(cProtocolo), ToUTF8(mostrarPreview)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<MDFE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void ImprimirEvento(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<MDFE_ImprimirEvento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        public void ImprimirEventoPDF(string eArquivoXmlNFe, string eArquivoXmlEvento)
        {
            var method = GetMethod<MDFE_ImprimirEventoPDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlNFe), ToUTF8(eArquivoXmlEvento)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<MDFE_Finalizar>();
            var ret = ExecuteMethod(() => finalizar());
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<MDFE_UltimoRetorno>();

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