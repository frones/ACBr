using System;
using System.IO;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed partial class ACBrSat : ACBrLibHandle
    {
        #region Constructors

        public ACBrSat(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrSAT64.dll" : "libacbrsat64.so",
            IsWindows ? "ACBrSAT32.dll" : "libacbrsat32.so")
        {
            var inicializar = GetMethod<SAT_Inicializar>();
            var ret = ExecuteMethod<int>(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new SatBaseConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<SAT_Nome>();
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

                var method = GetMethod<SAT_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public SatBaseConfig Config { get; }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<SAT_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<SAT_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<SAT_ConfigLerValor>();

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

            var method = GetMethod<SAT_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<SAT_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConfigExportar>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void Inicializar()
        {
            var method = GetMethod<SAT_InicializarSAT>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void DesInicializar()
        {
            var method = GetMethod<SAT_DesInicializar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public AtivarSatResposta AtivarSAT(string CNPJValue, int cUF)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AtivarSAT>();
            var ret = ExecuteMethod(() => method(ToUTF8(CNPJValue), cUF, buffer, ref bufferLen));

            CheckResult(ret);

            return AtivarSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public AssociarAssinaturaResposta AssociarAssinatura(string CNPJValue, string assinaturaCNPJs)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AssociarAssinatura>();
            var ret = ExecuteMethod(() => method(ToUTF8(CNPJValue), ToUTF8(assinaturaCNPJs), buffer, ref bufferLen));

            CheckResult(ret);

            return AssociarAssinaturaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public BloquearSatResposta BloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_BloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return BloquearSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public DesbloquearSatResposta DesbloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_DesbloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return DesbloquearSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public TrocarCodigoDeAtivacaoResposta TrocarCodigoDeAtivacao(string codigoDeAtivacaoOuEmergencia, int opcao, string novoCodigo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_TrocarCodigoDeAtivacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(codigoDeAtivacaoOuEmergencia), opcao,
                ToUTF8(novoCodigo), buffer, ref bufferLen));

            CheckResult(ret);

            return TrocarCodigoDeAtivacaoResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarSatResposta ConsultarSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarStatusOperacionalResposta ConsultarStatusOperacional()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarStatusOperacional>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarStatusOperacionalResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarSessaoSatResposta ConsultarNumeroSessao(int cNumeroDeSessao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarNumeroSessao>();
            var ret = ExecuteMethod(() => method(cNumeroDeSessao, buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarSessaoSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void SetNumeroSessao(string cNumeroDeSessao)
        {
            var method = GetMethod<SAT_SetNumeroSessao>();
            var ret = ExecuteMethod(() => method(cNumeroDeSessao));

            CheckResult(ret);           
        }

        public ConsultarUltimaSessaoFiscalResposta ConsultarUltimaSessaoFiscal()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarUltimaSessaoFiscal>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarUltimaSessaoFiscalResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public AtualizarSoftwareSatResposta AtualizarSoftwareSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AtualizarSoftwareSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return AtualizarSoftwareSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ComunicarCertificadoICPBRASILResposta ComunicarCertificadoICPBRASIL(string certificado)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ComunicarCertificadoICPBRASIL>();
            var ret = ExecuteMethod(() => method(ToUTF8(certificado), buffer, ref bufferLen));

            CheckResult(ret);

            return ComunicarCertificadoICPBRASILResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void ExtrairLogs(string eArquivo)
        {
            var method = GetMethod<SAT_ExtrairLogs>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivo)));

            CheckResult(ret);
        }

        public TesteFimAFimResposta TesteFimAFim(string eArquivoXmlVenda)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_TesteFimAFim>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlVenda), buffer, ref bufferLen));

            CheckResult(ret);

            return TesteFimAFimResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string GerarAssinaturaSAT(string eCNPJSHW, string eCNPJEmitente)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarAssinaturaSAT>();
            var ret = ExecuteMethod(() => method(ToUTF8(eCNPJSHW), ToUTF8(eCNPJEmitente), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public CFeResposta CriarCFe(CupomFiscal CFe) => CriarCFe(CFe.ToString());

        public CFeResposta CriarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CriarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return CFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void CarregarXML(string eArquivo)
        {
            var method = GetMethod<SAT_CarregarXML>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivo)));

            CheckResult(ret);
        }

        public void validarCFe(string eArquivoXml)
        {
            var method = GetMethod<SAT_ValidarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public string ObterIni()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ObterIni>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public EnvioResposta CriarEnviarCFe(CupomFiscal CFe) => CriarEnviarCFe(CFe.ToString());

        public EnvioResposta CriarEnviarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CriarEnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EnvioResposta EnviarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_EnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public CancelarCFeResposta CancelarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CancelarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return CancelarCFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void ImprimirExtratoVenda(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void ImprimirExtratoResumido(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoResumido>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public PDFExtratoVendaResposta GerarPDFExtratoVenda(string eArquivoXml, string eNomeArquivo = "")
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarPDFExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeArquivo), buffer, ref bufferLen));

            CheckResult(ret);

            return PDFExtratoVendaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public PDFExtratoVendaResposta GerarPDFCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeArquivo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarPDFCancelamento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqXMLVenda), ToUTF8(eArqXMLCancelamento), ToUTF8(eNomeArquivo), buffer, ref bufferLen));

            CheckResult(ret);

            return PDFExtratoVendaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string GerarImpressaoFiscalMFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarImpressaoFiscalMFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void ImprimirExtratoCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoCancelamento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqXMLVenda), ToUTF8(eArqXMLCancelamento), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void EnviarEmail(string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
            string sMensagem, string sCC, string eAnexos)
        {
            var method = GetMethod<SAT_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(ePara), ToUTF8(eAssunto),
                ToUTF8(eNomeArquivo), ToUTF8(sMensagem), ToUTF8(sCC), ToUTF8(eAnexos)));

            CheckResult(ret);
        }

        public async void SalvarPDF(Stream aStream)
        {
            if (aStream == null) throw new ArgumentNullException(nameof(aStream));

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_SalvarPDF>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var pdf = ProcessResult(buffer, bufferLen);
            Base64ToStream(pdf, aStream);
        }

        public CupomFiscal ObterCFe() => CupomFiscal.Load(ObterIni());

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<SAT_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<SAT_UltimoRetorno>();

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